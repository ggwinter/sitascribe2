#' fn20_actualise_date_titre
#'
#' @param x date au format caractere anneemois
#'
#' @return nothing
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom lubridate month
#' @importFrom rvest html_element
#' @importFrom stringr str_detect
#' @importFrom stringr str_sub
#' @importFrom utf8 as_utf8
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_child
#' @importFrom xml2 xml_set_attr
#' @export
fn20_actualise_date_titre <- function(x = "202202") {
  ls_modele$t_objets_numero %>%
    dplyr::filter(stringr::str_detect(nom_objet, "titre$")) %>%
    dplyr::pull(id_objet) -> id_titre


  ls_modele$pg %>%
    rvest::html_element(paste0("PAGEOBJECT[ItemID=\'", id_titre, "\']")) %>%
    xml2::xml_child("StoryText")  %>%
    xml2::xml_child("ITEXT") %>%
    xml2::xml_attr("CH")

  paste(
    stringr::str_sub(params$annee_mois, 1, 4),
    stringr::str_sub(params$annee_mois, 5, 6),
    "01",
    sep = "/"
  ) %>% lubridate::ymd() -> date_plaq

  mois_plaq <-
    lubridate::month(date_plaq, label = TRUE, abbr = FALSE) %>% as.character()

  mois_annee_plaq <-
    paste(mois_plaq, stringr::str_sub(params$annee_mois, 1, 4))

  date_maj <-
    paste(utf8::as_utf8("La construction neuve de logements \u00e0 fin"),
          mois_annee_plaq)

  xml2::xml_set_attr(
    x = ls_modele$pg %>%
      rvest::html_element(paste0(
        "PAGEOBJECT[ItemID=\'", id_titre, "\']"
      )) %>%
      xml2::xml_child("StoryText")  %>%
      xml2::xml_child("ITEXT"),
    attr = "CH",
    value = date_maj
  )
}
