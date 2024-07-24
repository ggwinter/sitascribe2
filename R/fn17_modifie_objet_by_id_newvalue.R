#' fn17_modifie_objet_by_id_newvalue
#'
#' Scribus : Modifie la valeur d attribut d un node
#'
#' @param id_objet identifiant scribus de l objet
#' @param attr le nom de l attribut
#' @param new_value la nouvelle valeur
#' @importFrom purrr pwalk
#' @importFrom rvest html_element
#' @importFrom xml2 xml_set_attr
#'
#' @return nothing
#' @export
fn17_modifie_objet_by_id_newvalue <- function(id_objet = ls_modele$t_images$id_objet,
                                              attr = ls_modele$t_images$attr,
                                              new_value = ls_modele$t_images$new_value) {
  purrr::pwalk(
    .l = list(id_objet, attr, new_value),
    .f = ~ xml2::xml_set_attr(
      x = ls_modele$pg |>
        rvest::html_element(paste0("PAGEOBJECT[ItemID=\'", ..1, "\']")),
      attr = ..2,
      value = ..3
    )
  )
}

