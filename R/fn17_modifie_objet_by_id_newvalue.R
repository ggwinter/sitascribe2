#' fn17_modifie_objet_by_id_newvalue
#'
#' Scribus : Modifie la valeur d attribut d un node
#'
#' @param x identifiant scribus de l objet
#' @param y le nom de l attribut
#' @param z la nouvelle valeur
#' @importFrom purrr pwalk
#' @importFrom rvest html_element
#' @importFrom xml2 xml_set_attr
#'
#' @return nothing
#' @export
fn17_modifie_objet_by_id_newvalue <- function(x = ls_modele$t_images$id_objet,
                                              y = ls_modele$t_images$attr,
                                              z = ls_modele$t_images$new_value) {
  purrr::pwalk(
    .l = list(x, y, z),
    .f = ~ xml2::xml_set_attr(
      x = ls_modele$pg %>%
        rvest::html_element(paste0("PAGEOBJECT[ItemID=\'", ..1, "\']")),
      attr = ..2,
      value = ..3
    )
  )
}

