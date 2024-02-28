#' fn_scri_ls_nodes_tableau
#'
#' Scribus interroge le fichier sla pour extraire tous les nodes des cellules
#' d un tableau.
#'
#' @param id_objet numero unique scribus du tableau
#' @importFrom rvest html_element
#' @importFrom xml2 xml_find_all
#' @return nodes avec uniquement les cellules du tableau
#' @export
fn_scri_ls_nodes_tableau <- function(id_objet = "211587152") {
  ls_modele$pg |>
    rvest::html_element(paste0("PAGEOBJECT[ItemID=\'", id_objet, "\']"))  |>
    xml2::xml_find_all("TableData")  |>
    xml2::xml_find_all("Cell") -> ph
  return(ph)
}
