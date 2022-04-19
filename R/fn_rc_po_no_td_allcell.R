#' Scribus interroge le fichier sla pour extraire tous les nodes des cellules
#' d'un tableau.
#'
#' @param x id_objet numero de l objet
#' @importFrom xml2 xml_find_all
#' @return nodes avec uniquement les cellules du tableau
#' @export
fn_rc_po_no_td_allcell <- function(x = "211587152") {
  ls_modele$pg %>%
    rvest::html_element(paste0("PAGEOBJECT[ItemID=\'", x, "\']")) %>%
    xml2::xml_find_all("TableData") %>%
    xml2::xml_find_all("Cell") -> ph
  return(ph)
}
