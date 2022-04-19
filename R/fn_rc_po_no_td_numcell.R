#' Scribus interroge le fichier sla pour extraire tous les nodes d'une cellule
#' d'un tableau.
#'
#' @param x num_objet id du tableau
#' @param y num_cell id de la cellule
#' @importFrom xml2 xml_find_all
#' @return nodes de la cellule
#' @export
fn_rc_po_no_td_numcell <- function(x = "211587152",
                                   y = 2) {
  ls_modele$pg %>%
    rvest::html_element(paste0("PAGEOBJECT[ItemID=\'", x, "\']")) %>%
    xml2::xml_find_all("TableData") %>%
    xml2::xml_find_all("Cell") %>% .[[y]] -> ph
  return(ph)
}
