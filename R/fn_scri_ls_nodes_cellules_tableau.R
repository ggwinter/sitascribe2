#' fn_scri_ls_nodes_cellules_tableau
#'
#' Scribus interroge le fichier sla pour extraire le node de la cellule specifiee
#' d un tableau.
#'
#' @param id_objet id unique du tableau
#' @param num_cell id de la cellule
#' @importFrom rvest html_element
#' @importFrom xml2 xml_find_all
#' @return nodes de la cellules
#' @export
fn_scri_ls_nodes_cellules_tableau <-
  function(id_objet = "211587152",
           num_cell = 2) {
    ls_modele$pg |>
      rvest::html_element(paste0("PAGEOBJECT[ItemID=\'", id_objet, "\']"))  |>
      xml2::xml_find_all("TableData")  |>
      xml2::xml_find_all("Cell") |> (\(.) .[[num_cell]])() -> ph
    return(ph)
  }
