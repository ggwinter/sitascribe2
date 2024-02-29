#' fn_scri_modifie_tableau
#'
#' @param id_objet identifiant unique du tableau
#' @param num_cell identifiant unique de la cellule
#' @param txt_new  nouveau texte pour remplacer l ancien
#'
#' @return nothing
#'
#' @importFrom purrr walk
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_add_child
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_set_attr
#' @export
#'
fn_scri_modifie_tableau <- function(id_objet = "211587152" ,
                                    num_cell = 8,
                                    txt_new = "Corse-du-Sud") {
  # Ajouter une marge dans la cellule gauche droite haut et bas
  purrr::walk(
    c(
      "LeftPadding",
      "RightPadding",
      "TopPadding",
      "BottomPadding"
    ),
    ~ xml2::xml_set_attr(fn_scri_ls_nodes_tableau(id_objet),
                         .x,
                         1)
  )

  # Ajouter le style PARENT dans la cellule gauche droite haut et bas
  if (fn_scri_ls_nodes_cellules_tableau(id_objet, num_cell) |>
      xml2::xml_find_all("StoryText")  |>
      xml2::xml_find_all("trail") |>
      xml2::xml_attr("PARENT") |> is.na() == FALSE) {
    xml2::xml_set_attr(
      fn_scri_ls_nodes_cellules_tableau(id_objet, num_cell) |>
        xml2::xml_find_all("StoryText")  |>
        xml2::xml_find_all("trail"),
      "PARENT",
      NULL
    )
  }
  # Fixer la distance entre le bord de la cellule et le texte a 1 unite
  # Aligner verticalement le texte
  purrr::walk(
    c(
      "TextDistLeft",
      "TextDistTop",
      "TextDistBottom",
      "TextDistRight",
      "TextVertAlign"
    ),
    ~ xml2::xml_set_attr(fn_scri_ls_nodes_cellules_tableau(id_objet, num_cell),
                         .x,
                         "1")
  )


  # if (ls_modele$pg |> xml_find_all(".//PAGEOBJECT") |>
  #     (\(.) .[[id_objet]])() |>
  #     xml_find_all("TableData") |>
  #     xml_find_all("Cell") |>
  #     (\(.) .[[num_cell]])() |>
  #     xml_find_all("StoryText") |>
  #     xml_find_all("DefaultStyle") |>
  #     xml_attr("PARENT") |> na.omit() |> length() == 0) {

  # Imposer le type de paragraphe suivant le style de la cellule
  # PARENT sty_tab_droite et CPARENT tab_normal
  xml2::xml_set_attr(
    fn_scri_ls_nodes_cellules_tableau(id_objet, num_cell) |>
      xml2::xml_find_all("StoryText") |>
      xml2::xml_find_all("DefaultStyle"),
    "PARENT",
    "sty_tab_droite"
  )

  xml2::xml_set_attr(
    fn_scri_ls_nodes_cellules_tableau(id_objet, num_cell) |>
      xml2::xml_find_all("StoryText") |>
      xml2::xml_find_all("DefaultStyle"),
    "CPARENT",
    "tab_normal"
  )
  # }
  # suivant le modele de plaquette une cellule peut ne pas contenir de texte
  # dans ce cas dans le node StoryText  creer ITEXT
  if (fn_scri_ls_nodes_tableau(id_objet) |>
      (\(.) .[[num_cell]])() |>
      xml2::xml_find_all("StoryText") |>
      xml2::xml_find_all("ITEXT") |> length() == 0) {
    xml2::xml_add_child(
      fn_scri_ls_nodes_cellules_tableau(id_objet, num_cell) |>
        xml2::xml_find_all("StoryText"),
      xml2::read_xml("<ITEXT CH=''/>")# complete les cellules sans itext
    )


  }
  # Ajouter le nouveau contenu dans la cellule attribut CH du node ITEXT
  xml2::xml_set_attr(
    fn_scri_ls_nodes_cellules_tableau(id_objet, num_cell) |>
      xml2::xml_find_all("StoryText") |>
      xml2::xml_find_all("ITEXT"),
    "CH",
    txt_new
  )
}
