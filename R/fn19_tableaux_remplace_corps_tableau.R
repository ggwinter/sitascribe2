#' fn19_tableaux_remplace_corps_tableau
#'
#' scribus, Tableaux, ajoute les valeurs issues des calculs dans les tableaux
#'
#' @param x caractere
#'
#' @return rien
#' @importFrom purrr pwalk
#' @importFrom purrr walk
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_add_child
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_set_attr
#' @export
fn19_tableaux_remplace_corps_tableau <-
  function(x = 'aut') {

    # read_xml(chem_fich, encoding = 'UTF-8') -> pg

    fn_complete_tableau <-
      function(id_objet = "211587152" ,
               num_cell = 8,
               txt_new = "Corse-du-Sud") {
        purrr::walk(
          c(
            "LeftPadding",
            "RightPadding",
            "TopPadding",
            "BottomPadding"
          ),
          ~ xml2::xml_set_attr(fn_scri_ls_nodes_tableau(id_objet), .x, 1)
        )


        if (fn_scri_ls_nodes_cellules_tableau(id_objet, num_cell) |>
            xml2::xml_find_all("StoryText")  |>
            xml2::xml_find_all("trail") |>
            xml2::xml_attr(., "PARENT") |> is.na() == FALSE) {
          xml2::xml_set_attr(
            fn_scri_ls_nodes_cellules_tableau(id_objet, num_cell) |>
              xml2::xml_find_all("StoryText")  |>
              xml2::xml_find_all("trail"),
            "PARENT",
            NULL
          )
        }

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


        # if (ls_modele$pg |> xml_find_all(".//PAGEOBJECT") |> (\(.) .[[id_objet]])() |>
        #     xml_find_all("TableData") |>
        #     xml_find_all("Cell") |> (\(.) .[[num_cell]])() |>
        #     xml_find_all("StoryText") |>
        #     xml_find_all("DefaultStyle") |>
        #     xml_attr("PARENT") |> na.omit() |> length() == 0) {
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
        xml2::xml_set_attr(
          fn_scri_ls_nodes_cellules_tableau(id_objet, num_cell) |>
            xml2::xml_find_all("StoryText") |>
            xml2::xml_find_all("ITEXT"),
          "CH",
          txt_new
        )
      }

    purrr::pwalk(
      list(
        "id_objet" = toto$id_objet,
        "num_cell" = toto$num_cell,
        "txt_new" = toto$txt_new
      ),
      .f = fn_complete_tableau
    )
  }

