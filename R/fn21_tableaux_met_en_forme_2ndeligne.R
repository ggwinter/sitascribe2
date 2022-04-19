#' fn21_tableaux_met_en_forme_2ndeligne
#'
#' Scribus, tableaux, mise en forme des sous-indicateurs Enregistre aussi toutes les modifications en sortie
#'
#' @param x df
#'
#' @return rien
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom here here
#' @importFrom purrr flatten_chr
#' @importFrom purrr walk2
#' @importFrom utf8 as_utf8
#' @importFrom xml2 write_xml
#' @importFrom xml2 xml_add_child
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_set_attr
#' @export
fn21_tableaux_met_en_forme_2ndeligne <-
  function(x = toto) {
    x %>% dplyr::filter(lgn == 1) %>%
      dplyr::distinct(id_objet, num_cell) -> t_tableaux_cell_2ndeligne

    fn_tableaux_mef_2ndeligne <-
      function(id_objet = "211587152" ,
               num_cell = 8) {
        xml2::xml_set_attr(
          fn_rc_po_no_td_numcell(id_objet, num_cell) %>%
            xml2::xml_find_all("StoryText") %>%
            xml2::xml_find_all("DefaultStyle"),
          "PARENT",
          "sty_tab_centre"
        )

        xml2::xml_set_attr(
          fn_rc_po_no_td_numcell(id_objet, num_cell) %>%
            xml2::xml_find_all("StoryText") %>%
            xml2::xml_find_all("DefaultStyle"),
          "CPARENT",
          NULL
        )

        xml2::xml_set_attr(
          fn_rc_po_no_td_numcell(id_objet, num_cell) %>%
            xml2::xml_find_all("StoryText") %>%
            xml2::xml_find_all("trail"),
          "PARENT",
          "sty_tab_gras_centre"
        )

      }

    purrr::walk2(t_tableaux_cell_2ndeligne$id_objet,
                 t_tableaux_cell_2ndeligne$num_cell,
                 .f = fn_tableaux_mef_2ndeligne)

    xml2::write_xml(ls_modele$pg,
                    here::here(
                      "4_resultats",
                      params$annee_mois,
                      paste0("P_SCIL_OL_Sitadel_", params$annee_mois, "_T01.sla")
                    ),
                    overwrite = TRUE)
  }
