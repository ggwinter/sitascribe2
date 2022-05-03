#' fn16_lit_le_modele_sla
#'
#' Scribus : texte, lit la plaquette modele, supprime les lignes et en cree
#' autant de vides que dans le nouveau texte
#'
#' @param x date annee etude
#'
#' @return liste
#' @importFrom dplyr arrange
#' @importFrom dplyr case_when
#' @importFrom dplyr count
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr tibble
#' @importFrom here here
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom purrr map_dfr
#' @importFrom purrr map2
#' @importFrom purrr map2_dfr
#' @importFrom purrr set_names
#' @importFrom purrr walk
#' @importFrom purrr walk2
#' @importFrom rvest html_element
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace
#' @importFrom stringr str_which
#' @importFrom stats na.omit
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_add_child
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_attrs
#' @importFrom xml2 xml_child
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_remove
#' @export
fn16_lit_le_modele_sla <- function(x = 'txt_plaquette') {
  # Lecture du texte avant modification de la plaquette

  # library(magrittr)
  # txt_plaquette <-
  #   here::here("3_tables", "P_SCIL_SITADEL_2022trim1_t01.sla")


  xml2::read_xml(plaquette_sitadel, encoding = 'UTF-8') -> pg


  # liste des objets nommés

  pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>%
    xml2::xml_attrs() -> eff

  purrr::map_dfr(eff, ~ .x[c("ANNAME", "ItemID")], .id = "num_objet") %>%
    dplyr::rename(c("nom_objet" = "ANNAME", "id_objet" = "ItemID")) %>%
    dplyr::arrange(nom_objet) -> t_objets_numero

  # les images --------
  #
  t_objets_numero %>%
    dplyr::filter(stringr::str_detect(nom_objet , "image|logo|qrcode|graphe|carte")) -> t_images

  purrr::map_dfr(eff[as.integer(t_images$num_objet)], ~ .x[c("ANNAME", "ItemID", "PFILE")], .id = "num_objet") %>%
    dplyr::select(num_objet, ANNAME, ItemID, PFILE) %>%
    dplyr::filter(stats::complete.cases(PFILE)) %>%
    dplyr::rename(c("nom_objet" = "ANNAME", "id_objet" = "ItemID")) %>%
    dplyr::mutate(
      "new_value" = dplyr::case_when(
        stringr::str_detect(nom_objet, "image|logo|qrcode") ~ stringr::str_replace(PFILE, "../4_resultats", ".."),
        stringr::str_detect(nom_objet, "graphe|carte") ~
          stringr::str_replace(PFILE, paste0("../4_resultats/202202"), ".")
      ),
      "attr" = "PFILE"
    ) %>% dplyr::arrange(nom_objet) -> t_images


  # les paragraphes ----------

  t_objets_numero %>%
    dplyr::filter(stringr::str_detect(nom_objet, "_texte|_resume")) -> t_paragraphes



  # Les tableaux ----------
  #
  #
  # liste des objets nommés



  # purrr::map_dfr(eff, ~ .x[c("ItemID", "CellAreas")], .id = "num_objet") %>%
  #   dplyr::rename(c("nom_objet" = "ANNAME", "id_objet" = "ItemID")) -> t_objets_numero




  t_objets_numero %>%
    dplyr::filter(stringr::str_detect(nom_objet, "tab")) %>%
    dplyr::arrange(num_objet) %>%
    dplyr::mutate(num_objet = as.integer(num_objet))  %>%
    dplyr::mutate(tab = stringr::str_extract(nom_objet, "(?<=p[:digit:][:lower:]_).*$")) %>%
    dplyr::arrange(nom_objet) -> t_tableaux


  # verifie les cell_areas
  pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>% xml2::xml_attrs() -> eff
  purrr::map(eff, ~ .x[c("ItemID", "CellAreas")]) -> eff2
  eff2[t_tableaux %>% dplyr::pull(num_objet)] -> eff2
  # tableau existant

  # ajout des infos manquantes à la table t_tableaux

  fn_tableaux_infos <- function(x = "211587152") {
    pg %>%
      rvest::html_element(paste0("PAGEOBJECT[ItemID=\'", x, "\']")) -> ph

    dplyr::tibble(
      id_objet = x,
      nb_cell =  ph %>%
        xml2::xml_find_all("TableData") %>%
        xml2::xml_find_all("Cell") %>% length(),
      nb_lgn = ph %>%
        xml2::xml_attr("Rows") %>% as.integer(),
      nb_col = ph %>%
        xml2::xml_attr("Columns") %>% as.integer()
    ) -> t_tableaux_comp
    return(t_tableaux_comp)
  }
  purrr::map_dfr(t_tableaux$id_objet, fn_tableaux_infos) -> eff3
  t_tableaux %>% dplyr::inner_join(eff3, by = "id_objet") -> t_tableaux

  purrr::map2_dfr(
    t_tableaux$id_objet,
    t_tableaux$nb_cell,
    ~ dplyr::tibble("id_objet" = .x,
                    "num_cell" = 1:.y)
  ) -> t_tableaux_cell

  fn_tableaux_lit_cell <- function(x = "211587152", y = 2) {
    pg %>%
      rvest::html_element(paste0("PAGEOBJECT[ItemID=\'", x, "\']")) -> ph

    dplyr::tibble(
      "id_objet" = x,
      "num_cell" = y,

      "lgn" = ph %>%
        xml2::xml_find_all("TableData") %>%
        xml2::xml_find_all("Cell") %>% .[[y]] %>%
        xml2::xml_attr("Row") %>% as.integer(),
      "col" = ph %>%
        xml2::xml_find_all("TableData") %>%
        xml2::xml_find_all("Cell") %>% .[[y]] %>%
        xml2::xml_attr("Column") %>% as.integer()
    ) -> df
    return(df)
  }

  purrr::map2_dfr(t_tableaux_cell$id_objet,
                  t_tableaux_cell$num_cell,
                  fn_tableaux_lit_cell) -> t_tableaux_cell


  fn_tableaux_trouve_cell_vides <- function(x = "211587152") {
    pg %>%
      rvest::html_element(paste0("PAGEOBJECT[ItemID=\'", x, "\']")) -> ph
    dplyr::tibble(id_objet = x,
                  cell_vides = which(
                    purrr::map_chr(
                      .x = x,
                      .f = ~ ph %>%
                        xml2::xml_find_all("TableData") %>%
                        xml2::xml_find_all("Cell") %>%
                        xml2::xml_find_all("StoryText") %>%
                        xml2::xml_find_all("ITEXT") %>% length()# on verifie pour chaque objet qu'il y a bien un Itext
                    ) %in% "0"
                  )) -> df
    return(df)
  }


  purrr::map_dfr(t_tableaux_cell$id_objet,
                 fn_tableaux_trouve_cell_vides) -> t_tableaux_cell_vides


  x <- 20
  y <- 1

  fn_remplie_tableau_node_itext <- function(x = "211587152", y) {
    xml2::xml_add_child(
      pg %>%
        rvest::html_element(paste0("PAGEOBJECT[ItemID=\'", x, "\']")) %>%
        xml2::xml_find_all("TableData") %>%
        xml2::xml_find_all("Cell") %>% .[[y]] %>%
        xml2::xml_find_all("StoryText"),

      xml2::read_xml("<ITEXT CH=''/>")# complete les cellules sans itext
    )
  }
  if (nrow(t_tableaux_cell_vides) > 0) {
    purrr::walk2(
      t_tableaux_cell_vides$id_objet,
      t_tableaux_cell_vides$cell_vides,
      fn_remplie_tableau_node_itext
    )
  }




  fn_tableaux_lit_cell_txt <- function(x = "211587152", y = 2) {
    dplyr::tibble(
      "id_objet" = x,
      "num_cell" = y,

      "txt" = pg %>%
        rvest::html_element(paste0("PAGEOBJECT[ItemID=\'", x, "\']")) %>%
        xml2::xml_find_all("TableData") %>%
        xml2::xml_find_all("Cell") %>% .[[y]] %>%
        xml2::xml_find_all("StoryText") %>%
        xml2::xml_find_all("ITEXT") %>%
        xml2::xml_attr("CH"),

    ) -> df
    return(df)
  }

  purrr::map2_dfr(t_tableaux_cell$id_objet,
                  t_tableaux_cell$num_cell,
                  fn_tableaux_lit_cell_txt) -> t_tableaux_cell_txt

  t_tableaux_cell %>%
    dplyr::left_join(t_tableaux_cell_txt, by = c("id_objet", "num_cell")) -> t_tableaux_cell

  # !!! a modifier lier id_objet num_tab
  t_tableaux_cell %>% dplyr::filter(!lgn %in% 0) %>%
    dplyr::count(id_objet, name = "nbp") %>%
    dplyr::mutate(noms_objets2 = c("tab1", "tab2", "tab3", "tab4")) -> t_tableaux_nb_col_lgn

  ls_modele <- list(
    "pg" = pg,
    "t_objets_numero" = t_objets_numero,
    "t_images" = t_images,
    "t_paragraphes" = t_paragraphes,
    "t_tableaux" = t_tableaux,
    "t_tableaux_cell" = t_tableaux_cell,
    "t_tableaux_nb_col_lgn" = t_tableaux_nb_col_lgn
  )

  return(ls_modele)

}
