#' fn18_maj_tableaux
#'
#' Scribus : tableaux, tableau des cellules a remplacer
#'
#' @param data liste
#' @return df
#' @importFrom dplyr across
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr slice
#' @importFrom dplyr tibble
#' @importFrom dplyr where
#' @importFrom purrr flatten_chr
#' @importFrom purrr imap_dfr
#' @importFrom purrr map2_dfr
#' @importFrom purrr pwalk
#' @importFrom stats complete.cases
#' @importFrom stringr str_squish
#' @importFrom tidyr expand_grid
#' @importFrom utf8 as_utf8
#' @importFrom utf8 utf8_valid
#' @export
fn18_maj_tableaux <- function(data = ls_tab) {
  # Modification des tableaux --------
  #
  purrr::map(data,
             ~ .x |>
               dplyr::mutate(dplyr::across(dplyr::where(is.factor), as.character))) -> ls_tab

  fn_tableau_transforme_tabw_to_tabl <-
    function(x = ls_tab[[1]]) {
      x -> df0
      nrow(df0) -> nblgn
      ncol(df0) -> nbcol
      tidyr::expand_grid("lgn" = 1:nblgn,
                         "col" = 1:nbcol - 1) |>
        dplyr::mutate(
          "txt_new" = purrr::map(1:nblgn, ~ df0 |>
                                   dplyr::slice(.x) |>
                                   unlist() |>
                                   unname()) |>
            purrr::flatten_chr() |> stringr::str_squish()
        ) -> df1
      return(df1)
    }
  # fn_tableau_transforme_tabw_to_tabl()
  purrr::map(.x = ls_tab,
             .f = fn_tableau_transforme_tabw_to_tabl) -> ls_tab_contenu
  # ls_tab_contenu[["tab4"]] -> toto




  # purrr::imap_dfr(ls_tab_contenu,
  #                 ~ dplyr::tibble(noms_objets2 = .y, nbr = nrow(.x))) -> eff
  #
  # ls_modele$t_tableaux_nb_col_lgn |>
  #   dplyr::left_join(eff, by = "noms_objets2") -> t_tableaux_compare_valeurs

  # purrr::map(ls_tab_contenu, ~ utf8::utf8_valid(.x$txt_new))

  purrr::map(ls_tab_contenu,
             ~ .x |>
               dplyr::mutate(txt_new = utf8::as_utf8(txt_new))) -> ls_tab_contenu

  purrr::map2_dfr(
    ls_tab_contenu,
    ls_modele$t_tableaux |> dplyr::pull(id_objet),
    ~ .x |>
      dplyr::mutate(id_objet = .y),
    ncol = as.integer(ncol)
  ) -> df_tab_contenu


  ls_modele$t_tableaux_cell |>
    dplyr::left_join(df_tab_contenu, by = c("id_objet", "lgn", "col")) -> ls_modele$t_tableaux_cell

  ls_modele$t_tableaux_cell |>
    dplyr::filter(complete.cases(txt_new)) -> toto

  purrr::pwalk(list("id_objet" = toto$id_objet,
                    "num_cell" = toto$num_cell,
                    "txt_new" = toto$txt_new),
               .f = fn_scri_modifie_tableau)

  return(toto)
}
