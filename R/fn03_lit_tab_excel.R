#' fn03_lit_tab_excel
#'
#' @param x vecteur caractere path to file
#' @importFrom attempt stop_if
#' @importFrom cli bg_red
#' @importFrom cli col_yellow
#' @importFrom dplyr arrange
#' @importFrom dplyr contains
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr one_of
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr summarise_all
#' @importFrom dplyr vars
#' @importFrom janitor clean_names
#' @importFrom purrr map_at
#' @importFrom purrr map2
#' @importFrom purrr set_names
#' @importFrom readxl excel_sheets
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_replace
#' @return liste
#' @export
fn03_lit_tab_excel <- function(x = params$annee_mois) {

  here::here("2_data", stringr::str_c("ROES_", x, ".xlsx"))-> chem_fich
  stopifnot(exprs = file.exists(chem_fich)==TRUE)

  choix_onglet <-
    c("AUT_FR",
      "AUT_NEW_REG",
      "AUT_DPT",
      "COM_FR",
      "COM_NEW_REG",
      "COM_DPT")

  error_fichier_xlsx_onglet_incorrect <- function(x) {
    any(x %in% readxl::excel_sheets(chem_fich)) == FALSE
  }
  attempt::stop_if(
    .x = choix_onglet,
    .p = error_fichier_xlsx_onglet_incorrect,
    msg = cli::bg_red(cli::col_yellow("Data PB : Au moins un des onglets du tableau excel a un nom qui
        ne correspond pas au mod\u00e8le - Georges"))
  )

  # lecture des tableaux excel

  purrr::map(choix_onglet,
             ~ readxl::read_xlsx(chem_fich, .x) %>% janitor::clean_names()) %>%
    purrr::set_names(choix_onglet) -> lsm

  COGiter::departements %>%
    dplyr::mutate(DEP = stringr::str_pad(DEP, 3, pad = "0"))-> x_cog_dep

  lsm$AUT_DPT-> eff

  purrr::map_at(
    .x = lsm,
    .at = dplyr::vars(dplyr::contains("FR")),
    .f = ~ .x %>%
      dplyr::mutate(
        "terr_cd" = "999",
        "territoire" = "France",
        "geo" = 1L
      ) %>%
      dplyr::select(dplyr::one_of(
        c("geo", "date", "terr_cd", "territoire")
      ), dplyr::everything()) %>%
      dplyr::arrange(terr_cd, date) %>%
      dplyr::filter(terr_cd > 10) %>%
      dplyr::select(-terr_cd, -territoire) %>%
      dplyr::group_by(geo, date) %>%
      dplyr::summarise_all(list(sum)) %>%
      dplyr::mutate(
        terr_cd = "666",
        "territoire" = "France m\u00e9tro.",
        "geo" = 1L
      ) %>%
      dplyr::select(dplyr::one_of(
        c("geo",
          "date",
          "terr_cd",
          "territoire")
      ), dplyr::everything())
  ) %>%
    purrr::map_at(
      .at = dplyr::vars(dplyr::contains("REG")),
      .f = ~ .x %>%
        dplyr::rename("terr_cd" = "reg", "territoire" = "nom_reg") %>%
        dplyr::mutate("geo" = 2L) %>%
        dplyr::select(dplyr::one_of(
          c("geo", "date", "terr_cd", "territoire")
        ), dplyr::everything()) %>%
        dplyr::arrange(terr_cd, date)
    ) %>%
    purrr::map_at(
      .at = dplyr::vars(dplyr::contains("DPT")),
      .f = ~ .x %>%
        dplyr::left_join(x_cog_dep %>% dplyr::select(DEP, NCCENR), by = c("dpt" = "DEP")) %>%
        dplyr::rename("terr_cd" = "dpt",
                      "territoire" = "NCCENR") %>%
        dplyr::mutate("geo" = 3L) %>%
        dplyr::select(dplyr::one_of(
          c("geo", "date", "terr_cd", "territoire")
        ), dplyr::everything()) %>%
        dplyr::arrange(terr_cd, date)
    ) %>%
    purrr::map_at(.at = dplyr::vars(1:3),
                  .f = ~ .x %>% dplyr::mutate(type = "aut")) %>%
    purrr::map_at(.at = dplyr::vars(4:6),
                  .f = ~ .x %>% dplyr::mutate(type = "com")) -> lsm

  purrr::map(lsm, ~ stringr::str_replace(names(.x), "_aut|_com", "")) -> ls_new_champ

  purrr::map2(lsm, ls_new_champ, ~ .x %>% purrr::set_names(.y)) -> lsm

  purrr::map_at(
    .x = lsm,
    .at = dplyr::vars(dplyr::contains("FR")),
    .f = ~ .x %>%
      dplyr::mutate(colres = col + res) %>%
      dplyr::select(geo, date, terr_cd, territoire, log, ip, ig, type, colres)
  ) -> lsm

  uti_transpose_liste(lsm)-> lsm

  return(lsm)

}
