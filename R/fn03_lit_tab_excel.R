#' fn03_lit_tab_excel
#'
#' @param x vecteur caractere path to file
#' @importFrom attempt stop_if
#' @importFrom cli bg_red
#' @importFrom cli col_yellow
#' @importFrom dplyr all_of
#' @importFrom dplyr arrange
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr one_of
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr summarise_all
#' @importFrom here here
#' @importFrom janitor clean_names
#' @importFrom purrr imap
#' @importFrom purrr set_names
#' @importFrom readxl excel_sheets
#' @importFrom readxl read_excel
#' @importFrom stringr str_c
#' @importFrom stringr str_extract
#' @importFrom stringr str_pad
#' @importFrom stringr str_replace
#' @importFrom stringr str_subset
#' @importFrom writexl write_xlsx
#' @return liste
#' @export
fn03_lit_tab_excel <- function(x = params$annee_mois) {
  list.files(
    here::here("2_data"),
    pattern = stringr::str_c("^ROES_", x),
    full.names = TRUE
  ) -> chem_fich
  if (length(chem_fich) > 1)
    chem_fich |> stringr::str_subset("xlsx") -> chem_fich


  stopifnot(exprs = file.exists(chem_fich) == TRUE)

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
    msg = cli::bg_red(
      cli::col_yellow(
        "Data PB : Au moins un des onglets du tableau excel a un nom qui
        ne correspond pas au mod\u00e8le - Georges"
      )
    )
  )

  # lecture des tableaux excel

  purrr::map(choix_onglet,
             ~ readxl::read_excel(chem_fich, .x) |>
               janitor::clean_names()) |>
    purrr::set_names(choix_onglet) -> lsm

  COGiter::departements |>
    dplyr::mutate(DEP = stringr::str_pad(DEP, 3, pad = "0")) -> x_cog_dep

  lsm$AUT_DPT -> eff

  fn_modifie_lsm <- function(data, nom) {
    if (grepl(pattern = "FR", nom) == TRUE) {
      data |>
        dplyr::mutate("terr_cd" = "999",
                      "territoire" = "France",
                      "geo" = 1L) |>
        dplyr::select(dplyr::one_of(c(
          "geo", "date", "terr_cd", "territoire"
        )), dplyr::everything())  |>
        dplyr::arrange(terr_cd, date) |>
        dplyr::filter(terr_cd > 10) |>
        dplyr::select(-terr_cd, -territoire) |>
        dplyr::group_by(geo, date) |>
        dplyr::summarise_all(list(sum)) |>
        dplyr::mutate(terr_cd = "666",
                      "territoire" = "France m\u00e9tro.",
                      "geo" = 1L) -> data_resultat

    } else{
      if (grepl(pattern = "REG", nom) == TRUE) {
        data |>
          dplyr::rename("terr_cd" = "reg", "territoire" = "nom_reg") |>
          dplyr::mutate("geo" = 2L) -> data_resultat
      } else{
        data |>
          dplyr::left_join(x_cog_dep |> dplyr::select(DEP, NCCENR),
                           by = c("dpt" = "DEP")) |>
          dplyr::rename("terr_cd" = "dpt",
                        "territoire" = "NCCENR") |>
          dplyr::mutate("geo" = 3L) -> data_resultat
      }
    }
    data_resultat  |>
      dplyr::mutate("type" = stringr::str_extract(nom, "^.{3,6}(?=_)") |> tolower()) -> data_resultat

    names(data_resultat) <-
      stringr::str_replace(names(data_resultat),
                  pattern = "_aut|_com",
                  replacement = "")

    if (grepl(pattern = "FR", nom) == TRUE) {
      data_resultat |> dplyr::mutate(colres = col + res) |>
        select(-col, -res) -> data_resultat
    }
    data_resultat  |>
      dplyr::select(dplyr::all_of(
        c(
          "geo",
          "date",
          "terr_cd",
          "territoire",
          "log",
          "ip",
          "ig",
          "colres",
          "type"
        )
      )) |>
      dplyr::arrange(terr_cd, date) -> data_resultat
    return(data_resultat)
  }
  # fn_modifie_lsm(data = lsm[[1]], nom = names(lsm[1]))

  purrr::imap(lsm, fn_modifie_lsm) -> lsm


  uti_transpose_liste(lsm) -> lsm
  # si fichier xls on le remplace par un fichier xlsx
  if (grepl(pattern = ".xls$", chem_fich)) {
    readxl::excel_sheets(chem_fich) |> purrr::set_names() -> eff
    purrr::map(eff,
               ~ readxl::read_excel(chem_fich[1], .x)) -> eff
    writexl::write_xlsx(eff, here::here("2_data", paste0("ROES_", x, ".xlsx")))
    unlink(chem_fich)
    rm(eff)
  }

  return(lsm)

}

