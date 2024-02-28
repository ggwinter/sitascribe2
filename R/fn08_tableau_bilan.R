#' fn08_tableau_bilan
#'
#' @param x charactere aut ou com
#'
#' @return dataframe
#' @importFrom attempt stop_if
#' @importFrom cli bg_red
#' @importFrom cli col_yellow
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr one_of
#' @importFrom dplyr select
#' @importFrom dplyr tibble
#' @importFrom dplyr ungroup
#' @importFrom here here
#' @importFrom purrr reduce
#' @importFrom stringr str_replace
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @export
fn08_tableau_bilan <- function(x = "aut") {

  attempt::stop_if(.x = x,
                   .p = ~!is.character(.x),
                   msg = cli::bg_red(cli::col_yellow("x doit etre au format caractere")))
  attempt::stop_if(.x = x,
                   .p = ~!.x %in% c("aut", "com"),
                   msg = cli::bg_red(cli::col_yellow("x doit etre eagl a aut ou com")))
  t_trim <-
    dplyr::tibble(
      date = ls_dates$liste_mois_trim,
      trimestre = c("trim", "trim1", "trim2", "trim_b", "trim1_b", "trim2_b")
    )

  purrr::reduce(
    list(
      lsm_12m0$FR,
      lsm_12m0$NEW_REG |> dplyr::filter(terr_cd %in% "94"),
      lsm_12m0$DPT |> dplyr::filter(terr_cd %in% c("02A", "02B"))
    ),
    .f = dplyr::bind_rows
  ) |>
    dplyr::filter(date %in% ls_dates$liste_mois_trim) |>
    dplyr::left_join(t_trim, by = "date") |>
    dplyr::ungroup() |>
    dplyr::select(c(type, geo, terr_cd, territoire, trimestre, log, ip, ig, colres)) |>
    tidyr::pivot_longer(cols = c(log:colres),
                        names_to = "variable",
                        values_to = "value") |>
    tidyr::pivot_wider(names_from = trimestre,
                       values_from = value) |>
    dplyr::mutate(
      diff_trim = round((trim - trim_b) / trim_b, 3),
      diff_trim1 = round((trim1 - trim1_b) / trim1_b, 3),
      diff_trim2 = round((trim2 - trim2_b) / trim2_b, 3),
      variable = stringr::str_replace(variable, "\\_.{3}", "")
    ) -> bilan_evo

  purrr::reduce(
    list(
      lsm_12m$FR,
      lsm_12m$NEW_REG |> dplyr::filter(terr_cd %in% "94"),
      lsm_12m$DPT |> dplyr::filter(terr_cd %in% c("02A", "02B"))
    ),.f = dplyr::bind_rows) |>
    dplyr::filter(date %in% params$annee_mois) |>
      dplyr::select(c(type, geo, terr_cd, territoire, date, log, ip, ig, colres)) |>
      tidyr::pivot_longer(
        cols = c(log:colres),
        names_to = "variable",
        values_to = "value"
      )-> bilan

  bilan |>
    dplyr::left_join(bilan_evo,
                     by = c("type", "geo", "terr_cd", "territoire", "variable")) |>
    dplyr::mutate(territoire = factor(
      territoire,
      levels = c("Corse-du-Sud", "Haute-Corse", "Corse", "France m\u00e9tro.")
    )) |>
    dplyr::select(dplyr::one_of(
      c(
        "geo",
        "date",
        "terr_cd",
        "territoire",
        "type",
        "variable",
        "value",
        "trim",
        "trim_b",
        "diff_trim",
        "trim1",
        "trim1_b",
        "diff_trim1",
        "trim2",
        "trim2_b",
        "diff_trim2"
      )
    )) -> bilan

  filename <- here::here("4_resultats", params$annee_mois, "doc_travail", "bilan.csv")
  utils::write.csv2(bilan, filename,
                    row.names = FALSE)

  return(bilan)
}
