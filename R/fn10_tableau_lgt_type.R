#' fn10_tableau_lgt_type
#'
#' Creation des tableaux au format texte pour la plaquette
#'
#' @param x caractere aut ou com
#'
#' @return dataframe
#' @importFrom attempt stop_if
#' @importFrom cli bg_red
#' @importFrom cli col_yellow
#' @importFrom dplyr across
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr contains
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr one_of
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom here here
#' @importFrom purrr set_names
#' @importFrom stringr str_replace
#' @export
fn10_tableau_lgt_type <- function(x = "toto") {

  # tableau 2

  attempt::stop_if(
    .x = x,
    .p = ~ !is.character(.x),
    msg = cli::bg_red(cli::col_yellow("x doit etre au format caractere"))
  )
  attempt::stop_if(
    .x = x,
    .p = ~ !.x %in% c("aut", "com"),
    msg = cli::bg_red(cli::col_yellow("toto uniquement"))
  )

  tab2_12m <- bilan |>
    dplyr::filter(territoire %in% c("Corse", "France m\u00e9tro."), type %in% x) |>
    dplyr::select(dplyr::one_of(c(
      "type", "variable", "territoire", "value", "evol_trim"
    )))

  tab2 <- tab2_12m |>
    dplyr::filter(territoire %in% "Corse") |>
    dplyr::mutate(Evolution_FRm = tab2_12m$evol_trim[!tab2_12m$territoire %in% "Corse"])

  # rm(tab2_12m)
  filename <-
    here::here("4_resultats",
               params$annee_mois,
               "tableaux",
               paste0("tab2_", x, ".csv"))
  utils::write.csv2(tab2, filename, row.names = FALSE)

  # Mise en forme pour publication

  pl_tab2 <-
    tab2 |>
    dplyr::select(dplyr::one_of(c(
      "variable", "value", "evol_trim", "Evolution_FRm"
    ))) |>
    dplyr::left_join(df_codelgt, by = "variable") |>
    dplyr::select(libelle, value, evol_trim, Evolution_FRm) |>
    purrr::set_names(c("Logements", "Nombre", "Evolution", "Evolution FRm"))


  # controle sur la somme des types de logements


  eff <- pl_tab2[2, 2] + pl_tab2[3, 2] + pl_tab2[4, 2]
  if (eff < pl_tab2[1, 2]) {
    pl_tab2[2, 2] <- pl_tab2[2, 2] + 50
    pl_tab2[4, 2] <- pl_tab2[4, 2] + 50
  } else if (eff > pl_tab2[1, 2]) {
    pl_tab2[2, 2] <- pl_tab2[2, 2] - 50
    pl_tab2[4, 2] <- pl_tab2[4, 2] - 50
  }


  pl_tab2 |>
    dplyr::mutate(
      "Nombre" = format(Nombre, decimal.mark = ",", big.mark = " "),
      "Logements" = as.character(Logements)
    ) |>
    dplyr::mutate(dplyr::across(
      dplyr::contains("Evolution"),
      ~ paste0(.x * 100, "%") |> stringr::str_replace("\\.", ",")

    )) -> pl_tab2

  pl_tab2 |> dplyr::slice(0) -> eff

  fn_dfg <-
    function(df = eff,
             x = c(NA_character_, NA_character_, "Corse", "France m\u00e9tro.")) {
      for (i in 1:ncol(df))
        df[1, i] <- x[i]
      return(df)
    }
  fn_dfg(eff,
         c(NA_character_, NA_character_, "Corse", "France m\u00e9tro.")) -> eff
  pl_tab2 <- eff |> dplyr::bind_rows(pl_tab2)

  filename <-
    here::here("4_resultats",
               params$annee_mois,
               "tableaux",
               paste0("pl_tab2_", x, ".csv"))
  utils::write.csv2(pl_tab2, filename, row.names = FALSE)
  return(pl_tab2)
}
