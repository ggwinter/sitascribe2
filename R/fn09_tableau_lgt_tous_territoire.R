#' fn09_tableau_lgt_tous_territoire
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
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom here here
#' @importFrom purrr set_names
#' @importFrom stringr str_replace
#' @export
fn09_tableau_lgt_tous_territoire <- function(x = "aut") {
  attempt::stop_if(.x = x,
                   .p = ~!is.character(.x),
                   msg = cli::bg_red(cli::col_yellow("x doit etre au format caractere")))
  attempt::stop_if(.x = x,
                   .p = ~!.x %in% c("aut", "com"),
                   msg = cli::bg_red(cli::col_yellow("aut ou com uniquement")))

  tab1 <- bilan |>
    dplyr::filter(variable %in% "log", type %in% x, geo != 0) |>
    dplyr::select(type,
                  geo,
                  variable,
                  territoire,
                  value,
                  evol_trim,
                  evol_trim1,
                  evol_trim2) |>
    dplyr::arrange(dplyr::desc(geo)) |>
    dplyr::select(-geo) |>
    purrr::set_names("type",
                     "variable",
                     "territoire",
                     "nombre",
                     "evol_trim",
                     "evol_trim1",
                     "evol_trim2")


  filename <- here::here("4_resultats", params$annee_mois, "tableaux", paste0("tab1_", x, ".csv"))
  utils::write.csv2(tab1, filename,
                    row.names = FALSE)

  # Mise en forme pour publication

  liste_mois_lib <-
    df_moislib |>
    dplyr::filter(date %in% ls_dates$liste_mois_trim[1:3]) |>
    dplyr::pull("moislibl")
  liste_mois_lib <- paste("fin", liste_mois_lib[c(3, 2, 1)])

  pl_tab1 <- tab1 |> dplyr::select(territoire:evol_trim2)
  # rm(tab1_aut)


  # inserer le controle sur la somme des deux départements

  eff <- pl_tab1[1, 2] + pl_tab1[2, 2]
  if (eff < pl_tab1[3, 2]) {
    pl_tab1[1, 2] <- pl_tab1[1, 2] + 50
    pl_tab1[2, 2] <-
      pl_tab1[2, 2] + 50
  } else if (eff > pl_tab1[3, 2]) {
    pl_tab1[1, 2] <- pl_tab1[1, 2] - 50
    pl_tab1[2, 2] <- pl_tab1[2, 2] - 50
  }

  rm(eff)


  pl_tab1 |>
    dplyr::mutate(nombre = format(nombre,
                                  decimal.mark = ",",
                                  big.mark = " ")) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::contains("evol"),
        ~ paste0(.x * 100, "%") |> stringr::str_replace("\\.", ",")
      ),
      territoire = as.character(territoire)
    ) -> pl_tab1
  pl_tab1 |> dplyr::slice(0) -> eff

  fn_dfg <-
    function(df = eff,
             x = c(NA_character_, NA_character_, liste_mois_lib)) {
      for (i in 1:ncol(df))
        df[1, i] <- x[i]
      return(df)
    }
  fn_dfg(eff, c(NA_character_, NA_character_, liste_mois_lib)) -> eff
  pl_tab1 <- eff |> dplyr::bind_rows(pl_tab1)

  filename <- here::here("4_resultats", params$annee_mois, "tableaux", paste0("pl_tab1_", x, ".csv"))
  utils::write.csv2(pl_tab1, filename,
                    row.names = FALSE)

  return(pl_tab1)
}
