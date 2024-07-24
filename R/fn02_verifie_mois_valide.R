#' fn02_verifie_mois_valide
#'
#' Vérifie que la date entrée dans les paramètres correspond bien a un tableau présent dans 2_data
#'
#' @param x caractere au format moisannee ex 202202
#'
#' @return nothing
#' @importFrom attempt stop_if
#' @importFrom cli bg_green
#' @importFrom cli bg_red
#' @importFrom cli col_yellow
#' @importFrom cli col_black
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr tibble
#' @importFrom here here
#' @importFrom stringr str_extract
#' @importFrom stringr str_sub
#' @export
fn02_verifie_mois_valide <- function(x = params$annee_mois){

  # liste les fichiers excel dans 2_data

  fl <- list.files(here::here("2_data"), pattern = "^ROES", full.names = FALSE) |>
    stringr::str_extract("[:digit:]{6}")

  error_data_fichier_nom_present <- function(x) {
    !x %in% fl
  }

  attempt::stop_if(
    .x = x,
    .p = error_data_fichier_nom_present,
    msg = cli::bg_red(cli::col_yellow("Data PB : Aucun fichier present pour ce mois"))
  )
  cat(cli::bg_green(cli::col_black("Data ok : le fichier est present pour ce mois")))


  # calcul de la plage temporelle d analyse de la publication

  annee <- as.numeric(substr(x, 1, 4))

  annee_nm1 <- annee - 1

  mois_nm1 <- paste0(annee - 1, substr(x, 5, 6))




  # t_mois_lib
  t_mois_lib <- df_moislib |> dplyr::filter(date <= x)


  # liste_mois_trim
  t_mois_data <- t_mois_lib |> dplyr::filter(date %in% x)

  liste_mois_tlong <- t_mois_lib |> dplyr::pull(date)


  a <- length(liste_mois_tlong)

  liste_mois_tlong <-
    liste_mois_tlong[c(seq(a - 11, a), seq(a - 23, a - 12))]

  eff <- seq(length(liste_mois_tlong), 1, by = -3)
  eff <- eff[c(1:3, 5:7)]

  liste_mois_trim <- sort(liste_mois_tlong)
  liste_mois_trim <- liste_mois_trim[eff]

  # liste_mois moisdata et mois an moins un

  liste_mois <- c(x, mois_nm1)

  ls_dates <- list("liste_mois_trim" = liste_mois_trim,
                   "liste_mois" = liste_mois,
                   "liste_mois_tlong" = liste_mois_tlong,
                   "annee" = annee,
                   "annee_nm1" = annee_nm1
  )
  return(ls_dates)

}
