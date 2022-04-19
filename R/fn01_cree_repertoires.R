#' fn01_cree_repertoires
#' on cree les repertoires 2_data, 4_resultats dans le nouveau dossier s ils n existent pas.
#'
#' @param dossier le repertoire ou est le projet
#'
#' @return nothing
#' @importFrom here here
#' @importFrom cli bg_green
#' @importFrom cli col_black
#' @importFrom purrr walk
#' @export
fn01_cree_repertoires <- function(dossier = here::here()) {
  repertoires <- c(
    "1_scripts", "2_data", "3_tables",
    "4_resultats", paste0("4_resultats/", params$annee_mois),
    "4_resultats/00_logo",
    paste0("4_resultats/", params$annee_mois, "/tableaux"),
    paste0("4_resultats/", params$annee_mois, "/images"),
    paste0("4_resultats/", params$annee_mois, "/doc_travail"),
    "5_publications"
  )
  purrr::walk(repertoires, ~ if (dir.exists(here::here(
    dossier,
    .x
  )) == FALSE) {
    dir.create(here::here(dossier, .x))
  })
  cat(cli::bg_green(cli::col_black("Les repertoires ont \u00e9t\u00e9 cr\u00e9\u00e9s\n")))
}


