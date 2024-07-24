#' fn00_verifie_existe_modele
#'
#' @param x character oui ou non
#'
#' @return chemin vers le modele
#' @importFrom cli bg_green
#' @importFrom cli bg_red
#' @importFrom cli col_black
#' @importFrom cli col_yellow
#' @export
#'
fn00_verifie_existe_modele <- function(x = params) {

  # verifie que modele_plaquette est oui ou non
  stopifnot(params$modele_plaquette %in% c("oui", "non"))

  a <- character(0)
  # on verifie qu il y a au moins un fichier sla dans 3_tables
  # puis que son nom correspond

  if (params$modele_plaquette == "oui" &
      identical(a, list.files("./3_tables")) == FALSE) {
    if (any(grepl(pattern = params$modele_nom, list.files("./3_tables"))) == TRUE) {
      cat(cli::bg_green(cli::col_black(
        paste0(
          "Vous avez choisi comme mod\u00e8le le fichier : ",
          params$modele_nom," \n"
        )
      )))
      test <- "ok"
      nom_modele <- params$modele_nom
    } else{
      cat(cli::bg_red(
        cli::col_yellow(
          paste0( "Attention il n'y a pas de fichier ", params$modele_nom," dans 3_tables\n")
        )
      ))
      test <- "pb"
      nom_modele <- ""
    }

    stopifnot(test == "ok")
  }
  return(nom_modele)
}
