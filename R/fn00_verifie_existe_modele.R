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
fn00_verifie_existe_modele <- function(x = params$modele_plaquette) {
  chemin_modele <- vector(mode = "character")
  # verifie que modele_plaquette est oui ou non
  stopifnot(params$modele_plaquette %in% c("oui", "non"))

  # modele_plaquette oui    des fichiers dans 3_tables
  #     un seul fichier sla     c est celui que l\u0027on a choisi
  #     deux fichiers sla   pb

  a <- character(0)

  if (params$modele_plaquette == "oui" &
      identical(a, list.files("./3_tables")) == FALSE) {
    if (length(grepl(pattern = "sla$", list.files("./3_tables"))) == 1) {
      cat(cli::bg_green(cli::col_black(
        paste0(
          "Vous avez choisi comme mod\u00e8le le fichier : ",
          list.files("./3_tables", pattern = "sla$")
        )
      )))
      test <- "ok"
      chemin_modele <- list.files("./3_tables", pattern = "sla$")
    } else{
      cat(cli::bg_red(
        cli::col_yellow(
          "Attention il y a plusieurs fichiers sla dans 3_tables\nsupprimez ceux inutiles\n"
        )
      ))
      test <- "pb"
      chemin_modele <- ""
    }

    stopifnot(test == "ok")
  }
  return(chemin_modele)
}
