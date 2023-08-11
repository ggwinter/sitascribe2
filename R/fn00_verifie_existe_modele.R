#' fn00_verifie_existe_modele
#'
#' @param x character oui ou non
#'
#' @return chemin vers le modele
#' @export
#'
fn00_verifie_existe_modele <- function(x = params$modele_plaquette) {
  chemin_modele <- vector(mode = "character")
  stopifnot(params$modele_plaquette %in% c("oui", "non"))
  if (params$modele_plaquette == "oui") {
    stopifnot(file.exists("./3_tables/modele_sitadel.sla"))
    "./3_tables/modele_sitadel.sla" -> chemin_modele
  }
  return(chemin_modele)
}
