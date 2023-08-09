#' x_utilitaire_importScribus
#'
#' enregistre le modele scribus dans data
#'
#' @param x le chemin d'acces au modele de plaquette
#'
#' @return nothing
#' @importFrom usethis use_data
x_utilitaire_importScribus <- function(x) {
  # "C:/Users/HP-G5/Documents/R/sitascribe_test/3_tables/P_SCIL_SITADEL_2022trim1_t01.sla"
  readLines(file.path(x), encoding = "UTF-8")-> plaquette_sitadel
  paste(plaquette_sitadel, collapse = " ")-> plaquette_sitadel
  usethis::use_data(plaquette_sitadel, overwrite = TRUE)
}

