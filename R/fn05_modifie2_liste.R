#' fn05_modifie2_liste
#'
#' Modifie la liste des valeurs cumulees sur 12 mois en liste des valeurs cumulees sur 12 mois arrondies.
#'
#' @param data liste
#'
#' @return liste
#' @importFrom purrr map
#' @export
#'
fn05_modifie2_liste <- function(data = lsm_12m0){
  purrr::map(.x = data, .f = uti_calcul_arrondi_moyenne_12mois) -> lsm_12m
  return(lsm_12m)
}

