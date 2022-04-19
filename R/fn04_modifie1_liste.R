#' fn04_modifie1_liste
#'
#' modifie liste valeurs en liste valeurs moyennes non arrondies.
#'
#' @param data liste
#'
#' @return liste
#' @importFrom purrr map
#' @export
fn04_modifie1_liste <- function(data = lsm){
  purrr::map(.x = data, .f = uti_calcul_moyenne_12mois) -> lsm_12m0
  return(lsm_12m0)
}
