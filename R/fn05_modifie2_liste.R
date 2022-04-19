#' fn05_modifie2_liste
#'
#' modifie liste valeurs moyennes en liste valeurs moyennes arrondies.
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

