#' uti_txdiff_ligne
#'
#' @param x un vecteur numerique
#'
#' @return un vecteur numerique
#' @export
#'
#' @examples
#' uti_txdiff_ligne(c(1,2,3))
uti_txdiff_ligne <- function(x =c(1,2,3)) {
  return(round(c(NA, diff(x)) / c(NA, x[1:length(x) - 1]), digits = 4))
}
