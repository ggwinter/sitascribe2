#' uti_transpose_liste
#'
#' @param data liste
#'
#' @return liste
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @importFrom stringr str_sub
#' @export
uti_transpose_liste <- function(data=lsm_12m) {
  list("FR" = data[stringr::str_sub(names(data),5,6)%in% "FR"],
       "NEW_REG" = data[stringr::str_sub(names(data),5,11)%in% "NEW_REG"],
       "DPT" = data[stringr::str_sub(names(data),5,7)%in% "DPT"]) -> eff
  purrr::map(eff, ~.x %>% purrr::reduce(.f = dplyr::bind_rows)) -> ls_result
  return(ls_result)
}
