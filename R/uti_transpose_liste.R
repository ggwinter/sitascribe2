#' uti_transpose_liste
#'
#' @param data liste
#'
#' @return liste
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @export
uti_transpose_liste <- function(data = lsm_12m) {
  list("FR" = data[grepl(pattern = "FR$", names(data)) == TRUE],
       "FRM" = data[grepl(pattern = "FRM", names(data)) == TRUE],
       "NEW_REG" = data[grepl(pattern = "NEW_REG", names(data)) == TRUE],
       "DPT" = data[grepl(pattern = "DPT", names(data)) == TRUE]) -> eff
  purrr::map(eff, ~ .x |> purrr::reduce(.f = dplyr::bind_rows)) -> ls_result
  return(ls_result)
}
