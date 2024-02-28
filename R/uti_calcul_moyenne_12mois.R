#' uti_calcul_moyenne_12mois
#'
#' @param data dataframe
#'
#' @return dataframe
#' @importFrom dplyr across
#' @importFrom dplyr contains
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom RcppRoll roll_sum
#' @export
uti_calcul_moyenne_12mois <-
  function(data) {

    data |>
      dplyr::group_by(geo, territoire, terr_cd, type) |>
      dplyr::mutate(dplyr::across(
        dplyr::contains(c("log", "ip", "ig", "colres")),
        .fns = ~ RcppRoll::roll_sum(.x, 12, align = "right", fill = NA)
      )) |>
      dplyr::ungroup()-> df

    return(df)
  }
