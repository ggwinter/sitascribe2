#' uti_calcul_arrondi_moyenne_12mois
#'
#' Compare la valeur arrondie a 100 du tous logements a la somme des valeurs
#' arrondies par type si c'est superieur a 0 on ajoute arbitrairement 50
#' logements a ip et colres,  methodologie locale non officielle.
#'
#' @param data df
#'
#' @return df
#' @importFrom dplyr across
#' @importFrom dplyr case_when
#' @importFrom dplyr contains
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom stats complete.cases
#' @export

uti_calcul_arrondi_moyenne_12mois <-
  function(data) {
    data |>
      dplyr::ungroup() |>
      dplyr::group_by(geo, type) |>
      dplyr::mutate(dplyr::across(dplyr::contains(c(
        "log", "ip", "ig", "colres"
      )),
      .fns = ~ round(.x, -2)
      )) |>
      dplyr::filter(stats::complete.cases(log)) |>
      dplyr::mutate(
        verif = ip + ig + colres,
        ip = dplyr::case_when(
          verif > log ~ ip - 50,
          verif == log ~ ip,
          verif < log ~ ip + 50
        ) |> as.integer(),
        colres = dplyr::case_when(
          verif > log ~ colres - 50,
          verif == log ~ colres,
          verif < log ~ colres + 50
        ) |> as.integer(),
        verif = NULL
      ) |>
      dplyr::ungroup()-> df

    return(df)
  }
