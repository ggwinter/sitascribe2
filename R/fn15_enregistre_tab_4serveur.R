#' fn15_enregistre_tab_4serveur
#'
#' @param x caractere aut ou com
#'
#' @return nothing
#' @importFrom cli bg_red
#' @importFrom cli col_yellow
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom purrr set_names
#' @importFrom stringr str_sub
#' @importFrom utils write.csv2
#'
#' @export
fn15_enregistre_tab_4serveur <- function(x = "aut") {
  attempt::stop_if(.x = x, .p = ~ !is.character(.x), msg = cli::bg_red(cli::col_yellow("x doit etre au format caractere")))
  attempt::stop_if(.x = x, .p = ~ !.x %in% c("aut", "com"), msg = cli::bg_red(cli::col_yellow("aut ou com uniquement")))

  # tableaux serveur

  lsm_12m$NEW_REG %>%
    dplyr::filter(terr_cd %in% 94, type %in% x) %>%
    dplyr::ungroup() %>%
    dplyr::select(-geo, -type) %>%
    dplyr::mutate(annee = stringr::str_sub(date, 1, 4)) %>%
    dplyr::select(terr_cd, territoire, annee, date, log:colres) %>%
    purrr::set_names(
      c(
        "terr_cd",
        "territoire",
        "annee",
        "date",
        "log",
        "ip",
        "ig",
        "colres"
      )
    ) -> sit_drp_corse

  utils::write.csv2(sit_drp_corse,
                    here::here("4_resultats", params$annee_mois, "tableaux", paste0("Serv_Corse_type_", x,".csv")),
                    row.names = FALSE)


  lsm_12m$DPT %>%
    dplyr::filter(terr_cd %in% c("02A", "02B")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-geo, -type) %>%
    dplyr::mutate(annee = stringr::str_sub(date, 1, 4)) %>%
    dplyr::select(terr_cd, territoire, annee, date, log:colres) -> sit_drp_dptcorse

  utils::write.csv2(
    sit_drp_dptcorse,
    here::here("4_resultats", params$annee_mois, "tableaux", paste0("t_Corse_dpt_total_", x,".csv")),
    row.names = FALSE
  )
}
