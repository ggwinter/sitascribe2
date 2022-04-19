#' fn06_graphe_mensuel_np
#'
#' @param x charactere aut ou com
#'
#' @return graphe
#' @importFrom cli bg_red
#' @importFrom cli col_yellow
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom purrr set_names
#' @importFrom utils write.csv2
#' @export
fn06_graphe_mensuel_np <- function(x = "aut") {
  attempt::stop_if(.x = x,
                   .p = ~!is.character(.x),
                   msg = cli::bg_red(cli::col_yellow("x doit etre au format caractere")))
  attempt::stop_if(.x = x,
                   .p = ~!.x %in% c("aut", "com"),
                   msg = cli::bg_red(cli::col_yellow("x doit etre eagl a aut ou com")))
  # graphique mensuel
  #
  lib <-dplyr::if_else(condition = x %in% "aut",
                       "autoris\u00e9s",
                       "commenc\u00e9s")

  p <- lsm$NEW_REG %>%
    dplyr::filter(terr_cd %in% "94", type %in% x) %>%
    dplyr::mutate(Mois =
                    paste("01",
                          substr(date, 5, 6),
                          substr(date, 3, 4),
                          sep = "/") %>%
                    lubridate::dmy()) %>%
    ggplot2::ggplot(ggplot2::aes(x = Mois, y = log)) +
    ggplot2::geom_line() +
    # scale_x_date(labels = date_format("%Y-%m"),
    #              breaks = graduation) +
    ggplot2::scale_y_continuous(name = "Nombre de logements") +
    ggplot2::theme_bw() +
    ggplot2::labs(title = paste0("Nombre de logements ", lib, " estim\u00e9s par mois"),
                  subtitle = "Non publiable")

  return(p)

}
