#' fn07_graphe_lgt_type_evol
#'
#' @param x caractere aut ou com
#'
#' @return graphe
#' @importFrom cli bg_red
#' @importFrom cli col_yellow
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom here here
#' @importFrom scales label_number
#' @importFrom stats complete.cases
#' @importFrom tidyr pivot_longer
#' @export
fn07_graphe_lgt_type_evol <- function(x = "aut") {
  attempt::stop_if(.x = x,
                   .p = ~!is.character(.x),
                   msg = cli::bg_red(cli::col_yellow("x doit etre au format caractere")))
  attempt::stop_if(.x = x,
                   .p = ~!.x %in% c("aut", "com"),
                   msg = cli::bg_red(cli::col_yellow("x doit etre eagl a aut ou com")))

  # graphique
  #
  lib <-
    dplyr::if_else(condition = x %in% "aut", "autorises", "commences")


  lsm_12m0$NEW_REG %>%
    dplyr::filter(terr_cd %in% "94", type %in% x, stats::complete.cases(log)) %>%
    dplyr::mutate(Mois =
                    paste("01",
                          substr(date, 5, 6),
                          substr(date, 3, 4),
                          sep = "/") %>%
                    lubridate::dmy()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-type) %>%
    tidyr::pivot_longer(cols = log:colres,
                        names_to = "type",
                        values_to = "value") %>%
    dplyr::mutate(type = factor(type, c("log", "ip", "ig", "colres"))) -> cor_long

  val_max <-
    100 * (round(max(cor_long$value) / 100, digits = 0) + 2)

  ggplot2::ggplot(cor_long,
                  ggplot2::aes(
                    x = Mois,
                    y = value,
                    group = type,
                    colour = type
                  )) +
    ggplot2::geom_line(size = 1.2) +
    # scale_x_date(labels = date_format("%Y-%m"),
    #              breaks = graduation) +
    ggplot2::scale_y_continuous(
      name = "Nombre de logements",
      limits = c(0, val_max),
      breaks = seq(0, val_max, 1000),
      labels = scales::label_number(big.mark = " ")
    ) +
    ggplot2::scale_colour_manual(
      values = df_palettecouleur$pal_4col %>% unlist() %>% unname(),
      labels = c(
        "Total",
        "Indiv. pur",
        "Indiv. group\u00e9",
        "Collectif & R\u00e9sidences"
      )
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = 10,
        angle = 90,
        hjust = 1,
        vjust = 0
      ),
      # reglage des legendes des graduations de x
      axis.title.x = ggplot2::element_text(size = 8, colour = "white"),
      # reglage de la legende du titre de de x (designe par name)
      axis.text.y = ggplot2::element_text(size = 10),
      axis.title.y = ggplot2::element_text(size = 11),
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.background = ggplot2::element_rect(fill = "transparent"),
      legend.key = ggplot2::element_rect(fill = "transparent",
                                         color = "transparent"),
      legend.position = c(0.12, 0.48),
      legend.justification = c(0.5, 0)
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(title = NULL))-> p


  filename <- here::here("4_resultats", params$annee_mois, "images", paste0("cor_",x,"_long.png"))

  ggplot2::ggsave(
    filename,
    width = 19.36,
    height = 6.91,
    unit = "cm",
    dpi = 300
  )

  rm(val_max)
  return(p)
}
