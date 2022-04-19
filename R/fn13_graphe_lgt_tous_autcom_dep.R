#' fn13_graphe_lgt_tous_autcom_dep
#'
#' @param data liste
#'
#' @return nothing
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr one_of
#' @importFrom dplyr select
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 scale_x_date
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom here here
#' @importFrom purrr map_dfr
#' @importFrom scales date_format
#' @importFrom stats complete.cases
#' @export
fn13_graphe_lgt_tous_autcom_dep <- function(data = lsm_12m0$DPT) {
  # 2A et 2B commences autorises sur le même tableau graphe page 4
  limite <- as.character(as.numeric(params$annee_mois) - 1000)
  data %>%
    dplyr::filter(stats::complete.cases(log),
                  terr_cd %in% c("02A", "02B"),
                  date >= limite) %>%
    dplyr::select(dplyr::all_of(c(
      "terr_cd", "territoire", "date", "log", "type"
    ))) %>%
    dplyr::mutate(
      Mois =
        paste("01",
              substr(date, 5, 6),
              substr(date, 3, 4),
              sep = "/") %>% lubridate::dmy(.),
      type = dplyr::case_when(
        type %in% "aut" ~ "autoris\u00e9s",
        type %in% "com" ~ "commenc\u00e9s"
      )
    ) -> sit_drp_autcom_dptcor

  maxvalue <- max(sit_drp_autcom_dptcor$log)
  maxvalue <- 1000 * (floor(maxvalue / 1000) + 1)
  sit_drp_autcom_dptcor$Mois%>% sort() %>% unique()-> touslesmois
  graduation <-touslesmois[seq(1, length(touslesmois), 6)]

  p <- ggplot2::ggplot(
    sit_drp_autcom_dptcor,
    ggplot2::aes(x = Mois, y = log, group = type)
  ) +
    ggplot2::geom_line(ggplot2::aes(linetype = type, color = type), size = 1.1) +
    ggplot2::scale_colour_manual(values = df_palettecouleur$pal_2col %>% unlist() %>% unname()) +
    ggplot2::scale_linetype_manual(values = c("solid", "twodash")) +
    ggplot2::facet_wrap(~territoire) +
    ggplot2::scale_x_date(
      labels = scales::date_format("%Y-%m"),
      breaks = graduation
    ) +
    ggplot2::scale_y_continuous(
      name = "Nombre cumul\u00e9 sur 12 mois \n des logements",
      limits = c(0, maxvalue),
      breaks = seq(0, maxvalue, 1000)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = 9,
        angle = 90,
        hjust = 1,
        vjust = 0
      ),
      # reglage des legendes des graduations de x
      axis.title.x = ggplot2::element_text(size = 5, colour = "white"),
      # reglage de la legende du titre de de x (designe par name)
      axis.text.y = ggplot2::element_text(size = 10),
      axis.title.y = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_text(colour = "white"),
      # reglage du titre de la legende
      # plot.title=element_text(size=14,face="bold"),
      strip.background = ggplot2::element_rect(fill = "#ECD7A2"),
      legend.background = ggplot2::element_rect(fill = "transparent"),
      legend.key = ggplot2::element_rect(
        fill = "transparent",
        color = "transparent"
      ),
      legend.position = c(1.0, 0.6),
      legend.justification = c(1.2, 0)
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(title = NULL),
      linetype = ggplot2::guide_legend(title = NULL)
    )

  p

  filename <- here::here("4_resultats", params$annee_mois, "images", "sit_drp_autcom_dptcor.png")
  ggplot2::ggsave(
    filename,
    width = 18.46,
    height = 6.59,
    unit = "cm",
    dpi = 300
  )
  rm(maxvalue)
  return(p)
}
