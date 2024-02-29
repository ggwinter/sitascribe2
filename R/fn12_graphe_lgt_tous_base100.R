#' fn12_graphe_lgt_tous_base100
#'
#' @param x caractere aut ou com
#'
#' @return graphe
#' @importFrom attempt stop_if
#' @importFrom cli bg_red
#' @importFrom cli col_yellow
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 scale_x_date
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom here here
#' @importFrom scales date_format
#' @importFrom stats complete.cases
#' @export
fn12_graphe_lgt_tous_base100 <- function(x = "aut") {
  attempt::stop_if(
    .x = x,
    .p = ~ !is.character(.x),
    msg = cli::bg_red(cli::col_yellow("x doit etre au format caractere"))
  )
  attempt::stop_if(
    .x = x,
    .p = ~ !.x %in% c("aut", "com"),
    msg = cli::bg_red(cli::col_yellow("aut ou com uniquement"))
  )

  limite <- as.character(as.numeric(params$annee_mois) - 1000)

  lsm_12m0$FR |>
    dplyr::select(type, territoire, date, log) |>
    dplyr::filter(date >= limite, type %in% x, stats::complete.cases(log)) -> b100_frm

  b100_frm$log[1] -> val_depart

  b100_frm |>
    dplyr::mutate(b100_log = round(log / val_depart * 100, digits = 2)) |>
    dplyr::select(-log) -> b100_frm



  lsm_12m0$NEW_REG |>
    dplyr::filter(terr_cd %in% "94", type %in% x , stats::complete.cases(log)) |>
    dplyr::select(type, territoire, date, log) |>
    dplyr::filter(date >= limite) -> b100_cor

  b100_cor$log[1] -> val_depart

  b100_cor |>
    dplyr::mutate(b100_log = round(log / val_depart * 100,
                                   digits = 2)) |>
    dplyr::select(-log) -> b100_cor


  b100_frm |>
    dplyr::bind_rows(b100_cor) |>
    dplyr::mutate(
      Mois = paste("01",
                   substr(date, 5, 6),
                   substr(date, 3, 4),
                   sep = "/") |> lubridate::dmy(),
      territoire = factor(territoire, levels = c("France m\u00e9tro.", "Corse"))
    ) -> b100_frmcor



  # rm(b100_frm, b100_cor)
  #
  b100_frmcor$b100_log |>
    range() |>
    (\(.) round(., -1))() -> val_range
  if ((val_range[1] / 10) %% 2 != 0) {
    val_range[1] <- val_range[1] - 10
  } else {
    val_range[1] <- val_range[1] - 20
  }
  if ((val_range[2] / 10) %% 2 != 0) {
    val_range[2] <- val_range[2] + 10
  } else {
    val_range[2] <- val_range[2] + 20
  }

  graduation <-
    b100_frmcor$Mois[seq(1, nrow(b100_frmcor) / 2, 6)]

  as.integer(ls_dates$liste_mois[1]) - 1000 -> eff
  paste0("(", paste(substr(eff, 1, 4), substr(eff, 5, 6), sep = "-"), ")") -> eff

  p <- ggplot2::ggplot(b100_frmcor,
                       ggplot2::aes(x = Mois, y = b100_log, group = territoire)) +
    ggplot2::geom_hline(yintercept = 100, color = "grey60") +
    ggplot2::geom_line(ggplot2::aes(linetype = territoire, color = territoire),
                       linewidth = 1.1) +
    ggplot2::scale_colour_manual(values = df_palettecouleur$pal_2col |> unlist() |> unname()) +
    ggplot2::scale_linetype_manual(values = c("twodash", "solid")) +
    ggplot2::scale_x_date(labels = scales::date_format("%Y-%m"),
                          breaks = graduation) +
    ggplot2::scale_y_continuous(
      name = paste0("valeur en base 100 :\n", eff),
      limits = val_range,
      breaks = seq(val_range[1], val_range[2], 20)
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
      legend.position.inside = c(0.25, 0),
      legend.justification.inside = c(1.2, 0)
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(title = NULL),
      linetype = ggplot2::guide_legend(title = NULL)
    )
  p

  filename <-
    here::here("4_resultats",
               params$annee_mois,
               "images",
               paste0("b100_frmcor_", x, ".png"))
  ggplot2::ggsave(
    filename,
    width = 19.36,
    height = 6.91,
    unit = "cm",
    dpi = 300
  )
  return(p)
}
