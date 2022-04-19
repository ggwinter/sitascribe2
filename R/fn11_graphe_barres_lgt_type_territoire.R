#' fn11_graphe_barres_lgt_type_territoire
#'
#' @param x caractere aut ou com
#'
#' @return graphique
#' @importFrom cli bg_red
#' @importFrom cli col_yellow
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr one_of
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 position_stack
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom grid unit
#' @importFrom here here
#' @importFrom utils write.csv2
#' @export
fn11_graphe_barres_lgt_type_territoire <- function(x = "aut") {

  attempt::stop_if(.x = x, .p = ~ !is.character(.x), msg = cli::bg_red(cli::col_yellow("x doit etre au format caractere")))
  attempt::stop_if(.x = x, .p = ~ !.x %in% c("aut", "com"), msg = cli::bg_red(cli::col_yellow("aut ou com uniquement")))


  # tab3 pour graphique à barre page 2

  bilan %>%
    dplyr::filter(!variable %in% "log", type %in% x) %>%
    dplyr::left_join(df_codelgt, by = "variable") %>%
    dplyr::select(dplyr::one_of(c(
      "type", "variable", "territoire", "libelle", "trim"
    ))) %>%
    dplyr::rename("nb" = "trim") %>%
    dplyr::group_by(type, territoire) %>%
    dplyr::mutate(
      nbcum = sum(nb),
      taux = round(nb / nbcum, digits = 3),
      label_y = 100 - (100 * cumsum(taux)),
      tauxpc = paste0(round(taux * 100, digits = 1), "%") %>%
        stringr::str_replace("\\.", "\\,")
    ) -> tab3

  filename <-
    here::here(
      "4_resultats",
      params$annee_mois,
      "tableaux",
      paste0("tab3_", x, ".csv")
    )

  utils::write.csv2(tab3, filename,
    row.names = FALSE
  )

  # graphe
  #
  p <-
    ggplot2::ggplot(
      tab3,
      ggplot2::aes(x = territoire, y = taux * 100, fill = libelle)
    ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(
      ggplot2::aes(x = territoire, label = tauxpc),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 3.5
    ) +
    ggplot2::scale_y_continuous(name = "Taux") +
    ggplot2::scale_fill_manual(values = df_palettecouleur$pal_3col_2 %>% unlist() %>% unname()) +
    # ggplot2::scale_fill_manual(values = c("cadetblue3", "darkseagreen1", "gold")) +
    # coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = 10,
        hjust = 0.4,
        vjust = 0.5
      ),
      # reglage des legendes des graduations de x
      axis.title.x = ggplot2::element_blank(),
      # reglage de la legende du titre de de x (designe par name)
      axis.text.y = ggplot2::element_text(size = 9),
      axis.title.y = ggplot2::element_blank(),
      # legend.title = ggplot2::element_text(size = 1, color = "white"),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 9),
      # ,face="bold"),
      legend.background = ggplot2::element_rect(fill = "transparent"),
      legend.key = ggplot2::element_rect(
        fill = "transparent",
        color = "transparent"
      ),
      legend.position = "bottom"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 3))

  p <-
    p + ggplot2::theme(legend.key.size = grid::unit(0.3, "cm")) +
    ggplot2::guides(size = "none")

  p + ggplot2::coord_flip() -> p
  p

  filename <-
    here::here(
      "4_resultats",
      params$annee_mois,
      "images",
      paste0("type_lgt_12mois_", x, "_long.png")
    )

  ggplot2::ggsave(
    filename,
    width = 8.6,
    height = 9.5,
    unit = "cm",
    dpi = 300
  )

  return(p)
}
