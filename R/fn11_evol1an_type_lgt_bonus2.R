#' fn11_evol1an_type_lgt_bonus2
#'
#' Analyse evolution sur un an pour tous les logements
#'
#' @param x caractere aut ou com
#'
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr one_of
#' @importFrom dplyr select
#' @importFrom DT datatable
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_minimal
#' @importFrom here here
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#'
#' @return liste
#' @export
#'
fn11_evol1an_type_lgt_bonus2 <- function(x = "aut") {
  tab3d <- bilan %>%
    dplyr::filter(variable %in% "log", type %in% x) %>%
    dplyr::left_join(df_codelgt, by = "variable") %>%
    dplyr::select(dplyr::one_of(c(
      "type", "territoire",
      "libelle", "trim", "trim_b"
    ))) %>%
    tidyr::pivot_longer(
      cols = -c(type:libelle),
      names_to = "trimestre",
      values_to = "valeur"
    ) |>
    dplyr::mutate(indicateur = "Nombre") |>
    tidyr::pivot_wider(names_from = "trimestre", values_from = "valeur") |>
    dplyr::mutate(
      diff = round(trim - trim_b,1),
      taux = 100 * round(diff / trim_b, 3),
      evolution = ifelse(test = taux > 0, "hausse", "baisse")
    )

  ggplot2::ggplot(tab3d |> dplyr::filter(!territoire %in% "France m\u00e9tro.")) +
    ggplot2::geom_segment(ggplot2::aes(
      x = territoire,
      xend = territoire,
      y = trim_b,
      yend = trim,
      color = evolution
    )) +
    ggplot2::scale_colour_manual(values = c("hausse" = "green", "baisse" = "orange")) +
    ggplot2::geom_point(ggplot2::aes(x = territoire, y = trim_b), color = "forestgreen") +
    ggplot2::geom_point(ggplot2::aes(x = territoire, y = trim), color = "red") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      title = "Evolution sur un an",
      subtitle = "12 mois cumul\u00e9s au dernier trimestre / 12 mois un an avant",
      x = "Type de logement",
      y = "Nombre et part du nombre de logements",
      caption = "Dernier trimestre point rouge, trimestre pr\u00e9c\u00e9dent point vert\n Attention pour chaque trimestre cumul sur 12 mois"
    ) -> p

  filename <- here::here(
    "4_resultats",
    params$annee_mois,
    "images",
    paste0("tous_lgt_evol12mois_", x, ".png")
  )
  ggplot2::ggsave(
    filename,
    width = 18,
    height = 16,
    unit = "cm",
    dpi = 300
  )

  ls_result <- list("tableau" = DT::datatable(tab3d),
                    "graphe" = p)
  return(ls_result)

}
