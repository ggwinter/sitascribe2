#' fn11_evol1an_type_lgt_bonus1
#'
#' Une fois l analyse realisee via run all chunk
#' la fonction permet de creer si besoin un graphe interactif
#' pour aider a la redaction.
#'
#' @param x caractere aut ou com
#'
#' @importFrom attempt stop_if
#' @importFrom cli bg_red
#' @importFrom cli col_yellow
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr join_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr one_of
#' @importFrom dplyr select
#' @importFrom DT datatable
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_minimal
#' @importFrom here here
#' @importFrom plotly ggplotly
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#'
#' @return liste des graphiques et tableaux pour les logements autorises et commences
#' @export
#'
fn11_evol1an_type_lgt_bonus1 <- function(x = "aut") {

  attempt::stop_if(.x = x, .p = ~ !is.character(.x), msg = cli::bg_red(cli::col_yellow("x doit etre au format caractere")))
  attempt::stop_if(.x = x, .p = ~ !.x %in% c("aut", "com"), msg = cli::bg_red(cli::col_yellow("aut ou com uniquement")))

  tab3b <- bilan |>
    dplyr::filter(!variable %in% "log", type %in% x) |>
    dplyr::left_join(df_codelgt, by = "variable") |>
    dplyr::select(dplyr::one_of(c(
      "type", "geo", "territoire", "libelle", "trim", "trim_b"
    ))) |>
    tidyr::pivot_longer(
      cols = -c(type:libelle),
      names_to = "trimestre",
      values_to = "nombre"
    )

  tab3c <- bilan |>
    dplyr::filter(variable %in% "log", type %in% x) |>
    dplyr::left_join(df_codelgt, by = "variable") |>
    dplyr::select(dplyr::one_of(c("type", "geo", "territoire", "trim", "trim_b"))) |>
    tidyr::pivot_longer(
      cols = -c(type:territoire),
      names_to = "trimestre",
      values_to = "nombrec"
    )
  dplyr::inner_join(tab3b, tab3c, by = dplyr::join_by(type, geo, territoire, trimestre)) |>
    dplyr::mutate(part = 100 * round(nombre / nombrec, digits = 3)) -> tab3b

  rm(tab3c)
  tab3b |>
    dplyr::select(-nombrec) |>
    tidyr::pivot_longer(
      cols = -c(type:trimestre),
      names_to = "indicateur",
      values_to = "valeur"
    ) |>
    tidyr::pivot_wider(names_from = "trimestre", values_from = "valeur") |>
    dplyr::mutate(
      diff = round(trim - trim_b, 1),
      taux = 100 * round(diff / trim_b, 3),
      evolution = ifelse(taux > 0, "hausse", "baisse"),
      taux = dplyr::case_when(indicateur == "nombre"~taux,
                              TRUE~NA),
      trim = round(trim, 1),
      trim_b = round(trim_b, 1)
    ) -> tab3b

  ggplot2::ggplot(tab3b |> dplyr::filter(geo >= 2)) +
    ggplot2::geom_segment(ggplot2::aes(
      x = libelle,
      xend = libelle,
      y = trim_b,
      yend = trim,
      color = evolution
    )) +
    ggplot2::scale_colour_manual(values = c("hausse" = "green", "baisse" = "orange")) +
    ggplot2::geom_point(ggplot2::aes(x = libelle, y = trim_b), color = "forestgreen") +
    ggplot2::geom_point(ggplot2::aes(x = libelle, y = trim), color = "red") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::facet_wrap(territoire ~ indicateur, scales = "free_x", ncol = 2) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      title = "Evolution sur un an par type de logements",
      subtitle = "12 mois cumul\u00e9s au dernier trimestre / 12 mois un an avant",
      x = "Type de logement",
      y = "Nombre et part du nombre de logement",
      caption = "Dernier trimestre point rouge, trimestre pr\u00e9c\u00e9dent point vert\n Attention pour chaque trimestre cumul sur 12 mois"
    ) -> p


  filename <- here::here(
    "4_resultats",
    params$annee_mois,
    "images",
    paste0("type_lgt_evol12mois_", x, ".png")
  )
  ggplot2::ggsave(
    filename,
    width = 18,
    height = 16,
    unit = "cm",
    dpi = 300
  )
  plotly::ggplotly(p) -> p
  ls_result <-
    list(
      "tableau" = DT::datatable(
        tab3b,
        extensions = 'Buttons',
        options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'))
      ),
      "graphe" = p
    )
  return(ls_result)

}
