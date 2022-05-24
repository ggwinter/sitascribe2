#' fn14_carte_region_lgt_tous_evolution
#'
#' @param x caractere aut ou com
#'
#' @return graphique
#' @importFrom cli bg_red
#' @importFrom cli col_yellow
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr one_of
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom ggplot2 '%+replace%'
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 geom_sf_text
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_grey
#' @importFrom grid unit
#' @importFrom here here
#' @importFrom purrr set_names
#' @importFrom sf st_as_sf
#' @importFrom sf st_as_sfc
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
#' @export
fn14_carte_region_lgt_tous_evolution <- function(x = "aut") {

  attempt::stop_if(.x = x, .p = ~ !is.character(.x), msg = cli::bg_red(cli::col_yellow("x doit etre au format caractere")))
  attempt::stop_if(.x = x, .p = ~ !.x %in% c("aut", "com"), msg = cli::bg_red(cli::col_yellow("aut ou com uniquement")))

  # cartographie -

  # Theme sans quadrillage pour la cartographie
  '%+replace%'<- ggplot2::'%+replace%'
  theme_clean <- function(base_size = 12) {
    ggplot2::theme_grey(base_size) %+replace%
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        axis.ticks.length = grid::unit(0, "cm"),
        panel.spacing = grid::unit(0, "lines"),
        plot.margin = grid::unit(c(0, 0, 0, 0), "lines"),
        complete = TRUE,
        legend.title = ggplot2::element_blank()
      )
  }

  map_regshp <- map_regwkt %>%
    dplyr::mutate(geometry = sf::st_as_sfc(geom_wkt)) %>%
    dplyr::select(-geom_wkt) %>% sf::st_as_sf(., crs = 2154)

  # carto REG logements autorises


  sit_drp_regfrm <- lsm_12m0$NEW_REG %>%
    dplyr::filter(terr_cd > 10, type %in% x, date %in% c(params$annee_mois, ls_dates$liste_mois[2])) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::one_of(c("type", "terr_cd", "territoire", "date", "log"))) %>%
    tidyr::pivot_wider(names_from = date,
                       values_from = log) %>%
    purrr::set_names("type", "terr_cd", "territoire", "an_prec", "an") %>%
    dplyr::mutate(diff = round((an - an_prec) / an_prec, digits = 3))

  sit_drp_regfrm %>% dplyr::pull(diff) -> liste_valeurs_diff

  range(liste_valeurs_diff) -> range_valeurs_diff
  floor(10 * range_valeurs_diff[1]) / 10 -> val_min
  floor(10 * range_valeurs_diff[2]) / 10 + 0.1 -> val_max
  max(abs(val_min), val_max) -> val_max_echelle
  seq(-val_max_echelle, val_max_echelle, by = 0.1) -> echelle_range



  map_bidon <-
    tibble::tibble(
      "x" = sample(
        seq(663542 - 10000, 663542 + 10000, by = 1000),
        size = length(echelle_range),
        replace = FALSE
      ),
      "y" = sample(
        seq(6845569 - 10000, 6845569 + 10000, by = 1000),
        size = length(echelle_range),
        replace = FALSE
      ),
      "diff" = sample(
        echelle_range,
        size = length(echelle_range),
        replace = FALSE
      )
    )

  map_reg <-
    map_regshp %>% dplyr::left_join(sit_drp_regfrm,
                                    by = c("INSEE_REG" = "terr_cd"))

  p <- ggplot2::ggplot(map_reg, ggplot2::aes(fill = diff)) +
    ggplot2::geom_sf(lwd = 1, colour = "grey90") + # , colour = "red"
    ggplot2::geom_point(data = map_bidon, ggplot2::aes(x = x, y = y), alpha = 0) +
    ggplot2::scale_fill_gradient2(
      low = "red",
      mid = "white",
      high = "green4",
      midpoint = 0,
      space = "Lab",
      na.value = "grey50",
      aesthetics = c("fill", "colour"),
      breaks = echelle_range,
      labels = scales::percent(echelle_range)
    ) +
    theme_clean() +
    ggplot2::theme(
      legend.text = ggplot2::element_text(size = 9),
      legend.background = ggplot2::element_rect(fill = "transparent",
                                                color = "transparent"),
      #
      legend.key = ggplot2::element_rect(
        fill = "transparent",
        color = "transparent",
        size = 2
      ),
      legend.position = c(0.92, 0.60),
      # legend.justification = c(1.2, 0),
      legend.key.size = grid::unit(0.4, "cm")
    )


  p + ggplot2::geom_sf_text(ggplot2::aes(label = round(diff * 100, 2) %>%
                                           format(., decimal.mark = ",")), size = 5) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(size = 5), reverse = TRUE))-> p
  p

  filename <-here::here("4_resultats", params$annee_mois, "images", paste0("reg_log_12m_chiffres_", x, ".png"))

  ggplot2::ggsave(
    filename,
    width = 11.3,
    height = 10,
    unit = "cm",
    dpi = 300
  )

  rm(map_reg)
  return(p)

}

