#' fn07_graphe_lgt_type_evol_area
#'
#' Affiche un graphe surfacique
#'
#' @param x character aut ou com
#'
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr tibble
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_area
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_x_date
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_minimal
#' @importFrom here here
#' @importFrom lubridate days
#' @importFrom lubridate year
#' @importFrom lubridate years
#' @importFrom lubridate ymd
#' @importFrom scales label_date
#' @importFrom scales label_number
#' @importFrom tidyr pivot_longer
#'
#' @return graphe
#' @export
#'
fn07_graphe_lgt_type_evol_area <- function(x = "aut") {

  lsm_12m0$NEW_REG |>
    dplyr::filter(terr_cd %in% "94") |>
    dplyr::filter(complete.cases(log), type %in% x) |>
    dplyr::select(date, ip, ig, colres) |>
    tidyr::pivot_longer(cols = -date,
                        names_to = "type_lgt",
                        values_to =  "nombre") |>
    dplyr::mutate(date = paste(substr(date, 1 , 4), substr(date, 5 , 6), "01", sep =
                                 "-") |>
                    lubridate::ymd()) -> sdf


  date_min <- max(sdf$date) - lubridate::years(10)
  sdf |> dplyr::filter(date >= date_min) |>
    dplyr::group_by(date) |> dplyr::summarise(nombre = sum(nombre), .groups = "drop") -> sdf_plot
  max(sdf_plot$nombre %/% 100) * 100 + 200 -> max_y
  rm(sdf_plot)

  date_min_an <-
    dplyr::if_else(date_min != lubridate::ymd(paste0(lubridate::year(date_min), "-01-01")),
                   lubridate::ymd(paste0(lubridate::year(date_min) + 1, "-01-01")),
                   date_min)
  date_min_an2 <-
    lubridate::ymd(paste0(lubridate::year(date_min_an), "-12-31"))
  date_max <- max(sdf$date) + months(1) - lubridate::days(1)
  date_max_an <-
    lubridate::ymd(paste0(lubridate::year(date_max), "-01-01"))
  date_max_an2 <-
    lubridate::ymd(paste0(lubridate::year(date_max), "-12-31"))


  dates <-
    rep(c(
      seq(date_min_an, date_max_an, by = '2 year'),
      seq(date_min_an2, date_max_an2, by = '2 year')
    ) |> sort(), each = 2)

  valeurs <- rep(c(0, max_y, max_y, 0), time = length(dates) %/% 4)

  dplyr::tibble(date = dates,
                valeur = valeurs,
                type = "rect") -> df2

  df2$date[df2$date == date_max_an2] <- date_max

  rm(dates, valeurs)

  ggplot2::ggplot(sdf |> dplyr::filter(date >= date_min)) +
    ggplot2::geom_area(ggplot2::aes(x = date, y = nombre, fill = type_lgt)) +
    ggplot2::geom_area(
      mapping = ggplot2::aes(x = date, y = valeur),
      fill = "white",
      alpha = 0.2,
      data = df2
    ) +
    ggplot2::scale_x_date(
      limits = c(date_min, date_max),
      expand = c(0, 0),
      breaks = seq(date_min_an, date_max_an, by = '1 year'),
      # labels = lubridate::year(seq(date_min_an, date_max_an, by = '1 year'))
      labels = scales::label_date(format = "%Y")
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      limits = c(0 , max_y),
      labels = scales::label_number()
    ) +
    ggplot2::scale_fill_manual(values = c("#8A614B", "#FFE552", "#C3992A"),
                               labels = c(
                                 "Collectif & R\u00e9sidences",
                                 "Indiv. group\u00e9",
                                 "Indiv. pur"
                               )) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(hjust = 0.0, size = 12),
      panel.grid = ggplot2::element_line(color = "grey90"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.ontop = TRUE
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
    ggplot2::labs(y = "Cumul des 12 derniers mois")-> p
  p

  filename <- here::here("4_resultats", params$annee_mois, "images", paste0("cor_",x,"_long_area.png"))

  ggplot2::ggsave(
    filename,
    width = 19.36,
    height = 6.91,
    unit = "cm",
    dpi = 300
  )

  rm(sdf, df2)

  return(p)

}
