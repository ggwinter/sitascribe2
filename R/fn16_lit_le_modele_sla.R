#' fn16_lit_le_modele_sla
#'
#' Scribus : texte, lit la plaquette modele, supprime les lignes et en cree
#' autant de vides que dans le nouveau texte
#'
#' @param x date annee etude
#'
#' @return liste
#' @importFrom dplyr arrange
#' @importFrom dplyr case_when
#' @importFrom dplyr count
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr tibble
#' @importFrom here here
#' @importFrom purrr map_dbl
#' @importFrom purrr map_dfr
#' @importFrom purrr map2_dfr
#' @importFrom purrr set_names
#' @importFrom purrr walk2
#' @importFrom rvest html_element
#' @importFrom stringr str_pad
#' @importFrom stringr str_replace_all
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_add_child
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_attrs
#' @importFrom xml2 xml_find_all
#' @export
fn16_lit_le_modele_sla <- function(x = 'txt_plaquette') {
  ifelse(
    test = params$modele_plaquette == "oui",
    yes = pg <-
      xml2::read_xml(here::here("3_tables", chemin_modele), encoding = "UTF-8"),
    no = pg <-
      xml2::read_xml(plaquette_sitadel, encoding = "UTF-8")
  )

  # On cherche tous les PAGEOBJECT (objets) dans la page
  eff <- pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>% xml2::xml_attrs()



  # On supprime tous les PAGEOBJECT dont le champ ANNAME n est pas rempli.
  #
  which(is.na(pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>% xml2::xml_attr("ANNAME"))) -> node_vide

  if(length(node_vide)>0) eff[-c(node_vide)]-> eff


  eff |> purrr::set_names(paste0("O_", stringr::str_pad(string = seq_along(eff), side = "left", width = 2, pad = "0")))-> eff


  # Chaque objet est défini par son nom et un identifiant unique cree par Scribus
  # le PTYPE inclut le type d'objet tableau, image importée, texte
  # on y affecte un nouvel num_objet
  t_objets_numero <-
    purrr::map_dfr(eff, ~ .x[c("ANNAME", "ItemID", "PTYPE")],
                   .id = "num_objet") %>%
    dplyr::rename(c(nom_objet = "ANNAME", id_objet = "ItemID")) %>%
    dplyr::arrange(nom_objet) |>
    dplyr::mutate(PTYPE_LIB = dplyr::case_when(PTYPE %in% "2"~"image",
                                               PTYPE %in% "4"~"texte",
                                               PTYPE %in% "16"~"tableau",
                                               TRUE ~ "Autre"))



  # On filtre toutes les images de la plaquette
  # on extrait dans le code scribus la présence des PFILE lien vers l'image
  #
  t_images <- purrr::map_dfr(eff[t_objets_numero$num_objet[t_objets_numero$PTYPE_LIB %in% "image"]], \(x) x[c("ANNAME", "ItemID", "PFILE")])|>
    dplyr::rename(c(nom_objet = "ANNAME", id_objet = "ItemID")) %>%
    dplyr::mutate(
      new_value = stringr::str_replace_all(PFILE, "../4_resultats/00_logo", "../00_logo") |>
        stringr::str_replace_all("../4_resultats/202202", "."),
      attr = "PFILE"
    ) %>% dplyr::arrange(nom_objet)

  # On cherche à identifier toutes les paragraphes de la plaquette
  # sol 1 par construction le nom de l'objet doit comporter un terme generique
  t_paragraphes <-
    t_objets_numero %>% dplyr::filter(PTYPE_LIB %in% "texte")

  # On cherche à identifier toutes les tableaux de la plaquette
  # sol 1 par construction le nom de l'objet doit comporter un terme generique
  t_tableaux <-
    t_objets_numero %>% dplyr::filter(PTYPE_LIB %in% "tableau")


  # eff <-
  #   pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>% xml2::xml_attrs()

  # on ajoute ala table des tableaux le nbre de cellules, de colonnes et de lignes

  fn_tableaux_infos <- function(x = "203195824") {
    ph <- pg %>% rvest::html_element(paste0("PAGEOBJECT[ItemID='", x, "']"))
    t_tableaux_comp <- dplyr::tibble(
      id_objet = x,
      nb_cell = ph %>%
        xml2::xml_find_all("TableData") %>% xml2::xml_find_all("Cell") %>%
        length(),
      nb_lgn = ph %>% xml2::xml_attr("Rows") %>%
        as.integer(),
      nb_col = ph %>% xml2::xml_attr("Columns") %>%
        as.integer()
    )
    return(t_tableaux_comp)
  }
  eff3 <- purrr::map_dfr(t_tableaux$id_objet, fn_tableaux_infos)

  t_tableaux <-
    t_tableaux %>% dplyr::inner_join(eff3, by = "id_objet")

  # creation d'une table avec toutes les cellules de tous les tableaux
  # on cree un identifiant unique pour chaque cellule du tableau id_objet + num_cell
  #
  t_tableaux_cell <-
    purrr::map2_dfr(
      t_tableaux$id_objet,
      t_tableaux$nb_cell,
      ~ dplyr::tibble(id_objet = .x, num_cell = 1:.y)
    )

  # on ajoute le numero de la ligne et de la colonne
  # !!! attention scribus les compte a partir de 0
  #
  fn_tableaux_lit_cell <- function(x = "203195824", y = 2) {
    ph <- pg %>% rvest::html_element(paste0("PAGEOBJECT[ItemID='", x, "']"))
    df <- dplyr::tibble(
      id_objet = x,
      num_cell = y,
      lgn = ph %>%
        xml2::xml_find_all("TableData") %>% xml2::xml_find_all("Cell") %>%
        .[[y]] %>% xml2::xml_attr("Row") %>% as.integer(),
      col = ph %>% xml2::xml_find_all("TableData") %>%
        xml2::xml_find_all("Cell") %>% .[[y]] %>% xml2::xml_attr("Column") %>%
        as.integer()
    )
    return(df)
  }
  t_tableaux_cell <- purrr::map2_dfr(t_tableaux_cell$id_objet,
                                     t_tableaux_cell$num_cell,
                                     fn_tableaux_lit_cell)

  # On cherche les cellules vides decorations
  # si il y en a il faut ajouter un node ITEXT dans le code SCRIBUS
  #
  fn_tableaux_trouve_cell_vides <- function(x = "203195824") {
    ph <- pg %>% rvest::html_element(paste0("PAGEOBJECT[ItemID='", x, "']"))
    df <-
      dplyr::tibble(id_objet = x,
                    cell_vides = which(
                      purrr::map_dbl(
                        .x = x,
                        .f = ~
                          ph %>% xml2::xml_find_all("TableData") %>%
                          xml2::xml_find_all("Cell") %>% xml2::xml_find_all("StoryText") %>%
                          xml2::xml_find_all("ITEXT") %>% length()
                      ) %in% "0"
                    ))
    return(df)
  }
  t_tableaux_cell_vides <- purrr::map_dfr(t_tableaux_cell$id_objet,
                                          fn_tableaux_trouve_cell_vides)


  x <- 20
  y <- 1


  fn_remplie_tableau_node_itext <- function(x = "203195824",
                                            y) {
    xml2::xml_add_child(
      pg %>% rvest::html_element(paste0("PAGEOBJECT[ItemID='",
                                        x, "']")) %>% xml2::xml_find_all("TableData") %>%
        xml2::xml_find_all("Cell") %>% .[[y]] %>% xml2::xml_find_all("StoryText"),
      xml2::read_xml("<ITEXT CH=''/>")
    )
  }
  if (nrow(t_tableaux_cell_vides) > 0) {
    purrr::walk2(
      t_tableaux_cell_vides$id_objet,
      t_tableaux_cell_vides$cell_vides,
      fn_remplie_tableau_node_itext
    )
  }

  # On cherche le contenu de toutes les cellules
  #
  fn_tableaux_lit_cell_txt <- function(x = "203195824", y = 2) {
    df <- dplyr::tibble(
      id_objet = x,
      num_cell = y,
      txt = pg %>%
        rvest::html_element(paste0("PAGEOBJECT[ItemID='",
                                   x, "']")) %>% xml2::xml_find_all("TableData") %>%
        xml2::xml_find_all("Cell") %>% .[[y]] %>% xml2::xml_find_all("StoryText") %>%
        xml2::xml_find_all("ITEXT") %>% xml2::xml_attr("CH"),
    )
    return(df)
  }
  t_tableaux_cell_txt <- purrr::map2_dfr(t_tableaux_cell$id_objet,
                                         t_tableaux_cell$num_cell,
                                         fn_tableaux_lit_cell_txt)
  t_tableaux_cell <-
    t_tableaux_cell %>% dplyr::left_join(t_tableaux_cell_txt,
                                         by = c("id_objet", "num_cell"))
  t_tableaux_nb_col_lgn <-
    t_tableaux_cell %>% dplyr::filter(!lgn %in% 0) %>%
    dplyr::count(id_objet, name = "nbp")
  # %>%
  # dplyr::mutate(noms_objets2 = c("tab1", "tab2", "tab3", "tab4"))


  ls_modele <- list(
    pg = pg,
    t_objets_numero = t_objets_numero,
    t_images = t_images,
    t_paragraphes = t_paragraphes,
    t_tableaux = t_tableaux,
    t_tableaux_cell = t_tableaux_cell,
    t_tableaux_nb_col_lgn = t_tableaux_nb_col_lgn
  )
  return(ls_modele)

}
