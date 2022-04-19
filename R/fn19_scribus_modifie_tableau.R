#' fn19_scribus_modifie_tableau
#'
#' scribus, Tableaux, ajoute les valeurs issues des calculs dans les tableaux
#'
#' @param x id_objet caractere
#' @param y num_cell numerique
#' @param z texte
#'
#' @return nothing
#' @importFrom purrr walk
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_add_child
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_set_attr
#' @export
fn19_scribus_modifie_tableau <-
  function(x = "211587152" ,
           y = 8,
           z = "Corse-du-Sud") {
    purrr::walk(
      c(
        "LeftPadding",
        "RightPadding",
        "TopPadding",
        "BottomPadding"
      ),
      ~ xml2::xml_set_attr(fn_rc_po_no_td_allcell(x),
                           .x,
                           1)
    )


    if (fn_rc_po_no_td_numcell(x, y) %>%
        xml2::xml_find_all("StoryText")  %>%
        xml2::xml_find_all("trail") %>%
        xml2::xml_attr(., "PARENT") %>% is.na() == FALSE) {
      xml2::xml_set_attr(
        fn_rc_po_no_td_numcell(x, y) %>%
          xml2::xml_find_all("StoryText")  %>%
          xml2::xml_find_all("trail"),
        "PARENT",
        NULL
      )
    }

    purrr::walk(
      c(
        "TextDistLeft",
        "TextDistTop",
        "TextDistBottom",
        "TextDistRight",
        "TextVertAlign"
      ),
      ~ xml2::xml_set_attr(fn_rc_po_no_td_numcell(x, y),
                           .x,
                           "1")
    )


    # if (ls_modele$pg %>% xml_find_all(".//PAGEOBJECT") %>% .[[x]] %>%
    #     xml_find_all("TableData") %>%
    #     xml_find_all("Cell") %>% .[[y]] %>%
    #     xml_find_all("StoryText") %>%
    #     xml_find_all("DefaultStyle") %>%
    #     xml_attr("PARENT") %>% na.omit() %>% length() == 0) {
    xml2::xml_set_attr(
      fn_rc_po_no_td_numcell(x, y) %>%
        xml2::xml_find_all("StoryText") %>%
        xml2::xml_find_all("DefaultStyle"),
      "PARENT",
      "sty_tab_droite"
    )

    xml2::xml_set_attr(
      fn_rc_po_no_td_numcell(x, y) %>%
        xml2::xml_find_all("StoryText") %>%
        xml2::xml_find_all("DefaultStyle"),
      "CPARENT",
      "tab_normal"
    )
    # }

    if (fn_rc_po_no_td_allcell(x) %>%
        .[[y]] %>%
        xml2::xml_find_all("StoryText") %>%
        xml2::xml_find_all("ITEXT") %>% length() == 0) {
      xml2::xml_add_child(
        fn_rc_po_no_td_numcell(x, y) %>%
          xml2::xml_find_all("StoryText"),
        xml2::read_xml("<ITEXT CH=''/>")# complete les cellules sans itext
      )


    }
    xml2::xml_set_attr(
      fn_rc_po_no_td_numcell(x, y) %>%
        xml2::xml_find_all("StoryText") %>%
        xml2::xml_find_all("ITEXT"),
      "CH",
      txt_new
    )
  }



