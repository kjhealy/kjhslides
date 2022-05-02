## Document format for slides

#' kjh Slide Document Format.
#'
#' A wrapper for `xaringan::moon_reader()` that sets various options and support file locations.
#'
#' @param ... Args to be passed on to xaringan::moon_reader
#'
#' @return Document format
#' @export
kjh_slides_reader <- function(...) {
  xaringan::moon_reader(
    lib_dir = "libs", # this is local to the document not the package
    css = kjhslides::kjh_css_files(),
    chakra = kjhslides::kjh_remark_js(),
    seal = FALSE,
    anchor_sections = FALSE,
    nature = list(
      beforeInit = kjhslides::kjh_js_files(),
      highlightStyle = "default",
      highlightLines = TRUE,
      countIncrementalSlides = FALSE,
      slideNumberFormat = "%current%",
      ratio = "16:9",
      navigation = list(scroll = FALSE)
    )
  )
}

