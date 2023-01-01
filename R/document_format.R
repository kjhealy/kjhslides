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


## Document format for slides

#' kjh Slide Document Format. Local version.
#'
#' A wrapper for `xaringan::moon_reader()` that sets various options and support file locations.
#' This version of the reader assumes all the files are local (i.e. in the top level of slides/)
#'
#' @param ... Args to be passed on to xaringan::moon_reader
#'
#' @return Document format
#' @export
kjh_local_slides_reader <- function(...) {
  base_path <- paste0(system.file("slides", package = "kjhslides"), "/")

  css_files <- stringr::str_remove(kjhslides::kjh_css_files(), base_path)
  js_files <- stringr::str_remove(kjhslides::kjh_js_files(), base_path)
  chakra_file <- stringr::str_remove(kjhslides::kjh_remark_js(), base_path)

  xaringan::moon_reader(
    lib_dir = "libs", # this is local to the document not the package
    css = css_files,
    chakra = chakra_file,
    seal = FALSE,
    anchor_sections = FALSE,
    nature = list(
      beforeInit = js_files,
      highlightStyle = "default",
      highlightLines = TRUE,
      countIncrementalSlides = FALSE,
      slideNumberFormat = "%current%",
      ratio = "16:9",
      navigation = list(scroll = FALSE)
    )
  )
}


