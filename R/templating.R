## Slide templating
## Template, document format, and support files to make things self-contained.


#' Path to the kjhslides package
#'
#' @param libpath The path to slides support files
#' @param subpath Desired subfolder if any
#' @param file The file you want
#'
#' @return A file path
#' @export
#'
kjh_resource_file <- function(file,
                              subdir = NULL,
                              libpath = "slides") {
  if(is.null(subdir)) {
    system.file(paste0(libpath, "/", file), package = "kjhslides")
  } else {
    system.file(paste0(libpath, "/", subdir, "/", file), package = "kjhslides")
  }
}


#' Construct vector of CSS file paths
#'
#' @param file Vector of custom CSS files
#' @param path Where the files are in the package
#'
#' @return Vector of css files to use for the css: arg in moon_reader
#' @export
#'
kjh_css_files <- function(css_files =
                            c("kjh-slides.css",
                              "tenso-berkeley.css",
                              "animate.css",
                              "widths.css"),
                          css_dir = "css") {

  files <- purrr::map_chr(css_files, ~ kjh_resource_file(.x, subdir = css_dir))
  c("default", files)
}

#' Path to js file
#'
#' @param js_file Javascript file
#' @param subdir JS file directory
#'
#' @return Filepath to js file
#' @export
#'
kjh_js_files <- function(js_file = "kjh-macros.js", subdir = "js") {
  kjh_resource_file(js_file, subdir = subdir)
}

#' The basic lib dir
#'
#' @param libdir Location of the libs directory
#'
#' @return path to libs
#' @export
#'
kjh_lib_dir <- function(libdir = "slides/libs") {
  system.file(libdir, package = "kjhslides")
}

#' Path to remark.js
#'
#' @param remark Local path to remark in package inst files
#'
#' @return Absolute path to remark within slides
#' @export
kjh_remark_js <- function(remark = "libs/remark-latest.min.js") {
  kjh_resource_file(remark)
}





