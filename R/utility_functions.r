#' Unzip the slides folder
#'
#' Copy slide scaffolding in slides.zip to the slides folder
#'
#' @title setup_slides
#' @param folder The destination to copy to within the user's home.
#'     This must be supplied by the user.
#' @param zipfile The name of the bundled slide file.
#' @param slidefolder The name of the course packet folder to be created
#' @return The `zipfile` is copied to `folder` and its contents
#'     expanded into a directory, the `slidefolder`.
#' @details Transfers an included zip file from the package to `slides`.
#' @author Kieran Healy
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' setup_slides()
#' @export
setup_slides <- function(folder, zipfile = "slides.zip",
                         slidefolder = "slides",
                         destfolder = "slides") {
  if(missing(folder)) {
    stop("You must specify a destination for the notes, e.g., 'here::here()'")
  }

  file_name <- zipfile
  lib_loc <- fs::path_package("kjhslides")

  ## zipped version of slides is stored in package "resources" folder
  origin_path <- fs::path(lib_loc, "resources", file_name)
  dest_path <- fs::path_expand(folder)

  if(!fs::dir_exists(dest_path)) {
    stop(paste("Cannot copy notes to the folder", dest_path, "because it does not exist."))
  }

  fs::file_copy(origin_path, dest_path)

  dest_file <- fs::path(dest_path, file_name)
  fs::dir_create(dest_path, slidefolder)
  dest_dir_name <- fs::path(dest_path)

  utils::unzip(dest_file, exdir = dest_dir_name)

  # Remove zipfile
  fs::file_delete(dest_file)

  message(paste("Copied", file_name, "to", dest_path, "and expanded it into", dest_dir_name))

}

#' Check if in and out folders exist
#'
#' Support function for reading and writing files
#'
#' @param indir Input directory path
#' @param outdir Output directory path
#'
#' @return Stops if they don't
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
check_in_out <- function(indir, outdir) {
  if(!fs::dir_exists(indir)) {
    stop("The input directory does not exist.")
  }

  if(!fs::dir_exists(outdir)) {
    stop("The output directory does not exist.")
  }
}

#' Get vector of filepaths
#'
#' Get filepaths of given type, possibly recursively
#'
#' @param ftype Filetype glob, defaults to '*.Rmd'
#' @param indir Directory to begin search in
#' @param depth Recursion depth, defaults to 1 (ie inside subfolders)
#'
#' @return Vector of file paths
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
get_files_of_type <- function(ftype = "*.Rmd", indir, depth = 1){
  tibble::tibble(
    inpath =
      fs::dir_ls(
        path = here::here(indir),
        recurse = depth,
        glob = ftype
      ))
}

#' Purl all slide .Rmds to .R
#'
#' Convert all Rmd files in the slide folder to R files in the code folder
#'
#' @param indir The source dir, default slides, crawled recursively
#' @param outdir The output dir, default code
#'
#' @return Side effect; code/ folder of purled .R files.
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
kjh_purl_slides <- function(indir = "slides", outdir = "code") {

  check_in_out(indir = indir, outdir = outdir)

  fnames <- get_files_of_type(ftype = "*.Rmd",
                              indir = here::here(indir)) %>%
    dplyr::mutate(outname = paste0(tools::file_path_sans_ext(basename(inpath)), ".R"),
                  outpath = here::here(outdir, outname)) %>%
    dplyr::filter(outname != "00-slides.R")

  purrr:::walk2(fnames$inpath, fnames$outpath, knitr::purl)
}



#' Render all the slides
#'
#' Knit all files in the slides dir to HTML
#'
#' @param indir Input directory (default 'slides')
#'
#' @return Side effect; Renders all the Rmd files to html
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
kjh_render_all_slides <- function(indir = "slides") {

  if(!fs::dir_exists(indir)) {
    stop("The input directory does not exist.")
  }

  fnames <- get_files_of_type(ftype = "*.Rmd",
                              indir = here::here(indir)) %>%
    .[.!="00-slides.Rmd"]

  purrr:::walk(fnames, rmarkdown::render)
}


#' Convert an HTML slide deck to PDF
#'
#' Render one slide html file to pdf_slides/file.pdf with decktape
#'
#' @param infile Input html file
#' @param outdir Output directory, defaults to `pdf_slides`
#'
#' @return Side-effect; rendered PDF slide file
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
kjh_decktape_one <- function(infile, outdir = "pdf_slides") {

  if(!fs::file_exists(infile)) {
    stop("The input file does not exist.")
  }

  if(!fs::dir_exists(here::here(outdir))) {
    stop(paste("The output directory does not exist at", here::here()))
  }

  outfile <- here::here(outdir, paste0(tools::file_path_sans_ext(basename(infile)), ".pdf"))

  xaringan::decktape(infile, outfile, docker = FALSE)
}


#' Render everythign inthe `slides` folder to `pdf_slides`
#'
#' Render all html slide files in a folder (to depth 1) to pdf with decktape
#'
#' @param indir Input dir, defaults to 'slides'
#' @param outdir Output dir, defaults to 'pdf_slides'
#'
#' @return Side effect; renders all the html to pdf
#' @details By default will recurse to 1 level of subdirs
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
kjh_decktape_all <- function(indir = "slides", outdir = "pdf_slides") {

  fnames <- get_files_of_type(ftype = "*.html",
                              indir = here::here(indir))

  purrr:::walk(fnames, kjh_decktape_one)
}


#' Unname all chunks
#'
#' Unname all Rmd chunks in the Rmd folders
#'
#' @param indir The input directory, default "slides"
#'
#' @return All the Rmd files with their chunk names removed
#' @details Recurses 1 level (i.e. subdirs) by default. Right now this will break any references made with chunk_reveal!
#' @export
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
kjh_unname_all_chunks <- function(indir = "slides") {

  if(!fs::dir_exists(indir)) {
    stop("The input directory does not exist.")
  }

  fnames <- get_files_of_type(ftype = "*.Rmd",
                              indir = here::here(indir))

  fnames <- as.vector(dplyr::pull(fnames))

  purrr:::walk(fnames, namer::unname_chunks)
}


#' Rename all chunks
#'
#' Name all Rmd chunks in the Rmd folders
#'
#' @param indir The input directory, default "slides"
#'
#' @return All the Rmd files with their chunks renamed
#' @details Recurses 1 level (i.e. subdirs) by default. Right now this will break any references made with chunk_reveal!
#' @export
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
kjh_name_all_chunks <- function(indir = "slides") {

  if(!fs::dir_exists(indir)) {
    stop("The input directory does not exist.")
  }

  fnames <- get_files_of_type(ftype = "*.Rmd",
                              indir = here::here(indir))

  fnames <- as.vector(dplyr::pull(fnames))

  purrr:::walk(fnames, namer::name_chunks)
}


#' Convert all chunks
#'
#' Run unname and then rename on all chunks in all files
#'
#' @param indir Input directory, default "slides"
#'
#' @return All Rmd chunks are unnamed and then renamed
#' @details Recurses to 1 level of depth (i.e. subdirs).
#' Right now this will break any references made with chunk_reveal!
#'
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
kjh_update_all_chunk_names <- function(indir = "slides") {
  kjh_unname_all_chunks(indir = indir)
  kjh_name_all_chunks(indir = indir)
}


#' Clear the systemfonts registry
#'
#' @return Setting
#' @export
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
kjh_clear_registry <- function() {
  systemfonts::clear_registry()
}

#' Register Tenso font variant
#'
#' @return Tenso variant registered in systemfonts database
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
kjh_register_tenso <- function(){

  kjh_clear_registry()

  systemfonts::register_variant(
    name = "Tenso Slide",
    family = "Tenso Slab",
    weight = c("normal", "medium"),
    width = "normal"
  )
}


#' Turn on ragg
#'
#' Set graphics device to ragg PNG
#'
#' @param ... Passed on to `agg_png()`
#' @param res PNG resolution, defaults to 150
#'
#' @return Sets the device
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
ragg_png <- function(..., res = 150) {
  ragg::agg_png(..., res = res, units = "in")
}

#' Set knitr options
#'
#' Default knitr opts for chunks.
#'
#' @param warning knitr warnings FALSE
#' @param message knitr messages FALSE
#' @param fig.retina Retina 3
#' @param fig.align Alignment center
#' @param dev Default device is ragg_png
#'
#' @return Knitr options
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
kjh_set_knitr_opts <- function(warning = FALSE,
                               message = FALSE,
                               fig.retina = 3,
                               fig.align = "center",
                               dev = "ragg_png") {

  knitr::opts_chunk$set(warning = warning,
                        message = message,
                        fig.retina = fig.retina,
                        fig.align = fig.align,
                        dev = dev)
}



#' Slide theme
#'
#' Set up slides theme
#'
#' @return Environment vars set and theme set etc
#' @details Sets up the slide theme with Tenso font, Okabe-Ito colors, and element_markdown()
#' @param tenso Use theme_tenso or not?
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
kjh_set_slide_theme <- function(tenso = TRUE) {

  slide_colors <- c(
    `slate` = "#242E3D",
    `pink` = "#FC2290",
    `red` =  "#FB0307",
    `orange` =  "#EF9B2D",
    `yellow` =  "#FFFF54",
    `lightgrey` = "#F6F6F6",
    #  `grey` =  "#CBCBCB",
    `grey` =  "#E8E8E8",
    `darkgrey` =  "#C0C0C0",
    `blue` =  "#212E3E",
    `lblue` =  "#4EAFF0",
    `green` = "#1BB71C"
  )

  # Reordered Okabe-Ito
  slide_colors_opt <- unname(palette.colors()[c(2:4,6:8,5,1,9)])

  ## NB UK spelling of colour here
  options(ggplot2.discrete.colour = slide_colors_opt,
          ggplot2.discrete.fill = slide_colors_opt)


  theme_baselayer <- function (base_size = 14, base_family = "")
  {
    thm <- ggplot2::theme_grey(base_size = base_size, base_family = base_family)
    for (i in names(thm)) {
      if ("colour" %in% names(thm[[i]])) {
        thm[[i]]["colour"] <- list(NULL)
      }
      if ("fill" %in% names(thm[[i]])) {
        thm[[i]]["fill"] <- list(NULL)
      }
    }
    thm + ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA),
                         legend.background = ggplot2::element_rect(colour = NA),
                         line = ggplot2::element_line(colour = "black"),
                         rect = ggplot2::element_rect(fill = "white",
                                                      colour = "black"),
                         text = ggplot2::element_text(colour = "black"))
  }



  theme_tenso <- function (base_size = 12, base_family = "Tenso Slide") {
    (ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
       ggplot2::theme(line = ggplot2::element_line(colour = slide_colors["slate"]),
                      rect = ggplot2::element_rect(fill = slide_colors["lightgrey"],
                                                   linetype = 0, colour = NA),
                      text = ggplot2::element_text(colour = slide_colors["slate"]),
                      axis.title = ggplot2::element_text(ggplot2::rel(1.15)),
                      axis.text = ggplot2::element_text(size = ggplot2::rel(1.15)),
                      strip.text = ggplot2::element_text(size = ggplot2::rel(1.35),
                                                         face = "bold"),
                      axis.ticks = ggplot2::element_line(),
                      axis.line = ggplot2::element_line(),
                      legend.background = ggplot2::element_rect(),
                      legend.position = "top",
                      legend.direction = "horizontal",
                      legend.box = "vertical",
                      panel.grid = ggplot2::element_line(colour = NULL),
                      panel.grid.major = ggplot2::element_line(colour = slide_colors["grey"]),
                      panel.grid.minor = ggplot2::element_blank(),
                      plot.title = ggtext::element_markdown(hjust = 0,
                                                            size = ggplot2::rel(1.5),
                                                            face = "bold"),
                      plot.subtitle = ggtext::element_markdown(hjust = 0,
                                                               size = ggplot2::rel(1.25),
                                                               face = "plain"),
                      plot.caption = ggtext::element_markdown(hjust = 0,
                                                              size = ggplot2::rel(0.8),
                                                              face = "plain"),
                      plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
                      strip.background = ggplot2::element_rect()
       )
    )
  }

  ifelse(tenso == TRUE,
         ggplot2::theme_set(theme_tenso()),
         ggplkot2::theme_set(ggplot2::theme_gray())
         )

}

#' Xaringan Extra options
#'
#' Turn on various XE Options
#'
#' @return Slide setup
#' @details Sets Tile view, animate css, fade transition, use clipboard.
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
kjh_set_xaringan_opts <- function() {
  ## We have to wrap the options as a tagList, otherwise
  ## only the one executed last will be returned.
  htmltools::tagList(
    xaringanExtra::use_tile_view(),
    xaringanExtra::use_animate_css(),
    xaringanExtra::use_animate_all("fade"),
    xaringanExtra::use_clipboard())
  }

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

#' kjh Slide Document Format
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

