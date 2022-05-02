## Rendering slides: html -> pdf, purling, etc

#' Purl all slide .Rmds to .R and put them in the `code/` folder
#'
#' Convert all Rmd files in the slide folder to R files in the code folder
#'
#' @param indir The source dir, default slides, crawled recursively
#' @param outdir The output dir, default code
#'
#' @return Side effect; `code/` folder of purled .R files.
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
#' Render all Rmd files in the `slides/` dir to HTML
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
#' @param outdir Output directory, defaults to `pdf_slides/`
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


#' Render every HTML file in the `slides/` folder to PDF and put it in `pdf_slides/`
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
