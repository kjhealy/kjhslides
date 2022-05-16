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

#' Render one slide deck from Rmd to HTML
#'
#' @param infile Relative or absolute path to Rmd input file
#' @param quietly Run rmarkdown::render() quietly
#'
#' @return Rendered HTML file
#' @export
#'
kjh_render_one_slide <- function(infile, quietly = TRUE) {

  infilename <- fs::path_real(infile)
  #message("My infilename is ", infilename)

  if(!fs::file_exists(infilename)) {
    stop("The input file does not exist.")
  }

  if(!fs::path_ext(infilename) == "Rmd") {
    stop("The input file must be an Rmd file.")
  }

  outfilename <- fs::path_ext_set(infilename, "html")
  #message("My outfilename is ", outfilename)

  #rmarkdown::render(input = infilename,
  #                  output_file = outfilname,
  #                  quiet = quietly)

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
                              indir = fs::path_real((indir))) |>
    pull(inpath)

  purrr:::walk(fnames, kjh_render_one_slide)
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
kjh_decktape_one_slide <- function(infile, outdir = "pdf_slides") {

  infilepath <- fs::path_real(infile)
  message("My infilepath is ", infilepath)
  outdirpath <- fs::path_real(outdir)

  if(!fs::file_exists(infilepath)) {
    stop("The input file does not exist.")
  }

  if(!fs::path_ext(infilepath) == "html") {
    stop("The input file must be an HTML file.")
  }


  outfilename <- fs::path_ext_set(fs::path_file(infilepath), "pdf")
  message("My outfilename is ", outfilename)
  outfilepath <-  fs::path(outdirpath, outfilename)
  message("My outfile path is ", outfilepath)


  xaringan::decktape(file = infilepath,
                     output = outfilepath,
                     docker = FALSE)
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
kjh_decktape_all_slides <- function(indir = "slides", outdir = "pdf_slides") {

  fnames <- get_files_of_type(ftype = "*.html",
                              indir = indir) |>
    pull(inpath)

  purrr:::walk(fnames, kjh_decktape_one_slide)
}
