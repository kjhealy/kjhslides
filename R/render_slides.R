## Rendering slides: html -> pdf, purling, etc


#' Purl one Rmd file
#'
#' @param infile Absolute or relative Rmd file name
#' @param outdir Output directory, default 'code'
#'
#' @return purled file
#' @export
#'
kjh_purl_one_slide <- function(infile, outdir = "code") {

  infilepath <- fs::path_real(infile)
  outdirpath <- fs::path_real(outdir)

  if(!fs::file_exists(infilepath)) {
    stop("The input file does not exist.")
  }

  if(!fs::path_ext(infilepath) == "Rmd") {
    stop("The input file must be an Rmd file.")
  }


  outfilename <- fs::path_ext_set(fs::path_file(infilepath), "R")
  outfilepath <-  fs::path(outdirpath, outfilename)

  knitr::purl(input = infilepath,
              output = outfilepath,
              quiet = TRUE)

  }


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
kjh_purl_all_slides <- function(indir = "slides", outdir = "code") {

    fnames <- get_files_of_type(ftype = "*.Rmd",
                                indir = indir) |> dplyr::pull(inpath)

    purrr:::walk(fnames, kjh_purl_one_slide)
}

#' Render one slide deck from Rmd to HTML
#'
#' @param infile Relative or absolute path to Rmd input file
#' @param quietly Run rmarkdown::render() quietly
#'
#' @return Rendered HTML file
#' @export
#'
kjh_render_one_slide <- function(infile, quietly = TRUE, warnings = FALSE) {

  infilename <- fs::path_real(infile)
  #message("My infilename is ", infilename)

  if(!fs::file_exists(infilename)) {
    stop("The input file does not exist.")
  }

  if(!fs::path_ext(infilename) == "Rmd") {
    stop("The input file must be an Rmd file.")
  }

  outfilename <- fs::path_ext_set(infilename, "html")
  message("Making ", basename(outfilename))


  suppressMessages(
    rmarkdown::render(input = infilename,
                    output_file = outfilename,
                    quiet = quietly)
  )

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
    dplyr::pull(inpath)

  purrr:::walk(fnames, kjh_render_one_slide)
}


#' Convert an HTML slide deck to PDF
#'
#' Render one slide html file to pdf_slides/file.pdf with decktape
#'
#' @param infile Input html file
#' @param outdir Output directory, defaults to `pdf_slides/`
#' @param timeout Page load timeout passed to `decktape`
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
kjh_decktape_one_slide <- function(infile, outdir = "pdf_slides", timeout = 30000) {

  infilepath <- fs::path_real(infile)
  outdirpath <- fs::path_real(outdir)

  if(!fs::file_exists(infilepath)) {
    stop("The input file does not exist.")
  }

  if(!fs::path_ext(infilepath) == "html") {
    stop("The input file must be an HTML file.")
  }


  outfilename <- fs::path_ext_set(fs::path_file(infilepath), "pdf")
  outfilepath <-  fs::path(outdirpath, outfilename)
  message("My outfile path is ", outfilepath)


  xaringan::decktape(file = infilepath,
                     output = outfilepath,
                     args = paste0("--page-load-timeout=",
                                   timeout,
                                   " --chrome-arg=--allow-file-access-from-files"),
                     docker = FALSE)
}


#' Render every HTML file in the `slides/` folder to PDF and put it in `pdf_slides/`
#'
#' Render all html slide files in a folder (to depth 1) to pdf with decktape
#'
#' @param indir Input dir, defaults to 'slides'
#' @param outdir Output dir, defaults to 'pdf_slides'
#' @param timeout Page load timeout passed to `decktape`
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
kjh_decktape_all_slides <- function(indir = "slides", outdir = "pdf_slides", timeout = 30000) {

  fnames <- get_files_of_type(ftype = "*.html",
                              indir = indir) |> dplyr::pull(inpath)

  print(fnames)

  purrr:::walk(fnames, kjh_decktape_one_slide, timeout = timeout)
}
