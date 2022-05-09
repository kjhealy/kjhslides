## Chunk processing
#' Which chunks are safe to exclude
#'
#' Check to see which chunks do not have include = FALSE
#'
#' @param chunk_options Chunk options vector
#'
#' @return Logical vector of chunks that can be safely unnamed
#' @details We want to make sure chunks invovled in flipbookr remain named,
#' so we don't re-name them and then break the label that allows flipbooks to be constructed.
#'
check_chunk_include <- function(chunk_options = chunk_options) {
  x <- stringr::str_remove_all(chunk_options, " ")
  ## Chunks not safe to unname
  stringr::str_detect(x, "include=FALSE")
}

#' Unname all unnameable chunks
#'
#' Unname chunks except 'setup' and flipbookr chunks that have include=FALSE
#'
#' @param path Path to Rmd file
#' @param chunk_name_prefix Character string with prefix of chunknames that will be removed.
#' Default: NULL (indicating all chunknames will be removed except the one named 'setup' and
#' any chunks with the include = FALSE option, to protect flipbookr references.)
#'
#' @return Unnames chunks, except setup and include=FALSE chunks
#' @export
#'
kjh_unname_chunks <- function (path, chunk_name_prefix = NULL) {

  lines <- readLines(path)
  chunk_headers_info <- namer:::get_chunk_info(lines)

  chunk_options <- chunk_headers_info$options
  check_include <- check_chunk_include(chunk_options)

  if (is.null(chunk_headers_info)) {
    return(invisible("TRUE"))
  }
  if (is.null(chunk_name_prefix)) {
    ind <- !((chunk_headers_info$name %in% "setup") | check_include)
    chunk_headers_info$name[ind] <- ""
  }
  else {
    del_labels <- strtrim(chunk_headers_info$name,
                          nchar(chunk_name_prefix)) %in% chunk_name_prefix

    setup_label <- !(chunk_headers_info$name %in% c("setup") |
                     check_include)
    del_labels <- del_labels & setup_label
    chunk_headers_info$name[del_labels] <- ""
  }
  newlines <- namer:::re_write_headers(chunk_headers_info)
  lines[newlines$index] <- newlines$line
  writeLines(lines, path)
}

#' Unname all chunks
#'
#' Unname all Rmd chunks in the Rmd folders
#'
#' @param indir The input directory, default "slides"
#'
#' @details All the Rmd files with their chunk names removed, except chunks labeled
#' 'setup' and chunks where include=FALSE, to protect chunk_reveal calls.
#' Recurses 1 level (i.e. subdirs) by default.
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

  purrr:::walk(fnames, kjh_unname_chunks)
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

