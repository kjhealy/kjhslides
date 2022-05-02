## Chunk processing


# kjh_unname_chunk <- function (path, chunk_name_prefix = NULL)
# {
#   lines <- readLines(path)
#   chunk_headers_info <- get_chunk_info(lines)
#   if (is.null(chunk_headers_info)) {
#     return(invisible("TRUE"))
#   }
#   if (is.null(chunk_name_prefix)) {
#     chunk_headers_info$name[chunk_headers_info$name != "setup"] <- ""
#   }
#   else {
#     del_labels <- strtrim(chunk_headers_info$name, nchar(chunk_name_prefix)) %in%
#       chunk_name_prefix
#     setup_label <- !(chunk_headers_info$name %in% "setup")
#     del_labels <- del_labels & setup_label
#     chunk_headers_info$name[del_labels] <- ""
#   }
#   newlines <- re_write_headers(chunk_headers_info)
#   lines[newlines$index] <- newlines$line
#   writeLines(lines, path)
# }

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

