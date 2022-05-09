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
#' Since we now make the CSS, JS, and other components available via a md document format that wraps
#' Xaringan's `moon_reader`, and also provide a template that will create a directory etc,
#' we don't need this way of doing things anymore.
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



#' Register Myriad font variant
#'
#' @return Myriad variant registered in systemfonts database
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
kjh_register_myriad <- function(){

  kjh_clear_registry()

  systemfonts::register_variant(
    register_variant(
      name = "Myriad Pro SemiCondensed",
      family = "Myriad Pro",
      width = "semicondensed",
      weight = c("normal", "semibold"),
    )

  )
}



#' Turn on ragg
#'
#' Set graphics device to ragg PNG
#'
#' @param ... Passed on to `agg_png()`
#' @param res PNG resolution, defaults to 300
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
ragg_png <- function(..., res = 300) {
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

#' Turn on showtext
#'
#' @param dpi DPI for output (300 default is recommended)
#'
#' @return Turns on showtext
#' @export
#'
kjh_set_showtext <- function(dpi = 300) {
  showtext::showtext_opts(dpi = 300)
  showtext::showtext_auto()
}
