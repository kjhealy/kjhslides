#' Copy slide infrastructure to a given folder
#'
#' Transfers a zip file containing course materials from the socviz
#'     library to the Desktop.
#' @title setup_slides
#' @param folder The destination to copy to within the user's home.
#'     This must be supplied by the user.
#' @param zipfile The name of the bundled slide file.
#' @param slidefolder The name of the course packet folder to be created
#' @return The `zipfile` is copied to `folder` and its contents
#'     expanded into a directory, the `slidefolder`.
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
    message("You must specify a destination for the notes, e.g., 'here::here()'")
  } else {
    file_name <- zipfile
    lib_loc <- fs::path_package("kjhslides")

    ## zipped version of slides is stored in package "resources" folder
    origin_path <- fs::path(lib_loc, "resources", file_name)
    dest_path <- fs::path_expand(folder)

    if(fs::dir_exists(dest_path)) {

      fs::file_copy(origin_path, dest_path)

      dest_file <- fs::path(dest_path, file_name)
      fs::dir_create(dest_path, slidefolder)
      dest_dir_name <- fs::path(dest_path)

      utils::unzip(dest_file, exdir = dest_dir_name)

      # Remove zipfile
      fs::file_delete(dest_file)

      message(paste("Copied", file_name, "to", dest_path, "and expanded it into", dest_dir_name))
    } else {
      message(paste("Failed. Cannot copy notes to the folder", dest_path, "because it does not exist."))}
  }
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
#' @return Tenso variant registered in systemfonts databast
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



#' Set up slides theme
#'
#' @return Environment vars set and theme set etc
#' @details Sets up the slide theme with Tenso font, Okabe-Ito colors, and element_markdown()
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
kjh_set_slide_theme <- function() {

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

  ggplot2::theme_set(theme_tenso())

}


#' Purl Rmd to R
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

  if(!fs::dir_exists(indir)) {
    stop("The input directory does not exist.")
  }

  if(!fs::dir_exists(outdir)) {
    stop("The output directory does not exist.")
  }

  fnames <- tibble::tibble(
    inpath =
      fs::dir_ls(
        path = here::here(indir),
        recurse = 1,
        glob = "*.Rmd"
      )) %>%
    dplyr::mutate(outname = paste0(tools::file_path_sans_ext(basename(inpath)), ".R"),
           outpath = here::here(outdir, outname)) %>%
    dplyr::filter(outname != "00-slides.R")

  purrr:::walk2(fnames$inpath, fnames$outpath, knitr::purl)
}

#' Set Xaringan Options
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
kjh_set_xaringnan_opts <- function() {
  xaringanExtra::use_xaringan_extra(c("tile_view"))
  xaringanExtra::use_animate_css()
  xaringanExtra::use_animate_all("fade")
  xaringanExtra::use_clipboard()
}

