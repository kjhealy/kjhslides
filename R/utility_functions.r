##' Copy slide infrastructure to a given folder
##'
##' Transfers a zip file containing course materials from the socviz
##'     library to the Desktop.
##' @title setup_slides
##' @param folder The destination to copy to within the user's home.
##'     This must be supplied by the user.
##' @param zipfile The name of the bundled slide file.
##' @param slidefolder The name of the course packet folder to be created
##' @return The `zipfile` is copied to `folder` and its contents
##'     expanded into a directory, the `slidefolder`.
##' @author Kieran Healy
##' @examples
##' setup_slides()
##' @export
setup_slides <- function(folder, zipfile = "slidepack.zip",
                               slidefolder = "slides") {
    if(missing(folder)) {
        message("You must specify a destination for the notes, e.g., '~/Desktop'")
    } else {
        file_name <- zipfile
        lib_loc <- fs::path_package("kjhslides")

        origin_path <- fs::path(lib_loc, "resources", file_name)
        dest_path <- fs::path_expand(folder)

        if(fs::dir_exists(dest_path)) {

            fs::file_copy(origin_path, dest_path)

            dest_file <- fs::path(dest_path, file_name)
            fs::dir_create(dest_path, packet)
            dest_dir_name <- fs::path(dest_path)

            utils::unzip(dest_file, exdir = dest_dir_name)

            message(paste("Copied", file_name, "to", dest_path, "and expanded it into", dest_dir_name))
        } else {
        message(paste("Failed. Cannot copy notes to the folder", dest_path, "because it does not exist."))}
        }
}
