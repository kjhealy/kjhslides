% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{get_files_of_type}
\alias{get_files_of_type}
\alias{get_rmd_qmd}
\title{Get vector of filepaths}
\usage{
get_files_of_type(ftype = "[.]Rmd|qmd$", indir, depth = 1)

get_rmd_qmd(ftype = "[.]Rmd|qmd$", indir, depth = 1)
}
\arguments{
\item{ftype}{Filetype regexp, defaults to '\link{.}Rmd|qmd$'}

\item{indir}{Directory to begin search in}

\item{depth}{Recursion depth, defaults to 1 (ie inside subfolders)}
}
\value{
Vector of file paths
}
\description{
Get filepaths of given type, possibly recursively
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
\dontrun{
if(interactive()){
 get_rmd_qmd()
 }
 }
}
