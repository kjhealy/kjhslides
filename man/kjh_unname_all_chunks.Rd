% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chunk_processing.R
\name{kjh_unname_all_chunks}
\alias{kjh_unname_all_chunks}
\title{Unname all chunks}
\usage{
kjh_unname_all_chunks(indir = "slides")
}
\arguments{
\item{indir}{The input directory, default "slides"}
}
\description{
Unname all Rmd chunks in the Rmd folders
}
\details{
All the Rmd files with their chunk names removed, except chunks labeled
'setup' and chunks where include=FALSE, to protect chunk_reveal calls.
Recurses 1 level (i.e. subdirs) by default.
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
