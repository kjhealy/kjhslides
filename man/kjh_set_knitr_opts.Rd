% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{kjh_set_knitr_opts}
\alias{kjh_set_knitr_opts}
\title{Set knitr options}
\usage{
kjh_set_knitr_opts(
  warning = FALSE,
  message = FALSE,
  fig.retina = 3,
  fig.align = "center",
  dev = "ragg_png"
)
}
\arguments{
\item{warning}{knitr warnings FALSE}

\item{message}{knitr messages FALSE}

\item{fig.retina}{Retina 3}

\item{fig.align}{Alignment center}

\item{dev}{Default device is ragg_png}
}
\value{
Knitr options
}
\description{
Default knitr opts for chunks.
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
