% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nish.R
\name{create_gradientn_scale}
\alias{create_gradientn_scale}
\title{Create a function that returns a gradientn scale}
\usage{
create_gradientn_scale(aes = c("colour", "fill"))
}
\arguments{
\item{aes}{A character aesthetic. Allowed values are "colour" and "fill"
(partial matching).}
}
\value{
A function that returns a gradientn scale when called. Defaults to
use the following colours, \code{nish_colours[c(7, 5, 3:1)]}.
}
\description{
Create a gradientn scale using nish colour scheme.
}
\details{
If options ggplot2.continuous.colour and ggplot2.continuous.fill are unset,
gradient is used instead of continuous.
}
