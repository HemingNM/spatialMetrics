% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/beta_spatial.R
\name{beta.temp.vec}
\alias{beta.temp.vec}
\title{Compute temporal beta diversity on vectors}
\usage{
beta.temp.vec(x, index.family = "sorensen", ...)
}
\arguments{
\item{x}{numeric vector with multiple species at two sites}

\item{index.family}{ family of dissimilarity indices, partial match of \code{"sorensen"} or \code{"jaccard"}.}

\item{...}{additional arguments for fun. Not in use.}
}
\description{
This function will compute beta diversity on vectors containing
multiple species at two sites. Species at sites should be
placed sequentially, so that the vector can be transformed in a
matrix with species at columns and sites at rows
}
