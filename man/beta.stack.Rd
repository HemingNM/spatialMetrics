% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_spatial_functions.R
\name{beta.stack}
\alias{beta.stack}
\title{Compute beta diversity on raster bricks or stacks}
\usage{
beta.stack(
  x,
  radius = 2.8,
  type = "circle",
  index.family = "sorensen",
  tree = NA,
  format = "GTiff",
  filename = NULL,
  overwrite = T,
  numCores = 1
)
}
\arguments{
\item{x}{raster brick or raster stack of species presence/absence}

\item{radius}{window radius to compute beta diversity metrics}

\item{type}{character indicating the type of filter to be returned}

\item{index.family}{ family of dissimilarity indices, partial match of \code{"sorensen"} or \code{"jaccard"}.}

\item{tree}{ a phylogenetic tree of class phylo with tips names identic to species names from the community matrix.}

\item{format}{Character. Output file type. See \code{\link[raster]{writeFormats}}. If this argument is not provided, it is attempted to infer it from the filename extension. If that fails, the default format is used. The default format is 'raster', but this can be changed using \code{\link[raster]{rasterOptions}}}

\item{filename}{Output filename}

\item{overwrite}{Logical. Should saved files be overwritten with new values?}

\item{numCores}{Number of cores to be used in parallel calculation}
}
\description{
This function will compute beta diversity on raster bricks or stacks
that contain binary (presence/absence) species distribution data
}
