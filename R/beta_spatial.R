#' Compute beta diversity on vectors
#' 
#' This function will compute beta diversity on vectors containing
#' multiple species at multiple sites. Species at sites should be 
#' placed sequentially, so that the vector can be transformed in a
#' matrix with species at columns and sites at rows
#' 
#' @param x numeric vector with multiple species and multiple sites
#' @param nspp numeric. number of species to transform the vector 
#' into a matrix with nspp columns
#' @inheritParams betapart::phylo.beta.pair
#' @inheritParams betapart::beta.pair
# #' @export
beta.vec <- function(x, nspp, index.family="sorensen", tree=NA){
  x <- matrix(x, ncol=nspp, byrow = F)
  x <- x[stats::complete.cases(x),]
  
  mean_turnover <- mean_nestedness <- mean_beta <- numeric(1)
  
  if(all(is.na(x))){
    return(c(NA, NA, NA, NA))
  } else if(sum(x)==0) {
    return(c(mean_turnover, mean_nestedness, mean_beta, mean_nestedness/mean_beta))
  } else {
    if(is.na(tree)){
      res <- betapart::beta.pair(x, index.family=index.family)
    } else {
      res <- betapart::phylo.beta.pair(x, tree, index.family=index.family)
    }
    mean_turnover <- mean(res[[1]][lower.tri(res[[1]])], na.rm=T) # mean(as.matrix(res[[1]])[2:length(as.matrix(res[[1]])[,1]),1], na.rm=TRUE) 
    mean_nestedness <- mean(res[[2]][lower.tri(res[[2]])], na.rm=T) # mean(as.matrix(res[[2]])[2:length(as.matrix(res[[2]])[,1]),1], na.rm=TRUE)
    mean_beta <- mean(res[[3]][lower.tri(res[[3]])], na.rm=T) # mean(as.matrix(res[[3]])[2:length(as.matrix(res[[3]])[,1]),1], na.rm=TRUE)
  }
  return(c(mean_turnover, mean_nestedness, mean_beta, mean_nestedness/mean_beta))
}

#' Compute beta diversity on spatRast objects
#' 
#' This function will compute beta diversity on spatRast objects
#' that contain binary (presence/absence) species distribution data
#' 
#' @param x raster brick or raster stack of species presence/absence
#' @param fm focal matrix. Numeric. Make a focal ("moving window")
#' weight matrix for use in the focal function
# #' @param d window radius to compute beta diversity metrics
#' @param numCores Number of cores to be used in parallel calculation
#' @param filetype file format expresses as GDAL driver names. If this 
#' argument is not supplied, the driver is derived from the filename.
#' For details see \code{\link[terra]{writeRaster}} 
#' @param overwrite Logical. Should saved files be overwritten with new values?
#' @inheritParams betapart::phylo.beta.pair
#' @inheritParams betapart::beta.pair
#' @inheritParams terra::focalMat
#' @inheritParams terra::writeRaster
#' @export
beta.spat <- function(x, fm=NULL, d = mean(terra::res(terra::rast(x)))*2, type = "circle",  
                       index.family="sorensen", tree=NA,
                       filetype="GTiff", filename=NULL, overwrite=T,
                       numCores=1, ...) {
  if(!inherits(x, c("SpatRaster"))){
    x <- terra::rast(x)
  }
  if(is.null(fm)){
    min.d <- sqrt(prod(terra::res(x))) #mean(res(x)*112)
    if(d < min.d) {
      stop(paste("radius too small to build a focal window. 
                 Minimum d should be larger than:", min.d)) #111.1194*res(x)[2]/(cos(y*(pi/180)))))
    }
    
    fm <- terra::focalMat(x, d, 
                          type = type,
                          fillNA = T)
  }
  # transformar tudo em 1 para encontrar valores exatos
  fm[] <- fm/max(fm, na.rm = T)
  # create array to 3D focal calculations
  fmA <- replicate(terra::nlyr(x), fm)
  
  betaR <- terra::focal3D(x, fmA,
                          beta.vec,
                          index.family=index.family, 
                          tree=tree, nspp=terra::nlyr(x),
                          na.policy="all")[[1:4]]
  names(betaR) <- c("turnover", "nestedness", "beta_div", "beta_ratio")
  
  if(!is.null(filename)){
    betaR <- terra::writeRaster(betaR, filename = filename,
                                 overwrite=overwrite, filetype=filetype)
  }
  return(betaR)
}