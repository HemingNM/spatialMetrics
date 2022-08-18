
## My spatial functions

#' Convert distance from kilometers to decimal degrees
#' 
#' @param dist Numeric. Vector of distances to be converted from km to 
#' decimal degrees
#' @param y Numeric or Extent object, or any object from which an 
#' Extent object can be extracted. See \code{\link[terra]{crop}} for details. 
#' Latitude of location where degrees will be converted to kilometers
#' @export
km2decdeg <- function(dist, y){
  y <- y_mean(y)
  dist/(cos(y*(pi/180)))/111.321
}


#' Convert distance from decimal degrees to kilometers
#' 
#' @param deg Numeric. Vector of distances (in degrees) to be converted to
#' kilometers
#' @param y Numeric, Extent object, or any object from which an 
#' Extent object can be extracted. See \code{\link[terra]{crop}} for details. Latitude 
#' of location where degrees will be converted to kilometers
#' @export
decdeg2km <- function(deg, y){
  y <- y_mean(y)
  deg*(cos(y*(pi/180)))*111.321
}

#' Compute avg values of y
#' 
#' @param y Numeric or Extent object, or any object from which an 
#' Extent object can be extracted. See \code{\link[terra]{crop}} for details. 
#' 
#' @keywords internal
y_mean <- function(y){
  UseMethod("y_mean", y)
}

#' Compute avg values of y for numeric class
#' @keywords internal
y_mean.numeric <- function(y){
  mean(y)
}

#' Compute avg values of y for SpatRaster class
#' @keywords internal
y_mean.SpatRaster <- function(y){
  mean(c(terra::ymin(y), terra::ymax(y)))
}

#' Compute avg values of y for Raster class
#' @keywords internal
y_mean.Raster <- function(y){
  mean(c(raster::ymin(y), raster::ymax(y)))
}

##
crs_m <- function(x){
  UseMethod("crs_m", x)
}

crs_m.SpatRaster <- function(x){
  terra::crs(
    paste0("+proj=eqdc +units=km", # eqdc \ laea
           " +lat_1=",
           mean(terra::ymin(x)),
           " +lat_2=",
           mean(terra::ymax(x)),
           " +lon_0=",
           mean(c(terra::xmax(x), terra::xmin(x))) )
  )
}

crs_m.Raster <- function(x){
  raster::crs(
    paste0("+proj=eqdc +units=km", # eqdc \ laea
           " +lat_1=",
           mean(raster::ymin(x)),
           " +lat_2=",
           mean(raster::ymax(x)),
           " +lon_0=",
           mean(c(raster::xmax(x), raster::xmin(x))) )
  )
}


#' Compute beta diversity on arrays
#' 
#' This function will compute beta diversity on arrays where 
#' the first two dimensions are sites and the third dimension is species
#' 
#' @param x numeric array of with 3 dimensions
#' @param fwCC vector of observations to be kept. It is a matrix returned
#' from raster::focalWeight and coerced to vector with as.vector()
#' @inheritParams betapart::phylo.beta.pair
#' @inheritParams betapart::beta.pair
#' @export
beta.array <- function(x, index.family="sorensen", tree=NA, fwCC){
  # print(x)
  x <- apply(x, 3, FUN=as.vector)
  x <- x[stats::complete.cases(x) & fwCC,]
  
  mean_turnover <- mean_nestedness <- mean_beta <- numeric(1)
  
  if(sum(x)==0) {
    return(c(mean_turnover, mean_nestedness, mean_beta))
  } else {
    if(is.na(tree)){
      res <- betapart::beta.pair(x, index.family=index.family)
    } else {
      res <- betapart::phylo.beta.pair(x, tree, index.family=index.family)
    }
    mean_turnover <- mean(as.matrix(res[[1]])[2:length(as.matrix(res[[1]])[,1]),1], na.rm=TRUE) 
    mean_nestedness <- mean(as.matrix(res[[2]])[2:length(as.matrix(res[[2]])[,1]),1], na.rm=TRUE)
    mean_beta <- mean(as.matrix(res[[3]])[2:length(as.matrix(res[[3]])[,1]),1], na.rm=TRUE)
  }
  return(c(mean_turnover, mean_nestedness, mean_beta))
}



#' Compute beta diversity on raster bricks or stacks
#' 
#' This function will compute beta diversity on raster bricks or stacks
#' that contain binary (presence/absence) species distribution data
#' 
#' @param x raster brick or raster stack of species presence/absence
#' @param radius window radius to compute beta diversity metrics
#' @param numCores Number of cores to be used in parallel calculation
#' @param overwrite Logical. Should saved files be overwritten with new values?
#' @inheritParams betapart::phylo.beta.pair
#' @inheritParams betapart::beta.pair
#' @inheritParams raster::focalWeight
#' @inheritParams raster::writeRaster
#' @export
beta.stack <- function(x, radius = 2.8, type = "circle",  
                       index.family="sorensen", tree=NA,
                       format="GTiff", filename=NULL, overwrite=T,
                       numCores=1) {
  min.radius <- mean(raster::res(x)*111.321)
  if(radius < min.radius) {
    # radius <- min.radius
    stop(paste("radius too small to build a focal window. Minimum radius is:", min.radius)) #111.1194*res(x)[2]/(cos(y*(pi/180)))))
  }
  fw <- raster::focalWeight(x, km2decdeg(radius, x), type=type)
  fw[fw>0] <- 1
  fw[fw==0] <- NA
  fwCC <- stats::complete.cases(as.vector(fw))
  
  # run
  # if(numCores>1){
  spatial.tools::sfQuickInit(numCores)
  # }
  # spatial.tools::sfQuickInit()
  betaR <- spatial.tools::rasterEngine(x=x,
                                       args=list(index.family=index.family, tree=tree, fwCC=fwCC), 
                                       fun=beta.array, 
                                       window_dims=dim(fw),
                                       outbands=3,
                                       setMinMax=T,
                                       .packages="betapart")
  # if(numCores>1){
  spatial.tools::sfQuickStop()
  # }
  
  names(betaR) <- c("turnover", "nestedness", "beta")
  if(!is.null(filename)){
    betaR <- raster::writeRaster(betaR, filename = filename,
                                 overwrite=overwrite, format=format)
  }
  return(betaR)
}
# end