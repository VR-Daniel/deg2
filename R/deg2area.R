#' Latitudinal semi-corrected areas from pixels in degrees
#'
#' @description \code{deg2area} calculates semi-corrected areas from raster files with cells in degrees, given that the equivalence of degrees to distance varies cosine-based latitudinally. Areas are considered semi-corrected because despite the latitudinal correction, the procedure is performed in planar space.
#' @param raster A raster object of class \code{RasterLayer}.
#' @param classCriteria Pixel values to be used as classification criteria for calculating the areas. It may be a single number or a vector for specific values, or a 2xN matrix with the lower and upper limits values when ranges.
#' @param outputUnits Measurement units of the output. It may be \code{"m2"} for square meters, \code{"km2"} for square kilometers, \code{"ha"} for hectare, \code{"yd2"} for square yards, \code{"mi2"} for square miles, and \code{"ac"} for acres. If no value is provided output is calculated in square kilometers by default.
#' @param intervalType Indicates the type of interval when \code{classCriteria} is a range matrix. It may be of four types: \code{1} for left open and right closed, \code{2} for left open and right closed except the first class which is closed, \code{3} for left closed and right open, and \code{4} for left closed and right open except the last class which is closed. By default it is \code{2}. The range matrix accepts \code{-Inf} and \code{Inf} as values.
#' @param corShape Indicates the geometric shape for the calculation of the areas. It may be "S" for square or "T" for a trapezoid (by default). When the shape is defined as a square, the pixel centroid latitude is used for the degrees to distance equivalence, and then the area is calculated for a perfect square in planar space. When the shape is a trapezoid, the area is calculated using the trapezium formula, in an attempt to approximate the real shape of the "cells" in a tridimensional space when increasing latitude. For this last, the total sum of areas for small resolution rasters is more precise (as in the Riemann sum) but computationally more expensive.
#' @param uncorrected Boolean indicating if the output should include a non-corrected area column (for comparison purposes only). The non-corrected area assumes distances at latitude 0°, that is, 1 arcsec = 30.87 meters. By default it is \code{FALSE}.
#' @return A data frame when the input is a single raster, or a list when multiple, including the pixel values used for classification, the number of pixels meeting the classification criteria, and the area in the selected units.
#' @export deg2area
deg2area <- function(raster, classCriteria, outputUnits = "km2", intervalType = 2, corShape = "T", uncorrected = FALSE){

  # Arguments validation ------

  V1 = 0
  V2 = 0
  V3 = 0
  V4 = 0
  V5 = 0
  V6 = 0

  if (missing(raster) | missing(classCriteria)){
    cat("\n")
    stop(paste("One or more missing arguments with no default value", "\n\n"), call. = FALSE)
  }
  if("RasterLayer" %in% class(raster) == TRUE | "RasterStack" %in% class(raster) == TRUE | "RasterBrick" %in% class(raster) == TRUE){
    V1 = 1
  }else{
    cat("\n")
    stop("Input file must be an object of class \"RasterLayer\", \"RasterStack\", or \"RasterBrick\"", "\n\n", call. = FALSE)
  }
  if((is.numeric(classCriteria) == TRUE) & (length(which(is.na(classCriteria))) == 0)){
    V2 = 1
  }else{
    if(is.matrix(classCriteria) == TRUE | is.data.frame(classCriteria) == TRUE){
      if(ncol(classCriteria) == 2 & nrow(classCriteria) >= 1){
        if((length(which(is.na(classCriteria))) == 0) & (is.numeric(classCriteria))){
          V2 = 1
        }else{
          cat("\n")
          stop("classCriteria matrix cannot contain NA or non-numeric values", "\n\n", call. = FALSE)
        }
      }else{
        cat("\n")
        stop("When matrix, classCriteria must be two columns x N rows", "\n\n", call. = FALSE)
      }
    }else{
      cat("\n")
      stop("classCriteria must be of class \"numeric\" or a two-column \"matrix\" or \"data.frame\"", "\n\n", call. = FALSE)
    }
  }
  if(outputUnits %in% c("m2", "km2", "ha", "yd2", "mi2", "ac")){
    V3 = 1
  }else{
    cat("\n")
    stop("outputUnits out of the limits", "\n\n", call. = FALSE)
  }
  if(intervalType %in% c(1:4)){
    V4 = 1
  }else{
    cat("\n")
    stop("intervalType out of the limits", "\n\n", call. = FALSE)
  }
  if(is.logical(uncorrected)){
    V5 = 1
  }else{
    cat("\n")
    stop("uncorrected must be TRUE/FALSE", "\n\n", call. = FALSE)
  }
  if(corShape %in% c("S", "T")){
    V6 = 1
  }else{
    cat("\n")
    stop("corShape out of the limits", "\n\n", call. = FALSE)
  }

  # Function ------

  if(all(c(V1, V2, V3, V4, V5, V6) == 1) == TRUE){
    if(dim(raster)[3] == 1){
      deg2area.int(raster, classCriteria, outputUnits, intervalType, corShape, uncorrected)
    }else{
      tmp.list <- list()
      for(i in 1:dim(raster)[3]){
        tmp.ras <- raster::subset(raster, i)
        tmp.list[[i]] <- deg2area.int(tmp.ras, classCriteria, outputUnits, intervalType, corShape, uncorrected)
      }
      return(tmp.list)
    }
  }

}

deg2area.int <- function(raster, classCriteria, outputUnits, intervalType, corShape, uncorrected){
  if(round(res(raster)[1], 7) == round(res(raster)[2], 7)){
    arcsec <- deg2arc(res(raster)[1])
  }else{
    cat("\n")
    stop("Oops! Raster must have the same resolution in the x and y dimensions", "\n\n", call. = FALSE)
  }
  raster.df <- as.data.frame(rasterToPoints(raster))
  if(ncol(raster.df) != 3){
    cat("\n")
    stop("Raster cannot be converted to a three-column matrix", "\n\n", call. = FALSE)
  }else{
    if(min(raster.df[, 1]) < -180 | max(raster.df[, 1]) > 180 | min(raster.df[, 2] < -90 | max(raster.df[, 2]) > 90)){
      cat("\n")
      stop("Raster longitude/latitude values are out of the limits", "\n\n", call. = FALSE)
    }
  }
  raster.df <- na.omit(raster.df)
  colnames(raster.df) <- c("x", "y", "value")
  if(is.matrix(classCriteria) == TRUE){
    min <- min(classCriteria)
    max <- max(classCriteria)
    levels <- c()
    for(i in 1:nrow(classCriteria)){
      tmp.level <- paste(classCriteria[i, 1], classCriteria[i, 2], sep = ",")
      levels <- append(levels, tmp.level)
    }
    if(intervalType == 1){
      raster.df <- dplyr::filter(raster.df, value >= min, value <= max)
      tmp.int <- cut(raster.df$value, unique(c(t(classCriteria))))
    }
    if(intervalType == 2){
      raster.df <- dplyr::filter(raster.df, value >= min, value < max)
      tmp.int <- cut(raster.df$value, unique(c(t(classCriteria))), include.lowest = TRUE)
    }
    if(intervalType == 3){
      raster.df <- dplyr::filter(raster.df, value > min, value <= max)
      tmp.int <- cut(raster.df$value, unique(c(t(classCriteria))), right = FALSE)
    }
    if(intervalType == 4){
      raster.df <- dplyr::filter(raster.df, value > min, value < max)
      tmp.int <- cut(raster.df$value, unique(c(t(classCriteria))), include.lowest = TRUE, right = FALSE)
    }
    if(corShape == "S"){
      if(uncorrected == FALSE){
        raster.df <- dplyr::mutate(raster.df, area = (((1852/60) * cos(y*pi/180) * arcsec)^2))
        raster.sum <- as.data.frame(dplyr::group_by(raster.df, value = tmp.int) %>% dplyr::summarise(pixels = n(), area = sum(area)))
        colnames(raster.sum) <- c("Pixel.Val", "Pixels", "Area")
      }else{
        raster.df <- dplyr::mutate(raster.df, area = (((1852/60) * cos(y*pi/180) * arcsec)^2), area.un = (((1852/60) * arcsec)^2))
        raster.sum <- as.data.frame(dplyr::group_by(raster.df, value = tmp.int) %>% dplyr::summarise(pixels = n(), area = sum(area), area.un = sum(area.un)))
        colnames(raster.sum) <- c("Pixel.Val", "Pixels", "Area", "Area.Un")
      }
    }else{
      if(uncorrected == FALSE){
        raster.df <- dplyr::mutate(raster.df, Lo = (y - (res(raster)[1]/2)), Up = (y + (res(raster)[1]/2)), Lo.Dis = ((1852/60) * cos(Lo*pi/180) * arcsec), Up.Dis = ((1852/60) * cos(Up*pi/180) * arcsec), h = ((1852/60) * arcsec), area = (((Lo.Dis + Up.Dis)/2) * h))
        raster.sum <- as.data.frame(dplyr::group_by(raster.df, value = tmp.int) %>% dplyr::summarise(pixels = n(), area = sum(area)))
        colnames(raster.sum) <- c("Pixel.Val", "Pixels", "Area")
      }else{
        raster.df <- dplyr::mutate(raster.df, Lo = (y - (res(raster)[1]/2)), Up = (y + (res(raster)[1]/2)), Lo.Dis = ((1852/60) * cos(Lo*pi/180) * arcsec), Up.Dis = ((1852/60) * cos(Up*pi/180) * arcsec), h = ((1852/60) * arcsec), area = (((Lo.Dis + Up.Dis)/2) * h), area.un = (((1852/60) * arcsec)^2))
        raster.sum <- as.data.frame(dplyr::group_by(raster.df, value = tmp.int) %>% dplyr::summarise(pixels = n(), area = sum(area), area.un = sum(area.un)))
        colnames(raster.sum) <- c("Pixel.Val", "Pixels", "Area", "Area.Un")
      }
    }
    raster.sum <- dplyr::filter(raster.sum, gsub("\\[|\\]|\\(|\\)", "", as.character(Pixel.Val)) %in% levels)
    raster.sum$Pixel.Val <- gsub(",", ", ", raster.sum$Pixel.Val)
    if(nrow(raster.sum) == 0){
      raster.sum[1,] <- NA
      return(raster.sum)
    }else{
      if(outputUnits == "m2"){
        return(raster.sum)
      }else{
        if(uncorrected == FALSE){
          raster.sum$Area <- deg2sysconv(raster.sum$Area, outputUnits)
          return(raster.sum)
        }else{
          raster.sum$Area <- deg2sysconv(raster.sum$Area, outputUnits)
          raster.sum$Area.Un <- deg2sysconv(raster.sum$Area.Un, outputUnits)
          return(raster.sum)
        }
      }
    }
  }else{
    raster.df <- dplyr::filter(raster.df, value %in% classCriteria)
    if(corShape == "S"){
      if(uncorrected == FALSE){
        raster.df <- dplyr::mutate(raster.df, area = (((1852/60) * cos(y*pi/180) * arcsec)^2))
        raster.sum <- as.data.frame(dplyr::group_by(raster.df, value) %>% dplyr::summarise(pixels = n(), area = sum(area)))
        colnames(raster.sum) <- c("Pixel.Val", "Pixels", "Area")
      }else{
        raster.df <- dplyr::mutate(raster.df, area = (((1852/60) * cos(y*pi/180) * arcsec)^2), area.un = (((1852/60) * arcsec)^2))
        raster.sum <- as.data.frame(dplyr::group_by(raster.df, value) %>% dplyr::summarise(pixels = n(), area = sum(area), area.un = sum(area.un)))
        colnames(raster.sum) <- c("Pixel.Val", "Pixels", "Area", "Area.Un")
      }
    }else{
      tmp.hres <-
      if(uncorrected == FALSE){
        raster.df <- dplyr::mutate(raster.df, Lo = (y - (res(raster)[1]/2)), Up = (y + (res(raster)[1]/2)), Lo.Dis = ((1852/60) * cos(Lo*pi/180) * arcsec), Up.Dis = ((1852/60) * cos(Up*pi/180) * arcsec), h = ((1852/60) * arcsec), area = (((Lo.Dis + Up.Dis)/2) * h))
        raster.sum <- as.data.frame(dplyr::group_by(raster.df, value) %>% dplyr::summarise(pixels = n(), area = sum(area)))
        colnames(raster.sum) <- c("Pixel.Val", "Pixels", "Area")
      }else{
        raster.df <- dplyr::mutate(raster.df, Lo = (y - (res(raster)[1]/2)), Up = (y + (res(raster)[1]/2)), Lo.Dis = ((1852/60) * cos(Lo*pi/180) * arcsec), Up.Dis = ((1852/60) * cos(Up*pi/180) * arcsec), h = ((1852/60) * arcsec), area = (((Lo.Dis + Up.Dis)/2) * h), area.un = (((1852/60) * arcsec)^2))
        raster.sum <- as.data.frame(dplyr::group_by(raster.df, value) %>% dplyr::summarise(pixels = n(), area = sum(area), area.un = sum(area.un)))
        colnames(raster.sum) <- c("Pixel.Val", "Pixels", "Area", "Area.Un")
      }
    }
    if(nrow(raster.sum) == 0){
      raster.sum[1,] <- NA
      return(raster.sum)
    }else{
      if(outputUnits == "m2"){
        return(raster.sum)
      }else{
        if(uncorrected == FALSE){
          raster.sum$Area <- deg2sysconv(raster.sum$Area, outputUnits)
          return(raster.sum)
        }else{
          raster.sum$Area <- deg2sysconv(raster.sum$Area, outputUnits)
          raster.sum$Area.Un <- deg2sysconv(raster.sum$Area.Un, outputUnits)
          return(raster.sum)
        }
      }
    }
  }
}
