#' Equivalence between degrees and arcmin/arcsec
#'
#' @description \code{deg2arc} converts degrees to their equivalents in arcmin or arcserc, and vice versa.
#' @param toConvert A raster object or a numeric value or vector.
#' @param units Measurement units of the output when transformation is from degrees to arc (\code{inverse = FALSE}). When transformation is from arc to degrees (\code{inverse = TRUE}), this is the input units. It may be \code{"m"} for arcmin, or \code{"s"} for arcsec. By default it is arcsec.
#' @param inverse Indicates the direction of the transformation, from degrees to arc when \code{FALSE}, or from arc to degrees when \code{TRUE}, in this case \code{toConvert} must be numeric, no raster input is supported.
#' @return A value or vector of values with the equivalences.
#' @export deg2arc
deg2arc <- function(toConvert, units = "s", inverse = FALSE){

  # Arguments validation ------

  V1 = 0
  V2 = 0
  V3 = 0

  if (missing(toConvert)){
    cat("\n")
    stop(paste("One or more missing arguments with no default value", "\n\n"), call. = FALSE)
  }
  if(("RasterLayer" %in% class(toConvert) == TRUE) & (inverse == FALSE)){
    V1 = 1
  }else{
    if(("RasterLayer" %in% class(toConvert) == TRUE) & (inverse == TRUE)){
      cat("\n")
      stop("Conversion from arc to degrees cannot be made with a raster as input, try with a \"numeric\" objet", "\n\n", call. = FALSE)
    }else{
      if(is.numeric(toConvert) == TRUE & length(toConvert) >= 1){
        V1 = 1
      }else{
        cat("\n")
        stop("toConvert argument must be an object of class \"RasterLayer\" or \"numeric\"", "\n\n", call. = FALSE)
      }
    }
  }
  if(units == "s" | units == "m"){
    V2 = 1
  }else{
    cat("\n")
    stop("units out of the limits", "\n\n", call. = FALSE)
  }
  if(is.logical(inverse)){
    V3 = 1
  }else{
    cat("\n")
    stop("inverse must be TRUE/FALSE", "\n\n", call. = FALSE)
  }

  # Function ------

  if(all(c(V1, V2, V3) == 1) == TRUE){
    if("RasterLayer" %in% class(toConvert) == TRUE){
      if(res(toConvert)[1] == res(toConvert)[2]){
        res.deg <- res(toConvert)[1]
      }else{
        # PENDIENTE
      }
      if(units == "s"){
        res.arc <- res.deg * 3600
      }else{
        res.arc <- res.deg * 60
      }
      return(res.arc)
    }else{
      if(inverse == FALSE){
        res.deg <- toConvert
        if(units == "s"){
          res.arc <- res.deg * 3600
        }else{
          res.arc <- res.deg * 60
        }
        return(res.arc)
      }else{
        res.arc <- toConvert
        if(units == "s"){
          res.deg <- res.arc / 3600
        }else{
          res.deg <- res.arc / 60
        }
        return(res.deg)
      }
    }
  }

}
