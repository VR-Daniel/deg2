deg2sysconv <- function(values, units){

  if(units == "km2"){
    values <- values / 1000000
    return(values)
  }
  if(units == "ha"){
    values <- values / 10000
    return(values)
  }
  if(units == "yd2"){
    values <- values * 1.196
    return(values)
  }
  if(units == "mi2"){
    values <- values / 2590000
    return(values)
  }
  if(units == "ac"){
    values <- values / 4047
    return(values)
  }

}
