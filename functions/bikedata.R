
bikedata <- function(brand,
                     model,
                     size,
                     wheelbase,
                     wheel_size_in,
                     reach,
                     stack = NA,
                     fork_length,
                     headtube_length,
                     headtube_angle,
                     seat_angle,
                     seat_length,
                     chainstay = NA,
                     bbdiff = NA,
                     bbheight = NA){
  
  # required quantities
  if (is.na(wheelbase)){
    stop("wheelbase is required")
  }
  if (is.na(wheel_size_in)){
    stop("wheel_size_in is required")
  }
  if (is.na(reach)){
    stop("reach is required")
  }
  if (is.na(fork_length)){
    stop("fork_length is required")
  }
  if (is.na(headtube_length)){
    stop("headtube_length is required")
  }
  if (is.na(headtube_angle)){
    stop("headtube_angle is required")
  }
  if (is.na(seat_angle)){
    stop("seat_angle is required")
  }
  if (is.na(seat_length)){
    stop("seat_length is required")
  }
  
  # derived quantity
  if (wheel_size_in == 26){
    tyreradMM = 670 / 2
    wheelradMM = 559 /2
  } else if(wheel_size_in == 27.5){
    tyreradMM = 700 / 2 # guess
    wheelradMM = 584 /2
  } else if(wheel_size_in == 29){
    tyreradMM = 740 / 2
    wheelradMM = 622 /2
  } else if(wheel_size_in == "700C"){
    tyreradMM =  (622 /2) + 40
    wheelradMM = 622 /2
  }
  
  RearWheelCentre = c(0, tyreradMM)
  FrontWheelCentre = c(wheelbase, tyreradMM)
  HeadTubeBottom = FrontWheelCentre + c(-cos(headtube_angle/360 * 2*pi), sin(headtube_angle/360 * 2*pi)) * fork_length
  HeadTubeTop = FrontWheelCentre + c(-cos(headtube_angle/360 * 2*pi), sin(headtube_angle/360 * 2*pi)) * (fork_length + headtube_length)
  
  # do we know bbdiff or bbheight
  if (is.na(bbdiff) & !is.na(bbheight)){
    bbdiff = bbheight - tyreradMM 
  }
  if (is.na(bbheight) & !is.na(bbdiff)){
    bbheight =  tyreradMM + bbdiff
  }
  
  # optional quantities
  # case 1 - have reach and stack
  if (!is.na(stack)){
    BottomBracket = HeadTubeTop - c(reach,stack)
    if (is.na(bbdiff)){
      bbdiff = BottomBracket[2] - tyreradMM
    }
    if (is.na(bbheight)){
      bbheight = BottomBracket[2]
    }
    if (is.na(chainstay)){
      chainstay = sqrt((BottomBracket[1] - RearWheelCentre[1])^2 + bbdiff^2)
    }
  } else{
    # case 2 have chainstay and bbdiff
    if (!is.na(chainstay) & !is.na(bbdiff)){
      BottomBracket = RearWheelCentre + c(sqrt(chainstay^2 - bbdiff^2), -bbdiff)
      stack = HeadTubeTop[2] - BottomBracket[2] 
    }
  }
  
  SeatTubeTop = BottomBracket + c(-cos(seat_angle/360 * 2*pi), sin(seat_angle/360 * 2*pi)) * seat_length
  
  dimensions = list(
    wheelbase = wheelbase,
    wheel_size_in = wheel_size_in,
    tire_radius = tyreradMM,
    wheel_radius = wheelradMM,
    reach = reach,
    stack = stack,
    fork_length = fork_length,
    headtube_length = headtube_length,
    headtube_angle = headtube_angle,
    seat_angle = seat_angle, 
    seat_length = seat_length,
    chainstay = chainstay,
    bbdiff = bbdiff,
    bbheight = bbheight
  )
  
  geompoints = list(
    RearWheelCentre = RearWheelCentre,
    FrontWheelCentre = FrontWheelCentre,
    BottomBracket = BottomBracket,
    HeadTubeBottom = HeadTubeBottom,
    HeadTubeTop = HeadTubeTop,
    SeatTubeTop = SeatTubeTop
  )
  
  this.name <- paste(brand, model, size)
  
  rc <- list(name = this.name, brand = brand, model = model, size = size, dimensions = dimensions, points = geompoints)
  
  attr(rc, "class") <- "bikedata"
  
  return(rc)
}

as.data.frame.bikedata <- function(bd){
  
  nam <- names(bd$dimensions)
  val <- unlist(bd$dimensions)
  
  rc <- data_frame(Name = bd$name, Brand = bd$brand, Model = bd$model, Size = bd$size, Dimension = nam, Value = val)
  return(rc)
}

print.bikedata <- function(bd){
  df <- as.data.frame(bd) %>%
    transmute(Dimension, Value)
  
  names(df) <- c("Dimension", bd$name)
  
  print(df)
}

plot.bikedata <- function(bd){
  p.basis <- ggplot() + 
    coord_cartesian(xlim = c(-1000, 2000), ylim = c(-1000,2000)) +
    theme_void()
  
  rc <- geomBike(p.basis, bd = bd)
  return(rc)
}

