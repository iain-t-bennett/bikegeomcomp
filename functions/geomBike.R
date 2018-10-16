
geomBike <- function(p, bd, offset = c(0,0)){

  points.df <- as.data.frame(bd$points)

  df <- NULL
  for (pt in c("RearWheelCentre", "BottomBracket","HeadTubeBottom", "FrontWheelCentre", "HeadTubeTop", "SeatTubeTop", "BottomBracket")){

    coords = points.df[[pt]] + offset
    
    if (is.null(df)){
      df <-  data_frame(point = pt, x = coords[1], y = coords[2])
    } else{
      df <- rbind(df,
                  data_frame(point = pt, x = coords[1], y = coords[2])
      )
    }    
  }
    
  df <- mutate(df, xstart = lag(x), ystart = lag(y), xend = x, yend = y, name = bd$name)
  
  rc <- p +
    geom_point(data = df, aes(x=x,y=y, color = name)) +
    geom_link(data = filter(df, !is.na(xstart)), aes(x=xstart,y=ystart,xend=xend,yend=yend, color = name)) +
    geom_circle(data = filter(df, point %in% c("RearWheelCentre", "FrontWheelCentre")), aes(x0 = x, y0 = y, r = bd$dimensions$wheel_radius, color = name)) +
    geom_circle(data = filter(df, point %in% c("RearWheelCentre", "FrontWheelCentre")), aes(x0 = x, y0 = y, r = bd$dimensions$tire_radius, color = name))
    
  return(rc)

}

