rm(list = ls())

require(ggplot2)
require(dplyr)
require(ggforce)
require(reshape2)

source("functions/geomBike.R")
source("functions/bikedata.R")

spot_rocket_L = bikedata(
  brand = "Spot",
  model = "Rocket",
  size = "L",
  wheelbase = 1143,
  wheel_size = 29,
  chainstay = 436,
  reach = 456,
  stack = 626,
  fork_length = 530,
  headtube_length = 120,
  headtube_angle = 67.7,
  seat_angle = 73.7,
  seat_length = 483
)


norco_sight_L = bikedata(
  brand = "Norco",
  model = "Sight",
  size = "L",
  wheelbase = 1152,
  wheel_size = 26,
  bbdiff = +0.5,
  reach = 444,
  stack = 580,
  fork_length = 511,
  headtube_length = 130,
  headtube_angle = 67.5,
  seat_angle = 74,
  seat_length = 470
)

nukeproof_scalp_L = bikedata(
  brand = "Nukeproof",
  model = "Scalp",
  size = "L",
  wheelbase = 1216.7,
  wheel_size_in = 26,
  reach = 410.6,
  stack = 602.1,
  fork_length = 585,
  headtube_length = 124,
  headtube_angle = 63,
  seat_angle = 68,
  seat_length = 420,
  bbheight = 14.16 * 23.5,
  chainstay = 446.3
)

nukeproof_scalp_M = bikedata(
  brand = "Nukeproof",
  model = "Scalp",
  size = "M",
  wheelbase = 1196.8,
  wheel_size_in = 26,
  reach = 390.6,
  stack = 602.1,
  fork_length = 585,
  headtube_length = 124,
  headtube_angle = 63,
  seat_angle = 68,
  seat_length = 405,
  bbheight = 14.16 * 23.5,
  chainstay = 446.3
)

ragley_bluepig_20 = bikedata(
  brand = "Ragley",
  model = "BluePig",
  size = "20",
  wheelbase = 1146,
  wheel_size_in = 26,
  bbdiff = -20, 
  reach = 423,
  fork_length = 539,
  headtube_length = 120,
  headtube_angle = 66.5 ,
  seat_angle = 72 ,
  seat_length = 508,
  chainstay = 425
)


nukeproof_scalp_M
plot(nukeproof_scalp_M)


plot(nukeproof_scalp_L) %>%
  geomBike(bd = nukeproof_scalp_M)

plot(nukeproof_scalp_L) %>%
  geomBike(bd = ragley_bluepig_20) %>%
  geomBike(bd = norco_sight_L)

plot(ragley_bluepig_20) %>%
  geomBike(bd = spot_rocket_L) 

plot(nukeproof_scalp_L) %>%
  geomBike(bd = ragley_bluepig_20, offset = c(0,-1000)) %>%
  geomBike(bd = norco_sight_L, offset = c(0,1000))

all.df <- rbind(
    as.data.frame(norco_sight_L),
    as.data.frame(ragley_bluepig_20),
    as.data.frame(nukeproof_scalp_M),
    as.data.frame(nukeproof_scalp_L),
    as.data.frame(spot_rocket_L)
)

all.df %>%
  filter(Brand != "Nukeproof") %>%
  dcast(Dimension ~ Name, value.var = "Value") %>%
  filter(Dimension %in% c("headtube_angle", "seat_angle", "wheelbase", "chainstay", "reach", "stack"))
