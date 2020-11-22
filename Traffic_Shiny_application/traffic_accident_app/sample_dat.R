pre.traffic.dat <- read.csv("traffic_accident_app/data/US_Accidents_June20.csv")
library(tidyverse)
samp.traffic.dat <- sample_n(pre.traffic.dat, size = 10000)

write_csv(samp.traffic.dat, file = "traffic_accident_app/data/sample_dat.csv")

