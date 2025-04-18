#Import data 

#Load packages
library(gts)

source("_alignPrimeMeridian.R")

#Load ESMs aka simulations 
global = read_gts("input/ipsl-cm5a-lr_historical_to_zs_monthly_195001_200512.nc4")

#Change prime meridian if necessary 
#global2 = global
#longitude(global2) = longitude(global2, "center") # put the prime meridian to the center instead of the left (because it was centered on the pacific)

#Load observations 
obs = read_gts("input/avhrr-only-v2-humboldt-n.198109-201706.nc4")
#there is no mask in this object so we need to create one 

#Make sure the Prime Meridian are the same for all gts objects
global <- harmonize_PrimeMeridian (global, target = "left")
obs <- harmonize_PrimeMeridian (obs, target = "left")

#Mask 
mask(obs) = mask(obs$x)
image(mask(obs))

#Spatial subsetting
sim = subset(global, grid=obs, expand=5) #expand the area (based on obs) by a number of degrees in every direction 

#Temporal subsetting 
aligned <- align_time_period(sim, obs)
sim <- aligned$sim
obs <- aligned$obs

#Regriding with the akima method (bivariate interpolation method)
#This ensures that we will have the same resolution for observations and simulations 
sim2 = regrid(object = sim, grid = obs, method = "akima", extrap=TRUE) #extrap = TRUE allows extrapolation of some values if needed 