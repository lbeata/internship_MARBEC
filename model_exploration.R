#Exploring models and covariates 
library(mgcv)
library(mgcViz)


modm0.0 = gam(sst~ offset(to), data=dat_month_small) #This will be our null model 
modm0.2 = gam(sst~ offset(to) + ti(lat, k=30) + ti(lon, k=20)+ ti(lon,lat, k=15), data=dat_month_small)
modm0.3 = gam(sst~ to  + ti(lat, k=30) + ti(lon, k=20)+ ti(lon,lat, k=15) + month_fact, data=dat_month_small)
modm0.3b = gam(sst~ to + ti(lat, k=30) + ti(lon, k=20)+ ti(lon,lat, k=15) + season, data=dat_month_small)
modm0.4 = gam(sst~ to  + ti(lat, k=30) + ti(lon, k=20)+ ti(lon,lat, k=15) + ti(month) + ti(lat,month) + ti(lon,month), data=dat_month_small)
modm0.4b = gam(sst~ to  + ti(lat, k=30) + ti(lon, k=20)+ ti(lon,lat, k=15) + month_fact + ti(lat,season) + ti(lon,month_fact), data=dat_month_small)
modm0.4c = gam(sst~ to  + ti(lat, k=30) + ti(lon, k=20)+ ti(lon,lat, k=15) + month_fact + ti(lat,month_fact) + ti(lon,season), data=dat_month_small)
modm0.4d = gam(sst~ to  + ti(lat, k=30) + ti(lon, k=20)+ ti(lon,lat, k=15) + season + ti(lat,month_fact) + ti(lon,month_fact), data=dat_month_small)
modm0.4e = gam(sst~ to  + ti(lat, k=30) + ti(lon, k=20)+ ti(lon,lat, k=15) + month_fact + ti(lat,month_fact) + ti(lon,month_fact), data=dat_month_small)


#Work on the plots with mgcViz
# Create a visualization object
viz <- getViz(modm0.4)

# Set up the layout for a 2x2 grid (adjust based on the number of plots you want)
par(mfrow = c(2, 2))
# Plot all the smooth terms into the same page
plot(viz, pages = 1)
