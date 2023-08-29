#######################################################################
#####  Sample script for opening/processing climate model output  #####
#######################################################################

# Load packages 
options(stringsAsFactors = FALSE)
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(dplyr) # package for general data manipulation


# Load a sample netcdf file 
nc <- nc_open('gfdl-esm4_r1i1p1f1_historical_tos_60arcmin_global_monthly_2000_2014.nc') 
# Save the print(nc) dump to a text file
{
   sink('gfdl-esm4_r1i1p1f1_historical_tos_60arcmin_global_monthly_2000_2014.txt')
   print(nc)
   sink()
}

# get the lat long and time dimensions 
nc.lon <- ncvar_get(nc, "lon")
nc.lat <- ncvar_get(nc, "lat", verbose = F)
nc.t <- ncvar_get(nc, "time") 
head(nc.lon) # look at the first few entries in the longitude vector

# Read in the data from the temp variable and verify the dimensions of the array 
temp.array <- ncvar_get(nc, "tos") # store the data in a 3-dimensional array
dim(temp.array) 

# see what fill value was used for missing data
fillvalue <- ncatt_get(nc, "tos", "_FillValue")
fillvalue
# replace fill values with the R-standard ‘NA'
temp.array[temp.array == fillvalue$value] <- NA

# get 1st month of the data and plot it 
  # time is the third dimension of the array
temp.slice <- temp.array[, , 1] 
dim(temp.slice)

# save this data in a raster format 
  # Note that we provide the coordinate reference system “CRS” in the standard well-known text format (this is the common WGS84 system)
r <- raster(t(temp.slice), xmn=min(nc.lon), xmx=max(nc.lon), ymn=min(nc.lat), ymx=max(nc.lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
# plot the data 
#r <- flip(r, direction='y') #flip the data to orient correctly 
plot(r)
# get the range of the raster values 
range(r[],na.rm=TRUE)

# get a timeseries of the mean temp across all months 
  # first the whole array needs to converted to a raster brick
r_brick <- brick(temp.array, xmn=min(nc.lat), xmx=max(nc.lat), ymn=min(nc.lon), ymx=max(nc.lon), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
print(r_brick)
# calculate the monthly mean and add it to a dataframe with date
brick.stats <- data.frame(temp.mean=cellStats(r_brick, "mean"))
brick.stats$time <- NA
brick.stats$time <- seq(as.Date("2000/01/01"), as.Date("2014/12/01"), by = "month")

# make a plot of the monthly mean of tc global
mean_plot <- ggplot(data=brick.stats, aes(x=time, y=temp.mean)) +
  geom_line()+
  #geom_point()+
  scale_x_date(breaks = scales::pretty_breaks(n = 10))
mean_plot + scale_color_grey() + theme_classic()

