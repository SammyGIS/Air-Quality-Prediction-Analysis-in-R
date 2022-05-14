# Install necessary libraries
# install.packages(c("tidyverse","leaflet", "rgdal"))

# Importing the needed library
library(tidyverse)
library(leaflet)
library(rgdal)
library(sp)
library(raster)
library(gstat)

# import dataset
data <- read_csv("annual_conc_by_monitor_2000.csv")

# print cols name in the data
spec(data) 


# After visualizing the data, we need to first filter the one we needed from the data because it contains all
# air PM25_pro for United State in the year 2000.

# Filter by state name, Select all data where the state name 
# is South Carolina, North Carolina, Georgia,Kentucky & Tennessee
States  <- filter(data, `State Name`  %in%  c("South Carolina", "North Carolina", "Georgia", "Kentucky","Tennessee"))


# The next thing to do is data cleaning, to drop the columns that is not need in this Analysis
#  The data columns was study veey well in the data view, then we decided to create a new dataframe with the data
# will be need to create interpolation and spatial clusters

"
The columns needed are
Latitude
Longitude
Parameter Name
Arithmetic Mean
State Name
"
#  create a new dataframe with the needed columns
df <- States[,c('Latitude', 'Longitude','Parameter Name','Arithmetic Mean','State Name')]

## Filter out the Parameters needed in the Parameter Name column.

# The three parameters needed for this analysis are
# PM25dioxide (PM25),
# PM25 (PM25) and,
# PM2.5 - Local Conditions


# filter out the parametrs required
df1  <- filter(df, `Parameter Name` %in%  c("PM25dioxide (PM25)", "PM25", "PM2.5 - Local Conditions"))

# Check for duplicate locations
duplicated(df1)

# There must be only one lat/long per pollutant.
# to create a data with one lat/long per pollutant,we need only the distinct values of lat/log
# We will use the LAtitude, Longititude and Parameter Name as a single group to aggregate the Arithmetic Mean values
# into a single mean for each points

df2 = df1 %>% group_by(Latitude, Longitude,`Parameter Name`,`State Name` )%>%
  summarise(Average = mean(`Arithmetic Mean`),
            .groups = 'drop')


# INTERPOLATION 
"
We are creating interpolation for each of the parameter
# PM25dioxide (PM25),
# PM25 and,
# PM2.5 - Local Conditions
"
# The gstat library  will be used for interpolation because it contains set of
# functions for Spatial and Spatio-Temporal Geostatistical Modelling,Prediction and Simulation.

# IDW Interpolation

# Method Selection
# Initial (df2)parameter selection (e.g., k for IDW)
# Cross-Validate - Use LOOCV
# Prediction and map creation

df2["Longitude"][df2["Longitude"] <= -90 ] <- -90

# PM25Interpolation
PM25 <- filter(df2, `Parameter Name` == "PM2.5 - Local Conditions")

# Display PM25 points on a map
leaflet(data = PM25)%>% 
  addTiles()%>%
  addMarkers()



# grab the long and lat from the data to create a coords 
xy <- PM25[,c(2,1)]

# Setting existing coordinate as lat-long system
PM25_1 <- SpatialPointsDataFrame(coords = xy, data = PM25,
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Transforming coordinate to UTM using EPSG=26916 for NAD83, UTM Zone=16N)
PM25_pro <- spTransform(PM25_1, CRS("+init=epsg:26916"))

# View the data property
PM25_pro@proj4string

# add the polygon boundary of the five contagious states
study_area <- readOGR("States.geojson")
study_area@proj4string


# Transforming coordinate to UTM using EPSG=26916 for NAD83, UTM Zone=16N)
States_pro <- spTransform(study_area, CRS("+init=epsg:26916"))


# overlay PM25on South Carolina boundary
plot(States_pro, border = "red")
plot(PM25_pro, add = TRUE)


# To create a grid around our study area, we will create a bounding box around it
extent.study_area <- extent(States_pro)
extent.study_area

#Based on the extent of the boundary box we use the expand.grid() function to generate a data frame with x and y coordinates.
grid.study_area <- expand.grid(x = seq(from = round(extent.study_area@xmin),
                                       to = round(extent.study_area@xmax),
                                       by = 10000),
                               y = seq(from = round(extent.study_area@ymin),
                                       to = round(extent.study_area@ymax),
                                       by = 10000))

head(grid.study_area , 10)


plot(grid.study_area)



# we cast it into a SpatialPoints object by applying the coordinates() function.
coordinates(grid.study_area) <- ~ x + y
class(grid.study_area)

proj4string(grid.study_area) <- proj4string(PM25_pro)
head(grid.study_area)

# Then we apply the gridded() function to cast the SpatialPoints to a SpatialPixels object.

gridded(grid.study_area) <- TRUE
class(grid.study_area)


# Finally we plot our regular grid of cell size 10 km, the administrative borders of the federal states within East Germany and the DWD weather stations.
plot(grid.study_area,
     main = paste("Air quality monitoring Station\n and Interpolation Grid"),
     col = "grey",
     cex.main = 0.9)

plot(States_pro, add = TRUE, border = "red")
plot(PM25_pro, add = TRUE, pch = 15, cex = 0.5, col = "blue")


neighbors = length(PM25_pro)
beta = 2

PM25_idw = gstat(formula = Average ~ 1, # intercept only model
                  data = PM25_pro, 
                  nmax = neighbors, 
                  set = list(idp = beta))



# Now that we have specified a gstat object we make use of the generic predict() function to interpolate our predictor variable for each pixel in our SpatialPixelsDataFrame object grid.east.germany.

study_area.PM25 <- predict(object = PM25_idw,
                         newdata = grid.study_area)

plot(study_area.PM25, main = "IDW Particle Matter ocal condition (PM2.5)")
plot(States_pro, add = TRUE, border = "white")
plot(PM25_pro, add = TRUE, pch = 19, cex = 0.5, col = "green")



# In order to clip the SpatialPixelsDataFrame object grid.east.germany.temp, 
# we first create a RasterLayer object, using the raster()
# function, and then we apply the mask() function.

# masking
study_area_PM25 <- mask(raster(study_area.PM25),States_pro)



# plotting
plot(study_area_PM25, main = "IDW PM25Dioxide (02) k = 2")
plot(States_pro, add = TRUE, border = "black")
plot(PM25_pro, add = TRUE, pch = 19, cex = 0.5, col = "red")




# The output is an object of class sp, with the prediction results stored in the variable: var1.pred
class(study_area.PM25)
str(study_area.PM25)
spplot(study_area.PM25, "var1.pred")




## Cross Validation

# The gstat package has a built in cross validation function for both IDW and Kriging, krige.cv()  

# The krige.cv function can conduct both LOOCV and k-fold validation

# LOOCV, set nfold = number of observations


LOOCV <- krige.cv(Average~1, 
                  PM25_pro, 
                  nfold = nrow(PM25_pro), # Set the number of folds to the number of rows
                  set = list(idp = 2)
)


# View the stucture of the object.
str(LOOCV)

# View the LOOCV data
LOOCV@data

# Plot the residuals.
spplot(LOOCV, "residual")


# To calculate RMSE we will need to square our residuals, find the mean, and take the square.
# This will need to be conducted a number of times. Here is a simple function to complete the same task.

RMSE_resid <- function(x){
  return(sqrt(mean(x^2)))
}

# Try the function.
RMSE_resid(LOOCV@data$residual)

# At this point you could try different exponent values and compare the cross-validation to determine the optimal k.
# Exponent of 1 (k)
LOOCV_k1 <- krige.cv(Average~1, 
                     PM25_pro, 
                     nfold = nrow(PM25_pro), # Set the number of folds to the number of rows
                     set = list(idp = 1)
)

# Exponent of 2 (k)
LOOCV_k2 <- krige.cv(Average~1, 
                     PM25_pro, 
                     nfold = nrow(PM25_pro), # Set the number of folds to the number of rows
                     set = list(idp = 2)
)

# Exponent of 3 (k)
LOOCV_k3 <- krige.cv(Average~1, 
                     PM25_pro, 
                     nfold = nrow(PM25_pro), # Set the number of folds to the number of rows
                     set = list(idp = 3)
)


# Determine the best value of k, using RMSE of the residuals.

# Determine RMSE when k = 1
RMSE_resid(LOOCV_k1@data$residual)

# Determine RMSE when k = 2
RMSE_resid(LOOCV_k2@data$residual)

# Determine RMSE when k = 3
RMSE_resid(LOOCV_k3@data$residual)