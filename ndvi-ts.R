library(terra)
library(magrittr)
library(sf)
# Directory where Sentinel-2 data is stored
datadir <- "E:/_SummerSchool/_SummerSchool/Data/Sentinel-2/Study_Area/"

# List of the gap polygon from each year
aoi <- vect("E:/_SummerSchool/_SummerSchool/Data/Study_Area_Perimeter/PerimeterUHSHL_Ukraine.shp")

# List of Sentinel-2 files with different time series
s2_files <- list(
  "S2_data_med_CP_2017_2017_7_8_L2A_UTM34N_SA.tif",
  "S2_data_med_CP_2018_2018_7_8_L2A_UTM34N_SA.tif",
  "S2_data_med_CP_2024_2024_7_8_L2A_UTM34N_SA.tif"
)

# Define the NIR and Red band indices
nir_band <- 7
red_band <- 3

# Function to calculate NDVI
ndviCal <- function(red, nir) {
  (nir - red) / (nir + red)
}

# Loop through the files, read the raster, and calculate NDVI
ndvi_list <- list()

for (i in seq_along(s2_files)) {
  # Load the Sentinel-2 raster
  s2_data <- rast(paste0(datadir, s2_files[[i]]))
  
  # Extract the Red and NIR bands
  red <- s2_data[[red_band]]
  nir <- s2_data[[nir_band]]
  
  # Calculate NDVI
  ndvi <- ndviCal(red, nir)
  
  # Store the NDVI result with a name indicating the year
  ndvi_list[[paste0("NDVI_", i)]] <- ndvi
}

# Extracting NDVI for each gaps
# terra::extract

# For test purposes: time series of the NDVI median
ndvi_median_list <- list()

for (i in seq_along(ndvi_list)) {
  # Get the NDVI layer
  ndvi_layer <- ndvi_list[[i]]
  
  # Calculate the median of the NDVI layer
  ndvi_median <- global(ndvi_layer, fun = "mean", na.rm = TRUE)
  
  # Save the NDVI median in a list with sequential numbering
  ndvi_median_list[[paste0("NDVI_median_", i)]] <- ndvi_median
}

# Assuming ndvi_median_list is already populated as shown in your example

# Extracting the mean NDVI values from ndvi_median_list
mean_values <- sapply(ndvi_median_list, function(x) x$mean)

# Create a line plot
ndvi_ts <- plot(x = seq_along(mean_values), 
     y = mean_values,
     type = "o",  # 'o' for connecting points with lines
     pch = 19,    # point character
     col = "blue",
     xlab = "Time Series",
     ylab = "Mean NDVI",
     main = "Mean NDVI Across Time Series")

# Adding points with labels
points(x = seq_along(mean_values), 
       y = mean_values,
       pch = 19,
       col = "blue")

# Adding mean values as text labels
text(x = seq_along(mean_values), 
     y = mean_values, 
     label = round(mean_values, 3), 
     pos = 3,  # position relative to points
     cex = 0.8)  # text size

# Calculate NDVI difference
ndvi_dif <- ndvi_list[[3]]-ndvi_list[[2]]
plot(ndvi_dif, fun = \() lines (aoi))
