library(terra)
library(magrittr)
library(sf)
library(tidyterra)
library(randomForest)
library(dplyr)
library(purrr)
library(ggplot2)

# Set working directory
setwd("E:/_SummerSchool/_SummerSchool/Data/")


# Inputs ------------------------------------------------------------------

dirs <- list.files(path = "Planet/Study_Area/", full.names = T) %>%
  grep(pattern = "_08_mosaic_SA*\\.tif$", value = TRUE)

# Define year for analysis
years <- c(2016:2024)

# Load shapefiles for pastures and boundary
pastures <- sf::st_read("Additional/Pastures.shp") %>%
  sf::st_cast("POLYGON") # Cast geometries as polygons
# boundary <- sf::st_read("Study_Area_Perimeter/PerimeterUHSHL_Ukraine.shp")

# ALTERNATIVE: use smaller boundary
boundary <- sf::st_read("Study_Area_Perimeter/smaller_aoi_reproj.shp") %>% 
  filter(id == 1)

# Create a mask for areas outside the pastures
mask <- sf::st_intersects(boundary, pastures, inverse = TRUE)


# Gap data pre-processing -------------------------------------------------

# Intersect the gap reference with the boundary
Gap_Reference <- vect("Reference/Gaps_Aerial_2017_negbuff_30m.shp") %>%
  st_as_sf() %>%  # Convert to sf object
  sf::st_intersection(boundary)  # Perform intersection with the boundary

# Intersect the non-gap reference with the boundary
NoGap_Reference <- vect("Reference/Non_Gaps_Aerial_2017.shp") %>%
  st_as_sf() %>%  # Convert to sf object
  sf::st_intersection(boundary)  # Perform intersection with the boundary

# Filter references by shape area (greater than 0.2 ha)
Gap_Reference_filtered <- Gap_Reference %>%
  filter(Shape_Area > 2000)
NoGap_Reference_filtered <- NoGap_Reference %>% 
  filter(Shape_Area > 2000)

# Get image from the same year as the gap reference
Gap_img_ref <- rast(dirs[[2]]) %>% 
  terra::crop(boundary) %>% 
  setNames(c("Band_1", "Band_2", "Band_3", "Band_4")) # Rename bands to avoid errors


# Extract values from the raster for both gap and non-gap areas
Gap_df <- terra::extract(Gap_img_ref, Gap_Reference_filtered) %>%
  mutate(Gap = 2) # Assign '2' for gaps

NoGap_df <- terra::extract(Gap_img_ref, NoGap_Reference_filtered) %>%
  mutate(Gap = 1) # Assign '1' for non-gaps

# Combine the gap and non-gap dataframes and convert 'Gap' to a factor
Gap_Ref_df <- bind_rows(Gap_df, NoGap_df) %>%
  mutate(Gap = as.factor(Gap))

# Model Training -----------------------------------------------------------

# Split data into training and validation sets (70% training, 30% validation)
set.seed(123) # Ensure reproducibility
data_set_size <- floor(nrow(Gap_Ref_df) / 10)
indexes <- sample(1:nrow(Gap_Ref_df), size = data_set_size * 7)

training <- Gap_Ref_df[indexes, -1] # Training set
validation <- Gap_Ref_df[-indexes, -1] # Validation set

# Train Random Forest model
model <- randomForest(Gap ~ ., data = training, ntree = 500, importance = TRUE, na.action = na.omit)

# OPTIONAL: since the model creation takes some time, you could save the model as 
#saveRDS(model, "rf_model.rds")
# model <- readRDS(file = "rf_model.rds")
# Clean up memory
gc()

# Gap Prediction -----------------------------------------------------------

# Ensure the result directory exists before the loop
output_dir <- "Result/Gap_predictions_Planet/"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE) # Create the directory if it doesn't exist
}

# Loop over the image directories and generate predictions for each year
for (i in seq_along(dirs)) {
  Img <- rast(dirs[i]) %>%
    setNames(c("Band_1", "Band_2", "Band_3", "Band_4")) # Rename bands to avoid errors
  
  year <- years[i]
  
  # Mask the image by boundary and exclude pasture areas
  Img_masked <- Img %>%
    mask(boundary) %>%
    terra::mask(pastures, inverse = TRUE)
  
  # Predict gaps using the Random Forest model
  pred <- predict(Img_masked, model, type = "response", na.rm = TRUE, progress = "text")
  
  # Save the prediction as a raster
  writeRaster(pred, file.path(output_dir, paste0(year, "_gap_prediction_Planet.tif")), overwrite = TRUE)
}

# Clean data result, isolate single gap pixels -------------------------------------------------------

# Function to reclassify gaps that are isolated as a single cell
reclass_isolated <- function(r) {
  # Reclassify class 2 pixels to NA
  r_reclass <- classify(r, cbind(2, NA))
  
  # Use the focal function to check the neighbors of each pixel (counts only class 1 pixels)
  neighbors <- focal(r_reclass, w = matrix(1, 3, 3), fun = sum, na.rm = TRUE, pad = TRUE, padValue = NA)
  
  # Isolated class 1 pixels have a value of 1 in the focal sum (surrounded by NA or class 2)
  isolated_pixels <- (r == 1) & (neighbors == 1)
  
  # Reclassify isolated pixels to class 2
  r[isolated_pixels] <- 2
  
  return(r)
}

# Define directory
gap_preds_reclassified_path <- paste0(output_dir, "reclassified")
if (!dir.exists(gap_preds_reclassified_path)) {
  dir.create(gap_preds_reclassified_path)
}
gap_preds_path <- list.files(path = output_dir, 
                             pattern = "\\.tif$", full.names = TRUE)

# Loop over each raster file, reclassify isolated pixels, and save the result
for (file in gap_preds_path) {
  # Read the raster
  r <- rast(file)
  
  # Apply the reclassification function
  r_reclass <- reclass_isolated(r)
  
  # Create the output file path inside the "reclassified" subfolder
  output_file <- file.path(gap_preds_reclassified_path, paste0(basename(gsub(".tif", "_reclassified.tif", file))))
  
  # Save the reclassified raster
  writeRaster(r_reclass, output_file, overwrite = TRUE)
}


# Calculate gap dynamic ---------------------------------------------------
# Define paths
gap_diff_path <- "Result/gap_difference/Planet"
if (!dir.exists(gap_diff_path)) {
  dir.create(gap_diff_path)
}

# List of rasters from reclassified gap map
gap_files <- list.files(path = gap_preds_reclassified_path, pattern = "\\.tif$", full.names = TRUE)

# Load all raster files
gap_maps <- lapply(gap_files, rast)

# Define a function to process each year pair
process_gap_dynamics <- function(prev_year_raster, curr_year_raster, prev_year, curr_year) {
  # Identify gap and non-gap areas
  gap_previous <- prev_year_raster == 1
  non_gap_previous <- prev_year_raster == 2
  gap_current <- curr_year_raster == 1
  
  # Identify gap neighbors (potential for expansion)
  gap_neighbors <- focal(gap_previous, w = matrix(1, nrow = 3, ncol = 3), fun = max, na.rm = TRUE, pad = TRUE)
  gap_expansion <- (non_gap_previous & gap_neighbors == 1 & gap_current) # Detect gap expansion
  gap_expansion_mask <- ifel(gap_expansion, TRUE, FALSE)
  
  # Calculate the gap difference between years
  gap_difference <- curr_year_raster - prev_year_raster
  gap_difference <- ifel(gap_expansion_mask, 3, gap_difference) # Mark gap expansion
  
  # Mark unchanged gap areas (no change or reclassified areas)
  gap_reclassification_mask <- (gap_difference == 0) & (gap_previous == 1)
  gap_difference <- ifel(gap_reclassification_mask, 4, gap_difference)
  
  # Save the result for the current year pair
  output_filename <- file.path(gap_diff_path, paste0("gap_difference_", curr_year, ".tif"))
  writeRaster(gap_difference, output_filename, overwrite = TRUE)
  
  return(gap_difference)
}
# Initialize a list to store gap difference results
gap_difference_results <- list()

# Function to extract year from filenames
extract_year_from_filename <- function(filename) {
  sub("^(\\d{4})_gap_prediction.*", "\\1", basename(filename))
}

# Process each pair of consecutive years
for (i in 1:(length(gap_maps) - 1)) {
  prev_raster <- gap_maps[[i]]
  curr_raster <- gap_maps[[i + 1]]
  prev_filename <- gap_files[i]
  curr_filename <- gap_files[i + 1]
  prev_year <- as.numeric(extract_year_from_filename(prev_filename))
  curr_year <- as.numeric(extract_year_from_filename(curr_filename))
  
  # Calculate and store the gap difference between consecutive years
  gap_difference_results[[i]] <- process_gap_dynamics(prev_raster, curr_raster, prev_year, curr_year)
}

# Reclassification: Assign the appropriate categories using a matrix
gap_categories <- data.frame(
  Value = c(1, 2, 3, 4, 5),
  Category = c("new gap", "no change gap", "gap closure", "gap extension", "no forest change")
)

reclass_matrix <- cbind(
  c(-1, 0, 1, 3, 4),  # Original values from gap_difference_results
  c(1, 2, 3, 4, 5)    # New category values based on gap dynamics
)

# Apply reclassification to the gap difference results
gap_difference_results_rcl <- lapply(gap_difference_results, function(x) {
  classify(x, reclass_matrix)
})

# Analyze gap frequencies for visualization
gap_list <- lapply(gap_difference_results_rcl, freq)

# Combine data and prepare for visualization
year_pairs <- paste(years[-length(years)], years[-1], sep = "-")
combined_df <- do.call(rbind, lapply(seq_along(gap_list), function(i) {
  df <- gap_list[[i]]
  df$year_pair <- year_pairs[i]  # Add year pair info
  return(df)
}))

# Merge the gap category labels into the data
combined_df <- combined_df %>%
  left_join(gap_categories, by = c("value" = "Value")) %>% 
  filter(value != 5)

# Create a readable ggplot for gap frequency visualization

ggplot(combined_df, aes(x = year_pair, y = count, group = Category, color = Category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Gap Dynamics Frequency by Year Pairs",
       x = "Year Pair",
       y = "Cell Count",
       color = "Gap Change Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

