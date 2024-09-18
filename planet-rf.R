library(terra)
library(magrittr)
library(sf)
library(tidyterra)
library(randomForest)
library(dplyr)

setwd("E:/_SummerSchool/_SummerSchool/Data/")


# List of the gap polygon from each year
gap_reference_filtered <- vect("E:/_SummerSchool/_SummerSchool/Data/Reference/Gaps_Aerial_2017.shp") %>% 
  tidyterra::filter(Shape_Area > 2000)
non_gap_reference_filtered <- vect("E:/_SummerSchool/_SummerSchool/Data/Reference/Non_Gaps_Aerial_2017.shp")

# List of Planet files with different time series
planet_2017 <- rast("Planet/Study_Area/2017_07_mosaic_SA.tif")
names(planet_2017) <- c("Band_2", "Band_3", "Band_4", "Band_alpha")

aoi <- read_sf("Study_Area_Perimeter/PerimeterUHSHL_Ukraine.shp") %>% 
  st_cast("POLYGON")
gap_df <- terra::extract(planet_2017, gap_reference_filtered) %>% 
  mutate(Gap = 1)
colnames(gap_df) <- c("ID", "Band_2", "Band_3", "Band_4", "Band_alpha", "Gap")

non_gap_df <- terra::extract(planet_2017, non_gap_reference_filtered) %>% 
  mutate(Gap = 2)
colnames(non_gap_df) <- c("ID", "Band_2", "Band_3", "Band_4", "Band_alpha", "Gap")


gap_ref_df <- bind_rows(gap_df, non_gap_df)
gap_ref_df$Gap <- as.factor(gap_ref_df$Gap)

# Do random forest

data_set_size <- floor(nrow(gap_ref_df)/10)
indexes <- sample(1:nrow(gap_ref_df), size = data_set_size*7) # 70% training set, 30% validation
# Assign the data to the correct sets
training <- gap_ref_df[indexes,-1]
validation1 <- gap_ref_df[-indexes,-1]

# model <- randomForest(Gap ~ ., data=training, ntree=500, importance=TRUE, na.action=na.omit)
# saveRDS(model, file = "model_rf")

pred <- predict(planet_2017, model, type="response", na.rm=TRUE, progress="text")

# Clip the extent

vhm <- rast("DSM_VHM_DTM/Study_Area/VHM_0m_60m_SA_5m.tif") %>%
  filter(VHM_0m_60m_SA_5m > 3) %>%
  mask(aoi)
pred_crop <- terra::mask(pred, vhm)
plot(pred_crop)

planet_2018 <- rast("Planet/Study_Area/2018_07_mosaic_SA.tif")
names(planet_2018) <- c("Band_2", "Band_3", "Band_4", "Band_alpha")

pred_2018 <- predict(planet_2018, model, type="response", na.rm=TRUE, progress="text")
pred_2018_crop <- mask(pred_2018, vhm)

planet_2024 <- rast("Planet/Study_Area/2018_07_mosaic_SA.tif")
pred_2024 <- predict()  
# gap change
# Step 1: Identify gap cells in the previous timestep
gap_previous <- pred_crop == 1

# Step 2: Identify non-gap cells in the previous timestep
non_gap_previous <- pred_crop == 2

# Step 3: Identify gap cells in the current timestep
gap_current <- pred_2018_crop == 1

# Step 4: Identify neighboring cells of gaps in the previous timestep
# Use focal to identify adjacent cells to previous gaps
gap_neighbors <- focal(gap_previous, w=matrix(1, nrow=3, ncol=3), fun=max, na.rm=TRUE, pad=TRUE)

# Step 5: Classify gap expansion
# Gap expansion occurs where the previous timestep was non-gap and current is gap, and they are adjacent to previous gaps
gap_expansion <- (non_gap_previous & gap_neighbors == 1 & gap_current)
# Step 1: Create a mask where gap_expansion is TRUE
gap_expansion_mask <- ifel(gap_expansion, TRUE, FALSE)

# Step 2: Reclassify the gap_difference raster where gap_expansion_mask is TRUE
# Assign 3 to gap_difference cells where gap_expansion_mask is TRUE
gap_difference[gap_expansion_mask == 1] <- 3

