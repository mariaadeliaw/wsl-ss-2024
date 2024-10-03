# WSL Summer School on Old Growth Forest Research 2024
## Remote Sensing Group Project

Welcome to the repository for the Remote Sensing Group Project conducted as part of the WSL Summer School on Old Forests 2024. This project aims to identify and analyze gap dynamics in a forested area using a time series of satellite imagery.

### Required Data
To run the provided R scripts, you will need the following inputs:

1. **Gap map reference** (polygon): A reference map that shows the gaps in the forest for a specific year.
2. **Time series satellite imagery** (raster): A set of raster images that capture the forest conditions over multiple years.

### Workflow Overview

1. **Gap Map Generation**:  
   Since the provided gap map is only for a single year, we must generate gap maps for each year in the time series. This is achieved by classifying gaps and non-gaps using the `randomForest` algorithm for each year's satellite image.

2. **Gap Dynamics Classification**:  
   Once the gap maps for each year are generated, we identify the dynamics of the gaps (i.e., how they change over time). This is done by comparing the gaps between consecutive years. Using the `focal` function, we classify the gaps as either new (created in the current year) or expanding (from previous gaps in earlier years).

### Usage
The R script provided in this repository can be run to perform the gap classification and dynamic analysis, however you will need to adjust the input directories.

The Summer School on Old Growth Forest Research is co-organized by the Swiss Federal Research Institute for Forest, Snow and Landscape Research (WSL), Birmensdorf, Switzerland, the Bern University of Applied Sciences (HAFL), Zollikofen, Switzerland, the Ukrainian National Forestry University of Ukraine (UNFU), Lviv, Ukraine, and the Carphatian Biosphere Reserve (CBR), Rakhiv, Ukraine.
This group project is conducted by Thu Uyen Bui, Maria Adelia Widijanto, Richard Slevin, and Anne Huber under the supervision of Marius Rüetschi and M. Hobi

https://www.wsl.ch/en/events-and-courses/summer-school-old-growth-forest-research/

