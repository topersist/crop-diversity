library(ggplot2)
library(dplyr)
library(tidyverse)
library(tmap)
library(terra)
library(sf)
library(rnaturalearth)

# set working directory to the folder of this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
working_dir <- getwd()

# crops included in comparison
compare_crops <- c('MAIZ', 'SOYB')

for (crop in compare_crops) {

  # folders for annual and seasonal approach SCS results
  results_path_ann <- paste0(working_dir, '/results_240624_2020/')
  results_path_seas <- paste0(working_dir, '/results_260624_2020_cropcal/')
  
  ## create tables comparing effects to current production at global and regional level ##
  
  # annual SCS, prepare for combining with seasonal
  perc_out_SCS_ann_global <- read.csv(paste0(results_path_ann, 'tables/', crop, 
                                            '/SCS_out_perc_', crop, '.csv')) %>%
                            mutate(region = 'Global') %>%
                            dplyr::select(-c(ends_with('_low'), ends_with('_high'), ref_tot, CROP)) %>%
                            relocate(region)
  
  perc_out_SCS_ann_regional <- read.csv(paste0(results_path_ann, 'tables/', crop, 
                                            '/SCS_out_perc_regional', crop, '.csv')) %>%
                              dplyr::select(-c(region_code, ref_tot, CROP, ends_with('_low'), ends_with('_high'))) %>%
                              arrange(REGION_WB) %>%
                              rename(region = REGION_WB) %>%
                              relocate(region)
    
  perc_out_SCS_ann <- rbind(perc_out_SCS_ann_global, perc_out_SCS_ann_regional) %>%
    rename_with(~ gsub("X", "ann_", .x, fixed = TRUE))
  
  # seasonal SCS, prepare for combining with seasonal
  perc_out_SCS_seas_global <- read.csv(paste0(results_path_seas, 'tables/', crop, 
                                              '/SCS_out_perc_', crop, '.csv')) %>%
                              mutate(region = 'Global') %>%
                              dplyr::select(-c(ends_with('_low'), ends_with('_high'), ref_tot, CROP)) %>%
                              relocate(region)
  
  perc_out_SCS_seas_regional <- read.csv(paste0(results_path_seas, 'tables/', crop, 
                                              '/SCS_out_perc_regional', crop, '.csv')) %>%
                              dplyr::select(-c(region_code, ref_tot, CROP, ends_with('_low'), ends_with('_high'))) %>%
                              arrange(REGION_WB) %>%
                              rename(region = REGION_WB) %>%
                              relocate(region)
  
  perc_out_SCS_seas <- rbind(perc_out_SCS_seas_global, perc_out_SCS_seas_regional) %>%
    rename_with(~ gsub("X", "seas_", .x, fixed = TRUE))
  
  # construct comparison table for supplement
  perc_out_SCS <- merge(perc_out_SCS_ann, perc_out_SCS_seas, by = 'region', all = TRUE) %>%
    dplyr::relocate(c(ends_with('_1.5'), ends_with('_2'), ends_with('_3'), 
                      ends_with('_4'))) %>%
    dplyr::relocate(region)
  
  # save as csv
  write.csv(perc_out_SCS, paste0(results_path_seas, 'tables/', crop, 
                                 '/SCS_out_perc_comparison_', crop, '.csv'))
  
  # map on effects to climatically suitable cropland area
  
  # read total diversity data
  r_div_ann <- rast(paste0(results_path_ann, crop, '/in_div_tot_', crop, '.tiff'))
  r_div_seas <- rast(paste0(results_path_seas, crop, '/in_div_tot_', crop, '.tiff'))
  
  # read coastline
  #coastline <- read_sf('ref_data/ne_50m_coastline.shp')
  coastline <- ne_download(scale = 50, type = "coastline",
                           category = "physical", returnclass = "sf")
  
  # combined category mapping for each warming level
  
  for (i in 1:4) {
    
    layer_ann <- r_div_ann[[i]]
    layer_seas <- r_div_seas[[i]]
    
    if (i > 1) {
      temp = as.character(i)
    } else {
      temp = "1.5"
    }
    
    # combined categorization for rasters
    r_div_comb <- layer_ann
    r_div_comb[layer_ann == 1 & layer_seas == 1] <- 1 # both ann and seas say in
    r_div_comb[layer_ann == 0 & layer_seas == 0] <- 2 # both ann and seas say out
    r_div_comb[layer_ann == 1 & layer_seas == 0] <- 3 # ann in, seas out
    r_div_comb[layer_ann == 0 & layer_seas == 1] <- 4 # ann out, seas in
    
    # project raster to Robinson proj
    #r_div_rob <- terra::project(r_div_comb, "+proj=robin +over", mask = TRUE)
    
    # save raster
    r_path <- paste0(results_path_seas, crop, '/SCS_comparison_seas_annual', temp, '_', crop, '.tiff')
    writeRaster(r_div_comb, r_path, overwrite=TRUE)
    
    # crop raster to exclude Antarctica from map
    r_div_comb <- crop(r_div_comb, ext(-180, 180, -56, 90))
    
    # create map
    plt_div_seas_ann <- tm_shape(r_div_comb) +
      tm_raster(title = "Within and outside\nSCS in annual (ann)\nand seasonal (seas) approach",
                breaks = c(-Inf, -9999, 1, 2, 3, 4),
                interval.closure = "right",
                palette = c("white", "#AA4499","#DDCC77","#332288", "#88CCEE"),
                labels = c("non-cropland", "both in", "both out", "ann in, seas out", "seas in, ann out")) +
      tm_shape(coastline)+#, projection = "ESRI:54030") +
      tm_lines(col = "black", lwd = 0.25)+
      tm_layout(frame = FALSE, legend.position = c('left', 'bottom'))
    
    plt_div_seas_ann
    
    save_path <- paste0(results_path_seas, crop, '/SCS_comparison_seas_annual', temp, '_', crop, '.pdf')
    tmap_save(plt_div_seas_ann, save_path)
    
  }
    
}

