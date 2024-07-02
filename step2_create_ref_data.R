library(terra)
library(dplyr)
library(openxlsx)
library(raster)
library(readxl)
library(sf)
library(tidyr)
library(rnaturalearth)

sf::sf_use_s2(FALSE)

# set working directory to the folder of this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
working_dir <- getwd()

#### COUNTRY AND REGION CODES #####

# read country id raster, country id table and country shapefile that will
# be used for regional aggregation
cntry_raster_path <- 'ref_data/cntry_raster_fao_id_plus.tif'
cntry_id_path <- 'ref_data/fao_ids_plus_A3.xls'
#cntry_geom_path <- 'ref_data/ne_110m_admin_0_countries.shp' 

r_cntry_id <- rast(cntry_raster_path)
cntry_id <- as.data.frame(read_excel(cntry_id_path))
#df_cntry <- read_sf(cntry_geom_path)
df_cntry <- ne_download(scale = 110, type = 'countries', category = "cultural",
                        returnclass = 'sf')

df_cntry_geom <- df_cntry[, c("ADM0_A3", "CONTINENT", "REGION_UN", "SUBREGION", "REGION_WB")]
df_cntry <- st_drop_geometry(df_cntry_geom)

# dissolve country geometries within regional grouping
region_geom <- df_cntry_geom %>%
  group_by(REGION_WB) %>%
  st_make_valid() %>%
  summarize()

# save dissolved geometries
write_sf(region_geom, 'ref_data/region_geometries.shp')

# combine country and region data
cntry_region_codes <- cntry_id %>%
  left_join(df_cntry, by = c("admin_A3" = "ADM0_A3")) %>%
  dplyr::select(new_fao_id_plus, admin_A3, REGION_WB)

# create numerical codes for regions
regions <- unique(cntry_region_codes$REGION_WB)
class_map <- seq(1, length(regions), 1)
names(class_map) <- regions
cntry_region_codes$region_code <- class_map[cntry_region_codes$REGION_WB]

# save region and country code table
write.xlsx(cntry_region_codes, "ref_data/country_region_codes.xlsx")

# reclassification matrix
rcl_matrix <- cntry_region_codes %>%
  dplyr::select(new_fao_id_plus, region_code) %>%
  as.matrix()

# reclassify country id raster
r_region_id <- terra::classify(r_cntry_id, rcl_matrix)

# save region id raster
writeRaster(r_region_id, 'ref_data/region_id.tif', overwrite=TRUE)


#### TOTAL CROPLAND MASK AND PHYSICAL AREA ####

years <- c('2020', '2005', '2010')

for (spam_year in years) {
  
  # crop data folder
  crop_folder <- paste0('spam', spam_year)
  
  #### AREA
  
  # crop data folder
  area_folder <- paste0('spam', spam_year, '/physical_area')
  
  # read all crop data into a stack
  area_rast_list <- list.files(path = area_folder, pattern='.tif$', all.files= T, full.names= T)
  area_stack <- terra::rast(area_rast_list)
  
  # sum area from all crops
  area_crops <- sum(area_stack, na.rm = TRUE)
  area_crops[area_crops == 0] <- NA
  
  # physical cell area in hectares
  #cell_area <- cellSize(area_crops, unit = 'ha')
  
  # cut off crop area with physical cell area
  #area_crops <- min(area_crops, cell_area)
  
  # extend 2005 prod rasters to match the extent of other data if using spam 2005 data
  if (crop_folder == 'spam2005') {
    area_crops <- extend(area_crops, r_cntry_id)
  }
  
  
  ### MASK
  
  # read all crop data into a stack
  rast_list <- list.files(path = crop_folder, pattern='.tif$', all.files= T, full.names= T)
  crop_stack <- terra::rast(rast_list)
  
  # turn into logical raster
  total_crops <- sum(crop_stack > 0, na.rm = TRUE)
  
  # extend 2005 prod rasters to match the extent of other data if using spam 2005 data
  if (crop_folder == 'spam2005') {
    total_crops <- extend(total_crops, r_cntry_id)
  }
  
  # continue turning cropland mask into logical raster
  total_crops[total_crops > 0] <- 1
  total_crops[total_crops <= 0] <- 0
  total_crops[is.na(total_crops)] <- 0
  
  # ensure that cropland area and mask have the same extent
  area_crops[total_crops == 0] <- NA
  total_crops[area_crops == 0] <- 0
  total_crops[is.na(area_crops)] <- 0 
  
  
  # save total cropland mask, cropland = 1 and everything else = 0
  writeRaster(total_crops, paste0('ref_data/total_cropland_mask_', spam_year,'.tif'), overwrite = TRUE)
  
  # save area raster
  writeRaster(area_crops, paste0('ref_data/total_cropland_area_', spam_year,'.tif'), overwrite = TRUE)
  
  
  if (spam_year == '2020') {  
    
    # Create total cropland mask and crop production data that have matching or
    # smaller extent than crop calendar data. Only for maize and soy. 
    
    r_maize <- terra::rast(paste0('spam', spam_year, '/MAIZ.tif'))
    r_soy <- terra::rast(paste0('spam', spam_year, '/SOYB.tif'))
    
    r_cropcal_maize <- terra::rast('crop_calendar/growing_season_filter_mai.tif') %>%
      sum()
    r_cropcal_soy <- terra::rast('crop_calendar/growing_season_filter_soy.tif')%>%
      sum()
    
    r_cropcal_maize[r_cropcal_maize > 0] <- 1
    r_cropcal_soy[r_cropcal_soy > 0] <- 1
    
    # set crop production outside crop cal extent to Na
    r_maize[is.na(r_cropcal_maize[])] <- NA
    r_soy[is.na(r_cropcal_soy[])] <- NA
    
    # set total cropland mask outside crop cal extent to zero
    r_total_cropland_maize <- total_crops
    r_total_cropland_soy <- total_crops
    
    r_total_cropland_maize[is.na(r_cropcal_maize[])] <- 0
    r_total_cropland_soy[is.na(r_cropcal_soy[])] <- 0
    
    # save rasters
    writeRaster(r_maize, paste0('spam', spam_year, '/MAIZ_cropcal.tif'), overwrite = TRUE)
    writeRaster(r_soy, paste0('spam', spam_year, '/SOYB_cropcal.tif'), overwrite = TRUE)
    writeRaster(r_total_cropland_maize, paste0('ref_data/total_cropland_mask_', spam_year, '_maize.tif'), overwrite = TRUE)
    writeRaster(r_total_cropland_soy, paste0('ref_data/total_cropland_mask_', spam_year, '_soy.tif'), overwrite = TRUE)
    
    # Combine tomato and onion with vegetables, citrus fruit with tropical fruit,
    # and rubber with rest of crops for comparison with SPAM 2010 and SPAM 2005 results
    vege_rast_list <- c(paste0('spam', spam_year, '/ONIO.tif'), paste0('spam', spam_year, '/TOMA.tif'),
                        paste0('spam', spam_year, '/VEGE.tif'))
    fruit_rast_list <- c(paste0('spam', spam_year, '/CITR.tif'), paste0('spam', spam_year, '/TROF.tif'))
    rest_rast_list <- c(paste0('spam', spam_year, '/RUBB.tif'), paste0('spam', spam_year, '/REST.tif'))
    
    r_vege <- terra::rast(vege_rast_list)
    r_fruit <- terra::rast(fruit_rast_list)
    r_rest <- terra::rast(rest_rast_list)
    
    r_vege_sum <- sum(r_vege, na.rm = TRUE)
    r_fruit_sum <- sum(r_fruit, na.rm = TRUE)
    r_rest_sum <- sum(r_rest, na.rm = TRUE)
    
    # save rasters
    writeRaster(r_vege_sum, paste0('spam', spam_year, '/VEGE_COMB.tif'), overwrite = TRUE)
    writeRaster(r_fruit_sum, paste0('spam', spam_year, '/TROF_COMB.tif'), overwrite = TRUE)
    writeRaster(r_rest_sum, paste0('spam', spam_year, '/REST_COMB.tif'), overwrite = TRUE)
    
    
  }
  
}


#### LATITUDE ZONES POLYGONS ####

# initialize cell values from y axis
r_lats <- total_crops
r_lats <- init(r_lats, fun='y')

# reclassify to latitude categories
rcl_mat <- matrix(c(-90,-30,-70,
                    -30,30,0,
                    30,90,70), nrow = 3, byrow = TRUE)

r_lat_zones <- classify(r_lats, rcl_mat)
df_lat_zones <- st_as_sf(terra::as.polygons(r_lat_zones))

writeRaster(r_lat_zones, 'ref_data/lat_zones.tif', overwrite = TRUE)
write_sf(df_lat_zones, 'ref_data/lat_zone_geometries.shp')

## UPSCALING DEM, ELEVATION ZONES ##

# read high resolution global DEM
r_dem_30s <- rast('ref_data/hyd_glo_dem_30s/hyd_glo_dem_30s.tif')

# resample to 5 arc min, using total cropland mask
r_dem_5min <- resample(r_dem_30s, total_crops, method = 'bilinear')

# elevation zones
rcl_mat_dem <- matrix(c(-Inf,800,1,
                        800,1500,2,
                        1500,2500,3,
                        2500,Inf,4), nrow = 4, byrow = TRUE)
r_dem_zones <- classify(r_dem_5min, rcl_mat_dem)

df_dem_zones <- st_as_sf(terra::as.polygons(r_dem_zones))

# save 5 arc min dem and elevation zone raster
writeRaster(r_dem_5min, 'ref_data/dem_5min.tif', overwrite = TRUE)
writeRaster(r_dem_zones, 'ref_data/dem_zones.tif', overwrite = TRUE)
write_sf(df_dem_zones, 'ref_data/dem_zone_geometries.shp')


