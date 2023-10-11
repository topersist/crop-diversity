library(terra)
library(dplyr)
library(openxlsx)
library(raster)
library(readxl)
library(sf)
library(tidyr)
library(rgeos)
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

#### TOTAL CROPLAND MASK ####

# crop data folder
crop_folder <- 'spam2010'

# read all crop data into a stack
rast_list <- list.files(path = crop_folder, pattern='.tif$', all.files= T, full.names= T)
crop_stack <- terra::rast(rast_list)
  
# turn into logical raster
total_crops <- sum(crop_stack > 0)

# extend 2005 prod rasters to match the extent of other data if using spam 2005 data
if (crop_folder == 'spam2005') {
  total_crops <- extend(total_crops, r_cntry_id)
}

# continue turning into logical raster
total_crops[total_crops > 0] <- 1
total_crops[total_crops <= 0] <- 0
total_crops[is.na(total_crops)] <- 0

# save total cropland mask, cropland = 1 and everything else = 0
writeRaster(total_crops, 'ref_data/total_cropland_mask_2010.tif', overwrite = TRUE)

#### TOTAL CROPLAND MASK AND PRODUCTION AREAS FOR SEASONAL SCS ####

# Create total cropland mask and crop production data that have matching or
# smaller extent than crop calendar data. Only for maize and soy. 

r_maize <- terra::rast('spam2010/MAIZ.tif')
r_soy <- terra::rast('spam2010/SOYB.tif')

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
writeRaster(r_maize, 'spam2010/MAIZ_cropcal.tif', overwrite = TRUE)
writeRaster(r_soy, 'spam2010/SOYB_cropcal.tif', overwrite = TRUE)
writeRaster(r_total_cropland_maize, 'ref_data/total_cropland_mask_2010_maize.tif', overwrite = TRUE)
writeRaster(r_total_cropland_soy, 'ref_data/total_cropland_mask_2010_soy.tif', overwrite = TRUE)

## LATITUDE ZONES POLYGONS ##

# initialize cell values from y axis
r_lats <- total_crops
r_lats <- init(r_lats, fun='y')

# reclassify to latitude categories
rcl_mat <- matrix(c(-90,-40,-70,
                    -40,40,0,
                    40,90,70), nrow = 3, byrow = TRUE)

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

