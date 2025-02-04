library(rhdf5)
library(rstudioapi)
library(openxlsx)
library(readxl)
library(dplyr)
library(terra)
library(raster)
library(sf)
library(matrixStats)
library(tidyr)

# start time
start_time = Sys.time()

# set working directory to the folder of this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
working_dir <- getwd()

#### Set parameters ####

# Crop data year? ('2005', '2010', or '2020')
spam_year <- '2020'

# From the three TRUE/FALSE variables below, only one (or none) can be TRUE for
# running the analysis. For main results, all three should be FALSE

# Use 2005/2010 crop types for the year 2020? (Supplementary analysis)
comb_2020 = FALSE

# only seasonal SCS analysis? (Supplementary analysis)
only_seasonal = FALSE

# maps only, all crops (Only produce mapped results for all crops, including non-food crops)
all_crops = FALSE

# results base path
results_path <- paste(working_dir, '/results_240624_2020/', sep = "" )

#### Do not modify from here on ####

# Outside SCS threshold for Fig 1
out_limit <- c(0.25, 0.5,0.75)

# groupings for results: individual crop groups and/or individual crops

if (spam_year == '2020' & !comb_2020 & !only_seasonal & !all_crops){
  groupings <- c('group', 'food', 'only_soyb', 'only_maize') #, 'nonfood')
  
} else if (only_seasonal) {
  groupings <- c('only_soyb', 'only_maize')
  
} else if (all_crops) {
  groupings <- c('all')
  
} else {
  groupings <- c('food', 'group')
}

all_tables_folder <- paste(results_path, 'tables/', sep="")
if (! file.exists(all_tables_folder)) {
  dir.create(all_tables_folder)
}

# Create folders for grouped results

# create folders for grouped outputs
if (spam_year == '2020' & !comb_2020) {
  group_codes <- read.xlsx('ref_data/fao_food_groups2020.xlsx')

} else if (spam_year == '2020' & comb_2020) {
  group_codes <- read.xlsx('ref_data/fao_food_groups2020_comb.xlsx')
  
} else{
  group_codes <- read.xlsx('ref_data/fao_food_groups.xlsx')
}

crop_group_list <- unique(group_codes$fao_group)

group_list <- c()

# Add maize and/or soy individually in group list for seasonal SCS results if needed
if ('only_maize' %in% groupings & 'only_soyb' %in% groupings) {
  group_list <- c('MAIZ', 'SOYB')
} else if ('only_maize' %in% groupings) {
  group_list <- c('MAIZ')
} else if ('only_soyb' %in% groupings) {
  group_list <- c('SOYB') 
} else if ('all' %in% groupings) {
  group_list <- c('ALL_CROPS')
}

# if crop group level analysis is needed, also add crop groups to group list
if ('group' %in% groupings | 'food' %in% groupings | 'nonfood' %in% groupings) {
  group_list <- c(group_list, crop_group_list)
  
} 

for (group in group_list) {
  
  group_folder <- paste(results_path, group, sep="")
  if (! file.exists(group_folder)) {
    dir.create(group_folder)
  }
  
  matlab_folder <- paste(results_path, 'matlab/', group, sep="")
  if (! file.exists(matlab_folder)) {
    dir.create(matlab_folder)
  }
  
  tables_folder <- paste(results_path, 'tables/', group, sep="")
  if (! file.exists(tables_folder)) {
    dir.create(tables_folder)
  }
}

if ('food' %in% groupings) {
  group_folder <- paste(results_path, 'FOOD', sep="")
  if (! file.exists(group_folder)) {
    dir.create(group_folder)
  }
  
  matlab_folder <- paste(results_path, 'matlab/FOOD', sep="")
  if (! file.exists(matlab_folder)) {
    dir.create(matlab_folder)
  }
  
  tables_folder <- paste(results_path, 'tables/FOOD', sep="")
  if (! file.exists(tables_folder)) {
    dir.create(tables_folder)
  }
}

if ('nonfood' %in% groupings) {
  group_folder <- paste(results_path, 'NONFOOD', sep="")
  if (! file.exists(group_folder)) {
    dir.create(group_folder)
  }
  
  matlab_folder <- paste(results_path, 'matlab/NONFOOD', sep="")
  if (! file.exists(matlab_folder)) {
    dir.create(matlab_folder)
  }
  
  tables_folder <- paste(results_path, 'tables/NONFOOD', sep="")
  if (! file.exists(tables_folder)) {
    dir.create(tables_folder)
  }
}


# remove unecessary variables
rm(list=setdiff(ls(), c("groupings", "working_dir", "group_list", "group_codes", 
                        "results_path", "spam_year", "out_limit", "start_time",
                        "comb_2020", "only_seasonal", "all_crops")))

# read elevation and latitude zones for zonal analysis
df_dem_zones <- read_sf('ref_data/dem_zone_geometries.shp')
df_lat_zones <- read_sf('ref_data/lat_zone_geometries.shp')

# SCS & crop diversity analysis

for (grouping in groupings){

  if (grouping == 'group') {
    groups <- group_list
  } else if (grouping == 'food'){
    groups <- c('FOOD')
  } else if (grouping == 'nonfood'){
    groups <- c('NONFOOD')
  } else if (grouping == "only_maize") {
    groups <- c('MAIZ')
  } else if (grouping == "only_soyb"){
    groups <- c('SOYB')
  } else if (grouping == "all") {
    groups <- c('ALL_CROPS')
  }
  
  for (group in groups) {
    print(group)
    # get crops in crop group
    if (group == 'FOOD' & spam_year != '2020') {
      crop_list <- c('WHEA','RICE','MAIZ','BARL','PMIL','SMIL','SORG','OCER','POTA',
      'SWPO','YAMS','CASS','ORTS','BEAN','CHIC','COWP','PIGE','LENT',
      'OPUL','SOYB','GROU','CNUT','BANA','PLNT','TROF','TEMF','VEGE')
      
    } else if (group == 'FOOD' & spam_year == '2020' & !comb_2020) {
      crop_list <- c('WHEA','RICE','MAIZ','BARL','MILL','PMIL','SORG','OCER','POTA','SWPO',
      'YAMS','CASS','ORTS','BEAN','CHIC','COWP','PIGE','LENT','OPUL',
      'SOYB','GROU','CNUT','BANA','PLNT','CITR','TROF','TEMF','TOMA',
      'ONIO','VEGE')
   
    } else if (group == 'FOOD' & spam_year == '2020' & comb_2020) {
      crop_list <- c('WHEA','RICE','MAIZ','BARL','MILL','PMIL','SORG','OCER','POTA','SWPO',
                     'YAMS','CASS','ORTS','BEAN','CHIC','COWP','PIGE','LENT','OPUL',
                     'SOYB','GROU','CNUT','BANA','PLNT','TROF_COMB','TEMF','VEGE_COMB')
      
    } else if (group == 'NONFOOD' & spam_year != '2020') {
      crop_list <- c('OILP','SUNF','RAPE','SESA','OOIL','SUGC','SUGB','COTT','OFIB',
        'ACOF','RCOF','COCO','TEAS','TOBA','REST')
      
    } else if (group == 'NONFOOD' & spam_year == '2020' & !comb_2020) {
      crop_list <- c('OILP','SUNF','RAPE','SESA','OOIL','SUGC','SUGB',
        'COTT','OFIB','COFF','RCOF','COCO','TEAS','TOBA','RUBB','REST')
      
    } else if (group == 'NONFOOD' & spam_year == '2020' & comb_2020) {
      crop_list <- c('OILP','SUNF','RAPE','SESA','OOIL','SUGC','SUGB',
                     'COTT','OFIB','COFF','RCOF','COCO','TEAS','TOBA','REST_COMB')
      
    } else if (group == 'MAIZ') {
        crop_list <- c('MAIZ')
        
    } else if (group == 'SOYB') {
      crop_list <- c('SOYB')
      
    } else if (group == 'ALL_CROPS' & spam_year == '2020') {
      crop_list <- c('WHEA','RICE','MAIZ','BARL','MILL','PMIL','SORG','OCER','POTA','SWPO',
                     'YAMS','CASS','ORTS','BEAN','CHIC','COWP','PIGE','LENT','OPUL',
                     'SOYB','GROU','CNUT','BANA','PLNT','CITR','TROF','TEMF','TOMA',
                     'ONIO','VEGE', 'OILP','SUNF','RAPE','SESA','OOIL','SUGC','SUGB',
                     'COTT','OFIB','COFF','RCOF','COCO','TEAS','TOBA','RUBB','REST')
    
    } else if (group == 'ALL_CROPS' & spam_year != '2020') {
      crop_list <- c('WHEA','RICE','MAIZ','BARL','PMIL','SMIL','SORG','OCER','POTA',
                     'SWPO','YAMS','CASS','ORTS','BEAN','CHIC','COWP','PIGE','LENT',
                     'OPUL','SOYB','GROU','CNUT','BANA','PLNT','TROF','TEMF','VEGE',
                     'OILP','SUNF','RAPE','SESA','OOIL','SUGC','SUGB','COTT','OFIB',
                     'ACOF','RCOF','COCO','TEAS','TOBA','REST')
        
    } else {
      crop_list <- group_codes %>%
        filter(fao_group == group) %>%
        dplyr::select(spamcrop) %>%
        pull()
    }
    
    
    # initialize table and matrices for storing results from different crops
    # (crop specific + total cropland)
    
    # tables, globally aggregated median SCS results
    columns <- c('CROP', '1.5','1.5_low', '1.5_high', '2', '2_low', '2_high',
                 '3','3_low', '3_high', '4', '4_low', '4_high', 'ref_tot')
    tbl_crop_perc <- data.frame(matrix(ncol = 14, nrow = 0))
    colnames(tbl_crop_perc) <- columns
    
    # tables, all global GCM SCS results
    columns <- c('CROP', '1.5', '2', '3', '4', 'ref_tot', 'GCM')
    tbl_crop_perc_GCMs <- data.frame(matrix(ncol = 7, nrow = 0))
    colnames(tbl_crop_perc_GCMs) <- columns
    
    # tables, regionally aggregated median SCS results
    columns <- c('CROP', '1.5','1.5_low', '1.5_high', '2', '2_low', '2_high',
                 '3','3_low', '3_high', '4', '4_low', '4_high','ref_tot',
                 'region_code', 'REGION_WB')
    tbl_crop_perc_reg <- data.frame(matrix(ncol = 16, nrow = 0))
    colnames(tbl_crop_perc_reg) <- columns
    
    # tables, all regional GCM SCS results
    columns <- c('CROP', '1.5', '2', '3', '4','ref_tot', 'region_code', 'REGION_WB', 'GCM')
    tbl_crop_perc_reg_GCMs <- data.frame(matrix(ncol = 9, nrow = 0))
    colnames(tbl_crop_perc_reg_GCMs) <- columns
    
    # tables, globally aggregated median area SCS results
    columns <- c('CROP', '1.5','1.5_low', '1.5_high', '2', '2_low', '2_high',
                 '3','3_low', '3_high', '4', '4_low', '4_high',
                 'change_dir', 'baseline_area')
    tbl_area_perc <- data.frame(matrix(ncol = 15, nrow = 0))
    colnames(tbl_area_perc) <- columns
    
    # tables, global area SCS results all GCMs
    columns <- c('CROP', 'GCM', '1.5', '2', '3','4', 'change_dir', 'baseline_area')
    tbl_area_perc_GCMs <- data.frame(matrix(ncol = 8, nrow = 0))
    colnames(tbl_area_perc_GCMs) <- columns
    
    # read country id raster, country id table and region shapefile that will
    # be used for regional aggregation
    region_raster_path <- 'ref_data/region_id.tif'
    cntry_region_id_path <- 'ref_data/country_region_codes.xlsx'
    region_geo_path <- 'ref_data/region_geometries.shp'
    
    r_region_id <- rast(region_raster_path)
    cntry_region_id <- read_excel(cntry_region_id_path)
    cntry_region_id <- cntry_region_id %>%
      dplyr::select(region_code, REGION_WB) %>%
      distinct()
    df_region_geom <- read_sf(region_geo_path)
    
    i <- 1
    # analyses for crops in crop group
    for (crop in crop_list) {
      print(crop)
      
      # read crop SCS data
      result_path <- paste(results_path, 'matlab/', crop, '/main_results_div', crop, spam_year, '.mat', sep="")
      
      # read crop prod data
      
      if (group == 'MAIZE' | group == 'SOYB') {
        prod_path <- paste('spam', spam_year, '/', crop, '_cropcal.tif', sep="")
      } else {
        prod_path <- paste('spam', spam_year, '/', crop, '.tif', sep="")
      }

      r_prod <- rast(prod_path)
      prod_sum <- as.numeric(global(r_prod, fun='sum', na.rm=TRUE))
      

      ## SHARE OF PRODUCTION AND AREA OUTSIDE SCS ##
      # combine global aggregates (% outside SCS) + 25th and 75th percentiles to table
      crop_data <- list(crop, median(h5read(result_path, name = '/crop_GCMs_15wp')[,2]),
                        quantile(h5read(result_path, name = '/crop_GCMs_15wp')[,2], probs=0.25)[[1]],
                        quantile(h5read(result_path, name = '/crop_GCMs_15wp')[,2], probs=0.75)[[1]],
                        median(h5read(result_path, name = '/crop_GCMs_2wp')[,2]),
                        quantile(h5read(result_path, name = '/crop_GCMs_2wp')[,2], probs=0.25)[[1]],
                        quantile(h5read(result_path, name = '/crop_GCMs_2wp')[,2], probs=0.75)[[1]],
                        median(h5read(result_path, name = '/crop_GCMs_3wp')[,2]),
                        quantile(h5read(result_path, name = '/crop_GCMs_3wp')[,2], probs=0.25)[[1]],
                        quantile(h5read(result_path, name = '/crop_GCMs_3wp')[,2], probs=0.75)[[1]],
                        median(h5read(result_path, name = '/crop_GCMs_4wp')[,2]),
                        quantile(h5read(result_path, name = '/crop_GCMs_4wp')[,2], probs=0.25)[[1]],
                        quantile(h5read(result_path, name = '/crop_GCMs_4wp')[,2], probs=0.75)[[1]],
                        sum(h5read(result_path, name = '/crop_med_15wp')[1,], na.rm = TRUE)/sum(h5read(result_path, name = '/crop_med_15wp')[2,], na.rm = TRUE))
      
      tbl_crop_perc[i,] <- crop_data
      
      # data from individual GCMs
      GCM_crop <- rep(crop, times = 8)
      GCM = paste0('X', 1:8)
      GCM_1.5 <- h5read(result_path, name = '/crop_GCMs_15wp')[,2]
      GCM_2 <- h5read(result_path, name = '/crop_GCMs_2wp')[,2]
      GCM_3 <- h5read(result_path, name = '/crop_GCMs_3wp')[,2]
      GCM_4 <- h5read(result_path, name = '/crop_GCMs_4wp')[,2]
      ref_tot_GCM <- rep(sum(h5read(result_path, name = '/crop_med_15wp')[1,], na.rm = TRUE)
                         /sum(h5read(result_path, name = '/crop_med_15wp')[2,], na.rm = TRUE),
                         times = 8)
      
      GCM_data <- data.frame(CROP = GCM_crop, `1.5` = GCM_1.5, `2` = GCM_2,
                             `3` = GCM_3, `4` = GCM_4, ref_tot = ref_tot_GCM, GCM) %>%
        setNames(c('CROP', '1.5', '2', '3', '4', 'ref_tot', 'GCM'))
      
      tbl_crop_perc_GCMs <- rbind(tbl_crop_perc_GCMs, GCM_data)
      
      # combine regional aggregates (% outside SCS) + 25th and 75th percentiles to table
      crop_data_region_15wp <- data.frame(h5read(result_path, name = '/region_GCMs_15wp')) %>%
        drop_na() %>%
        rename("region_code" = "X1", "ref_tot" = "X2") %>%
        left_join(cntry_region_id, by = "region_code") %>%
        mutate(`1.5` = rowMedians(as.matrix(.[grep('X\\d+', names(.))]), na.rm = TRUE)) %>%
        bind_cols(as.data.frame(t(apply(as.matrix(.[grep('X\\d+', names(.))]), 1, quantile, c(0.25, 0.75))))) %>%
        dplyr::select(-c(grep('X\\d+', names(.)))) %>%
        rename(`1.5_low` = `25%`,
               `1.5_high` = `75%`)

      crop_data_region_2wp <- data.frame(h5read(result_path, name = '/region_GCMs_2wp')) %>%
        drop_na() %>%
        rename("region_code" = "X1", "ref_tot" = "X2") %>%
        left_join(cntry_region_id, by = "region_code") %>%
        mutate(`2` = rowMedians(as.matrix(.[grep('X\\d+', names(.))]), na.rm = TRUE)) %>%
        bind_cols(as.data.frame(t(apply(as.matrix(.[grep('X\\d+', names(.))]), 1, quantile, c(0.25, 0.75))))) %>%
        dplyr::select(-c(grep('X\\d+', names(.)))) %>%
        rename(`2_low` = `25%`,
               `2_high` = `75%`)

      crop_data_region_3wp <- data.frame(h5read(result_path, name = '/region_GCMs_3wp')) %>%
        drop_na() %>%
        rename("region_code" = "X1", "ref_tot" = "X2") %>%
        left_join(cntry_region_id, by = "region_code") %>%
        mutate(`3` = rowMedians(as.matrix(.[grep('X\\d+', names(.))]), na.rm = TRUE)) %>%
        bind_cols(as.data.frame(t(apply(as.matrix(.[grep('X\\d+', names(.))]), 1, quantile, c(0.25, 0.75))))) %>%
        dplyr::select(-c(grep('X\\d+', names(.))))%>%
        rename(`3_low` = `25%`,
               `3_high` = `75%`)

      crop_data_region_4wp <- data.frame(h5read(result_path, name = '/region_GCMs_4wp')) %>%
        drop_na() %>%
        rename("region_code" = "X1", "ref_tot" = "X2") %>%
        left_join(cntry_region_id, by = "region_code") %>%
        mutate(`4` = rowMedians(as.matrix(.[grep('X\\d+', names(.))]), na.rm = TRUE)) %>%
        bind_cols(as.data.frame(t(apply(as.matrix(.[grep('X\\d+', names(.))]), 1, quantile, c(0.25, 0.75))))) %>%
        dplyr::select(-c(grep('X\\d+', names(.))))%>%
        rename(`4_low` = `25%`,
               `4_high` = `75%`)

      # combined percentiles from the warming scenarios
      crop_perc_regional <- crop_data_region_15wp %>%
        left_join(crop_data_region_2wp, by = c("region_code", "ref_tot", "REGION_WB")) %>%
        left_join(crop_data_region_3wp, by = c("region_code", "ref_tot", "REGION_WB")) %>%
        left_join(crop_data_region_4wp, by = c("region_code", "ref_tot", "REGION_WB")) %>%
        mutate(CROP = crop) %>%
        relocate(CROP, .before = `1.5`) %>%
        relocate(ref_tot, .after = `4_high`)

      tbl_crop_perc_reg <- rbind(tbl_crop_perc_reg, crop_perc_regional)
      
      # regional data from GCMs
      GCM_crop_reg <- rep(crop, times = 64)
      GCM_1.5_reg <- data.frame(h5read(result_path, name = '/region_GCMs_15wp')) %>%
        rename("region_code" = "X1", "ref_tot" = "X2") %>%
        filter(region_code != 107) %>%
        left_join(cntry_region_id, by = "region_code") %>%
        pivot_longer(cols = -c(REGION_WB, region_code, ref_tot),
                     names_to = 'GCMs', 
                     values_to = '1.5') %>%
        dplyr::select(-c(GCMs, REGION_WB, region_code, ref_tot))
      
      GCM_2_reg <- data.frame(h5read(result_path, name = '/region_GCMs_2wp')) %>%
        rename("region_code" = "X1", "ref_tot" = "X2") %>%
        filter(region_code != 107) %>%
        left_join(cntry_region_id, by = "region_code") %>%
        pivot_longer(cols = -c(REGION_WB, region_code, ref_tot),
                     names_to = 'GCMs', 
                     values_to = '2') %>%
        dplyr::select(-c(GCMs, REGION_WB, region_code, ref_tot))
      
      GCM_3_reg <- data.frame(h5read(result_path, name = '/region_GCMs_3wp')) %>%
        rename("region_code" = "X1", "ref_tot" = "X2") %>%
        filter(region_code != 107) %>%
        left_join(cntry_region_id, by = "region_code") %>%
        pivot_longer(cols = -c(REGION_WB, region_code, ref_tot),
                     names_to = 'GCMs', 
                     values_to = '3') %>%
        dplyr::select(-c(GCMs, REGION_WB, region_code, ref_tot))
      
      GCM_4_reg <- data.frame(h5read(result_path, name = '/region_GCMs_4wp')) %>%
        rename("region_code" = "X1", "ref_tot" = "X2") %>%
        filter(region_code != 107) %>%
        left_join(cntry_region_id, by = "region_code") %>%
        pivot_longer(cols = -c(REGION_WB, region_code, ref_tot),
                     names_to = 'GCMs', 
                     values_to = '4') %>%
        relocate(region_code, .after = ref_tot) %>%
        relocate(`4`, .before = ref_tot)
      
      GCM_data_reg <- cbind(GCM_crop_reg, GCM_1.5_reg, GCM_2_reg, GCM_3_reg, GCM_4_reg) %>%
        setNames(c('CROP', '1.5', '2', '3', '4','ref_tot', 'region_code', 'REGION_WB', 'GCM'))
      
      tbl_crop_perc_reg_GCMs <- rbind(tbl_crop_perc_reg_GCMs, GCM_data_reg)

      ## lost vs gained area for individual crops ##
      
      gained_area <- data.frame(h5read(result_path, name = '/area_GCMs_15wp')[,1], h5read(result_path, name = '/area_GCMs_2wp')[,1],
                                h5read(result_path, name = '/area_GCMs_3wp')[,1], h5read(result_path, name = '/area_GCMs_4wp')[,1],
                                h5read(result_path, name = '/area_GCMs_15wp')[,5])
      colnames(gained_area) <- c('1.5', '2', '3', '4', 'baseline')
      gained_area <- gained_area %>%
        mutate_at(vars(`1.5`:`4`), ~ .x / baseline *100)
      
      lost_area <- data.frame(h5read(result_path, name = '/area_GCMs_15wp')[,2], h5read(result_path, name = '/area_GCMs_2wp')[,2],
                              h5read(result_path, name = '/area_GCMs_3wp')[,2], h5read(result_path, name = '/area_GCMs_4wp')[,2],
                              h5read(result_path, name = '/area_GCMs_15wp')[,5])
      colnames(lost_area) <- c('1.5', '2', '3', '4', 'baseline')
      lost_area <- lost_area %>%
        mutate_at(vars(`1.5`:`4`), ~ .x / baseline *100)
      
      total_change <- data.frame(h5read(result_path, name = '/area_GCMs_15wp')[,4], h5read(result_path, name = '/area_GCMs_2wp')[,4],
                                 h5read(result_path, name = '/area_GCMs_3wp')[,4], h5read(result_path, name = '/area_GCMs_4wp')[,4],
                                 h5read(result_path, name = '/area_GCMs_15wp')[,5])
      colnames(total_change) <- c('1.5', '2', '3', '4', 'baseline')
      total_change <- total_change %>%
        mutate_at(vars(`1.5`:`4`), ~ (.x - baseline)/ baseline *100)
        
      
      crop_area_gained <- list(crop, median(gained_area$`1.5`), 
                        quantile(gained_area$`1.5`, probs=0.25)[[1]],
                        quantile(gained_area$`1.5`, probs=0.75)[[1]],
                        median(gained_area$`2`),
                        quantile(gained_area$`2`, probs=0.25)[[1]],
                        quantile(gained_area$`2`, probs=0.75)[[1]],
                        median(gained_area$`3`),
                        quantile(gained_area$`3`, probs=0.25)[[1]],
                        quantile(gained_area$`3`, probs=0.75)[[1]],
                        median(gained_area$`4`),
                        quantile(gained_area$`4`, probs=0.25)[[1]],
                        quantile(gained_area$`4`, probs=0.75)[[1]],
                        'gained', gained_area$baseline[1])
      
      tbl_area_perc[3*i-1, ] <- crop_area_gained
      
      crop_area_lost <- list(crop, median(lost_area$`1.5`), 
                               quantile(lost_area$`1.5`, probs=0.25)[[1]],
                               quantile(lost_area$`1.5`, probs=0.75)[[1]],
                               median(lost_area$`2`),
                               quantile(lost_area$`2`, probs=0.25)[[1]],
                               quantile(lost_area$`2`, probs=0.75)[[1]],
                               median(lost_area$`3`),
                               quantile(lost_area$`3`, probs=0.25)[[1]],
                               quantile(lost_area$`3`, probs=0.75)[[1]],
                               median(lost_area$`4`),
                               quantile(lost_area$`4`, probs=0.25)[[1]],
                               quantile(lost_area$`4`, probs=0.75)[[1]],
                               'lost', lost_area$baseline[1])
      
      tbl_area_perc[3*i-2, ] <- crop_area_lost
      
      crop_area_total <- list(crop, median(total_change$`1.5`), 
                             quantile(total_change$`1.5`, probs=0.25)[[1]],
                             quantile(total_change$`1.5`, probs=0.75)[[1]],
                             median(total_change$`2`),
                             quantile(total_change$`2`, probs=0.25)[[1]],
                             quantile(total_change$`2`, probs=0.75)[[1]],
                             median(total_change$`3`),
                             quantile(total_change$`3`, probs=0.25)[[1]],
                             quantile(total_change$`3`, probs=0.75)[[1]],
                             median(total_change$`4`),
                             quantile(total_change$`4`, probs=0.25)[[1]],
                             quantile(total_change$`4`, probs=0.75)[[1]],
                             'total', total_change$baseline[1])
      
      tbl_area_perc[3*i, ] <- crop_area_total
      
      # gained and lost area from all GCMs
      
      area_data_GCMs <- rbind(gained_area %>% mutate(change_dir = 'gained'),
                                lost_area %>% mutate(change_dir = 'lost'), 
                                total_change %>% mutate(change_dir = 'total'))
      
      area_data_GCMs <- cbind(rep(crop, times = 24), rep(paste0("X", 1:8), times = 3),
                              area_data_GCMs) %>%
        setNames(c("CROP", "GCM", "1.5", "2", "3", "4", "baseline_area", "change_dir")) %>%
        relocate(baseline_area, .after = change_dir)
      
      tbl_area_perc_GCMs <- rbind(tbl_area_perc_GCMs, area_data_GCMs)
      
      ## OUTSIDE/WITHIN SCS RASTERS ##

      # initialize crop specific SCS rasters
      r_SCS_tot_baseline <- rast(res=c(1/12, 1/12), names = c(crop))
      r_SCS_tot_15wp <- rast(res=c(1/12, 1/12), names = c(crop))
      r_SCS_tot_2wp <- rast(res=c(1/12, 1/12), names = c(crop))
      r_SCS_tot_3wp <- rast(res=c(1/12, 1/12), names = c(crop))
      r_SCS_tot_4wp <- rast(res=c(1/12, 1/12), names = c(crop))

      # fill SCS rasters with crop specific data
      values(r_SCS_tot_baseline) <- h5read(result_path, name = '/SCS_out_total_baseline')
      values(r_SCS_tot_15wp) <- h5read(result_path, name = '/SCS_out_total_15wp')
      values(r_SCS_tot_2wp) <- h5read(result_path, name = '/SCS_out_total_2wp')
      values(r_SCS_tot_3wp) <- h5read(result_path, name = '/SCS_out_total_3wp')
      values(r_SCS_tot_4wp) <- h5read(result_path, name = '/SCS_out_total_4wp')

      # reclassify to show whether most models indicate within or outside SCS.
      # 0 = inside, 1 = outside, -9999 = non-cropland
      rcl_mat_out <- matrix(c(1,0,2,0,3,1,4,1, -9999, -9999), nrow = 5, byrow=TRUE)
      r_SCS_tot_baseline <- r_SCS_tot_baseline %>%
        classify(rcl_mat_out)
      r_SCS_tot_15wp <- r_SCS_tot_15wp %>%
        classify(rcl_mat_out)
      r_SCS_tot_2wp <- r_SCS_tot_2wp %>%
        classify(rcl_mat_out)
      r_SCS_tot_3wp <- r_SCS_tot_3wp %>%
        classify(rcl_mat_out)
      r_SCS_tot_4wp <- r_SCS_tot_4wp %>%
        classify(rcl_mat_out)


      # increment raster stacks
      if (i > 1) {
        r_SCS_tot_baseline_stack <- c(r_SCS_tot_baseline_stack, r_SCS_tot_baseline)
        r_SCS_tot_15wp_stack <- c(r_SCS_tot_15wp_stack, r_SCS_tot_15wp)
        r_SCS_tot_2wp_stack <- c(r_SCS_tot_2wp_stack, r_SCS_tot_2wp)
        r_SCS_tot_3wp_stack <- c(r_SCS_tot_3wp_stack, r_SCS_tot_3wp)
        r_SCS_tot_4wp_stack <- c(r_SCS_tot_4wp_stack, r_SCS_tot_4wp)

        r_prod_stack <- c(r_prod_stack, r_prod)


      } else{

        r_SCS_tot_baseline_stack <- r_SCS_tot_baseline
        r_SCS_tot_15wp_stack <- r_SCS_tot_15wp
        r_SCS_tot_2wp_stack <- r_SCS_tot_2wp
        r_SCS_tot_3wp_stack <- r_SCS_tot_3wp
        r_SCS_tot_4wp_stack <- r_SCS_tot_4wp

        r_prod_stack <- r_prod
      }

      #increment index
      i <- i + 1
      
      # remove crop specific data
      rm(r_prod)
      gc()
    }
    
    #load land mask
    if (group == 'MAIZ') {
      land_data_path <- 'holdridge_data_maize/hLand.mat'
    } else if (group == 'SOYB') {
      land_data_path <- 'holdridge_data_soy/hLand.mat'
    } else {
      land_data_path <- 'holdridge_data/hLand.mat'
    }

    land_data <- h5read(land_data_path, name = 'hLand')
    land_data_int <- mapply(land_data, FUN=as.integer)
    land_mat <- matrix(data=land_data_int, ncol=4320, nrow=2160)
    r_land <- rast(res=c(1/12, 1/12))
    terra::values(r_land) <- (land_mat)
    
    # extend prod rasters to match the extent of other data if using spam 2005 data
    if (spam_year == '2005') {
      r_prod_stack <- extend(r_prod_stack, r_land)
    }
    
    # % global and % regional production of all food/nonfood crops outside SCS
    
    if (group %in% c('FOOD', 'NONFOOD')) {
    
    tbl_all_crops_perc <- tbl_crop_perc %>%
      mutate_at(vars(`1.5`:`4_high`), ~ .x * ref_tot) %>%
      summarise_if(is.numeric, sum) %>%
      mutate_at(vars(`1.5`:`4_high`), ~ .x / ref_tot)
    
    tbl_all_crops_perc_GCMs <- tbl_crop_perc_GCMs %>%
      mutate_at(vars(`1.5`:`4`), ~ .x * ref_tot) %>%
      group_by(GCM) %>%
      summarise_if(is.numeric, sum) %>%
      mutate_at(vars(`1.5`:`4`), ~ .x / ref_tot)
    
    tbl_all_crops_perc_reg <- tbl_crop_perc_reg %>%
      dplyr::select(-region_code) %>%
      mutate_at(vars(`1.5`:`4_high`), ~ .x * ref_tot) %>%
      group_by(REGION_WB) %>%
      summarise_if(is.numeric, sum) %>%
      mutate_at(vars(`1.5`:`4_high`), ~ .x / ref_tot)
    
    tbl_all_crops_perc_reg_GCMs <- tbl_crop_perc_reg_GCMs %>%
      dplyr::select(-region_code) %>%
      mutate_at(vars(`1.5`:`4`), ~ .x * ref_tot) %>%
      group_by(REGION_WB, GCM) %>%
      summarise_if(is.numeric, sum, na.rm = TRUE) %>%
      mutate_at(vars(`1.5`:`4`), ~ .x / ref_tot)
    
    out_filename_table_perc_all <- paste(results_path, 'tables/', group, '/SCS_out_perc_all_crops_', group, '.csv', sep="")
    write.csv(tbl_all_crops_perc, out_filename_table_perc_all, row.names=FALSE)
    
    out_filename_table_perc_all_GCMs <- paste(results_path, 'tables/', group, '/SCS_out_perc_all_crops_GCMs_', group, '.csv', sep="")
    write.csv(tbl_all_crops_perc_GCMs, out_filename_table_perc_all_GCMs, row.names=FALSE)
    
    out_filename_table_perc_all_reg <- paste(results_path, 'tables/', group, '/SCS_out_perc_all_crops_regional_', group, '.csv', sep="")
    write.csv(tbl_all_crops_perc_reg, out_filename_table_perc_all_reg, row.names=FALSE)
    
    out_filename_table_perc_all_reg_GCMs <- paste(results_path, 'tables/', group, '/SCS_out_perc_all_crops_regional_GCMs_', group, '.csv', sep="")
    write.csv(tbl_all_crops_perc_reg_GCMs, out_filename_table_perc_all_reg_GCMs, row.names=FALSE)
    
    }
    
    ## % production outside SCS
    # save global aggregated results
    out_filename_table_perc <- paste(results_path, 'tables/', group, '/SCS_out_perc_', group, '.csv', sep="")
    write.csv(tbl_crop_perc, out_filename_table_perc, row.names=FALSE)
    
    # save global GCM results
    out_filename_table_perc_GCMs <- paste(results_path, 'tables/', group, '/SCS_out_perc_GCMs_', group, '.csv', sep="")
    write.csv(tbl_crop_perc_GCMs, out_filename_table_perc_GCMs, row.names=FALSE)
    
    # save regional aggregated results
    out_filename_regional_perc <- paste(results_path, 'tables/', group, '/SCS_out_perc_regional_', group, '.csv', sep="")
    write.csv(tbl_crop_perc_reg, out_filename_regional_perc, row.names=FALSE)
    
    # save regional GCM results
    out_filename_regional_perc_GCMs <- paste(results_path, 'tables/', group, '/SCS_out_perc_regional_GCMs_', group, '.csv', sep="")
    write.csv(tbl_crop_perc_reg_GCMs, out_filename_regional_perc_GCMs, row.names=FALSE)
     
    ## % area outside/within SCS
    # save global aggregated results
    out_filename_table_perc_area <- paste(results_path, 'tables/', group, '/SCS_area_change_perc_', group, '.csv', sep="")
    write.csv(tbl_area_perc, out_filename_table_perc_area, row.names=FALSE)
    
    # save global GCM area results
    out_filename_table_perc_area_GCMs <- paste(results_path, 'tables/', group, '/SCS_area_change_perc_GCMs_', group, '.csv', sep="")
    write.csv(tbl_area_perc_GCMs, out_filename_table_perc_area_GCMs, row.names=FALSE)
    
    #---------------------------------------------------------------------------

    # total cropland mask & cropland area
    r_tot_cropland <- sum((r_SCS_tot_baseline_stack >= 0), na.rm=TRUE) > 0

    r_cropland_area <- rast(paste0('ref_data/total_cropland_area_', spam_year, '.tif'))


    # total cropland area in each region + add row for global total cropland area
    sums_regional_cropland <- terra::extract(r_cropland_area, df_region_geom,
                                             fun = 'sum', na.rm = TRUE) %>%
      rename(area = sum)

    sums_regional_cropland <- sums_regional_cropland %>%
      rbind(data.frame('ID' = (max(sums_regional_cropland$ID) + 1), 'area' = sum(sums_regional_cropland$area, na.rm = TRUE)))

    # total cropland area in latitude and elevation zones
    sums_lat_zone_cropland <- terra::extract(r_cropland_area, df_lat_zones,
                                             fun = 'sum', na.rm = TRUE)%>%
      rename(area = sum)

    sums_lat_zone_cropland <- sums_lat_zone_cropland %>%
      rbind(data.frame('ID' = (max(sums_lat_zone_cropland$ID) + 1), 'area' = sum(sums_lat_zone_cropland$area, na.rm = TRUE)))


    sums_elev_zone_cropland <- terra::extract(r_cropland_area, df_dem_zones,
                                             fun = 'sum', na.rm = TRUE) %>%
      rename(area = sum)

    sums_elev_zone_cropland <- sums_elev_zone_cropland %>%
      rbind(data.frame('ID' = (max(sums_elev_zone_cropland$ID) + 1), 'area' = sum(sums_elev_zone_cropland$area, na.rm = TRUE)))


    # group specific cropland area
    r_spec_mask <- sum((r_prod_stack > 0), na.rm=TRUE) > 0
    r_crop_spec_area <- r_cropland_area * r_spec_mask

    # group specific cropland area in each region + add row for global crop specific cropland area
    sums_spec_cropland <- terra::extract(r_crop_spec_area, df_region_geom,
                                             fun = 'sum', na.rm = TRUE) %>%
      rename(area = sum)

    sums_spec_cropland <- sums_spec_cropland %>%
      rbind(data.frame('ID' = (max(sums_spec_cropland$ID) + 1), 'area' = sum(sums_spec_cropland$area, na.rm = TRUE)))

    # boolean layers of areas within and outside of SCS
    # NOTE: here within SCS and non-cropland are both marked with 0,
    # cropland masking will be done later

    in_tot_baseline <- r_SCS_tot_baseline_stack == 0

    out_tot_15 <- (r_SCS_tot_15wp_stack == 1)
    in_tot_15 <- r_SCS_tot_15wp_stack == 0

    out_tot_2 <- (r_SCS_tot_2wp_stack == 1)
    in_tot_2 <- r_SCS_tot_2wp_stack == 0

    out_tot_3 <- (r_SCS_tot_3wp_stack == 1)
    in_tot_3 <- r_SCS_tot_3wp_stack == 0

    out_tot_4 <- (r_SCS_tot_4wp_stack == 1)
    in_tot_4 <- r_SCS_tot_4wp_stack == 0

    # total reference production within crop group
    r_prod_group <- sum(r_prod_stack, na.rm = TRUE)

    # production outside SCS in warming levels
    prod_SCS_15 <- out_tot_15 * r_prod_stack
    prod_SCS_2 <- out_tot_2 * r_prod_stack
    prod_SCS_3 <- out_tot_3 * r_prod_stack
    prod_SCS_4 <- out_tot_4 * r_prod_stack

    # % group prod outside SCS of group reference prod
    prod_perc_SCS_15 <- sum(prod_SCS_15, na.rm = TRUE) / r_prod_group
    prod_perc_SCS_2 <- sum(prod_SCS_2, na.rm = TRUE) / r_prod_group
    prod_perc_SCS_3 <- sum(prod_SCS_3, na.rm = TRUE) / r_prod_group
    prod_perc_SCS_4 <- sum(prod_SCS_4, na.rm = TRUE) / r_prod_group

    prod_perc <- c(prod_perc_SCS_15, prod_perc_SCS_2, prod_perc_SCS_3,
                   prod_perc_SCS_4)

    # masking non cropland (group specific) and sea
    group_cropland <- sum((r_prod_group > 0), na.rm=TRUE)
    r_landmask <- r_land
    r_landmask[r_land == 1] <- -9999
    prod_perc <- cover(prod_perc, r_landmask)
    prod_perc[group_cropland == 0 & r_landmask == -9999] <- -9999
    prod_perc[r_land == 0] <- NA

    #save result as tiff
    prod_perc_path <- paste(results_path, group, '/perc_prod_out_SCS_', group, '.tiff', sep="")
    writeRaster(prod_perc, prod_perc_path, overwrite=TRUE)

    # warming scenario that would push cell outside SCS ##

    # table for collecting summary statistics
    scens <- list("0", "1.5", "2", "3", "4")
    columns <- c('region', 'out_limit', scens)
    df_first_scen_out_regional <- data.frame(matrix(ncol = 7, nrow = 0))
    colnames(df_first_scen_out_regional) <- columns

    #boolean layers of % production outside SCS, with percentage threshold
    for (lim in out_limit){

      prod_perc_bool <- prod_perc
      prod_perc_bool[prod_perc_bool >= lim] <- 1
      prod_perc_bool[prod_perc_bool < lim] <- 0


      # finding the first scenario that pushes each cell outside of SCS
      first_scen_out <- prod_perc_bool[[1]]

      # 1.5 C is the first scenario to push that cell outside SCS
      first_scen_out[prod_perc_bool[[1]] == 1] <- 1.5

      # 2 C is the first scenario to push that cell outside SCS
      first_scen_out[prod_perc_bool[[1]] == 0 & prod_perc_bool[[2]] == 1 &
                       prod_perc_bool[[3]] == 1 & prod_perc_bool[[4]] == 1] <- 2

      # 3 C is the first scenario to push that cell outside SCS
      first_scen_out[prod_perc_bool[[1]] == 0 & prod_perc_bool[[2]] == 0 &
                       prod_perc_bool[[3]] == 1 & prod_perc_bool[[4]] == 1] <- 3

      # 4 C is the first scenario to push that cell outside SCS
      first_scen_out[prod_perc_bool[[1]] == 0 & prod_perc_bool[[2]] == 0 &
                       prod_perc_bool[[3]] == 0 & prod_perc_bool[[4]] == 1] <- 4

      # masking non cropland and sea
      group_cropland_mask <- cover(group_cropland, r_landmask)
      first_scen_out[group_cropland_mask == -9999] <- -9999
      first_scen_out[group_cropland_mask == 0] <- -9999
      first_scen_out[r_landmask == 0] <- NA

      # save result as tiff
      first_scen_out_path <- paste(results_path, group, '/first_scen_out_SCS_', group,'_', lim, '.tiff', sep="")
      writeRaster(first_scen_out, first_scen_out_path, overwrite=TRUE)

      # regional and global summary statistics

      df_first_scen_out_lim <- data.frame(matrix(ncol = 2, nrow = 0))

      df_first_scen_out_lim[1:nrow(df_region_geom), 1] <- df_region_geom[,1]
      df_first_scen_out_lim[1:nrow(df_region_geom), 2] <- lim
      df_first_scen_out_lim <- df_first_scen_out_lim %>% rbind(data.frame('X1' = 'Global', 'X2' = lim))

      for (scen in scens){

        r_scen_area <- (first_scen_out == as.numeric(scen)) * r_crop_spec_area

        # regional area in category
        scen_areas <- terra::extract(r_scen_area, df_region_geom, fun = 'sum',
                                    na.rm = TRUE)

        # add global area in category, join with region names, transform to % area
        scen_areas <- scen_areas %>%
          rbind(data.frame('ID' = (max(scen_areas$ID)+1), 'sum' = sum(scen_areas$sum, na.rm = TRUE))) %>%
          left_join(sums_spec_cropland, by = "ID") %>%
          mutate(perc = sum/area)

        df_first_scen_out_lim <- df_first_scen_out_lim %>%
          cbind(scen_areas$perc)

      }

        colnames(df_first_scen_out_lim) <- c('region', 'out_limit', scens)
        df_first_scen_out_regional <- rbind(df_first_scen_out_regional, df_first_scen_out_lim)
    }

    # save summary statistics
    reg_first_scen_out_file <- paste(results_path, 'tables/', group, '/first_scen_out_reg_stats_', group, '.csv', sep="")
    write.csv(df_first_scen_out_regional, reg_first_scen_out_file, row.names = FALSE)

    # remove unnecessary variables
    rm(r_prod_stack, out_tot_15, out_tot_2, out_tot_3, out_tot_4,
    prod_SCS_15, prod_SCS_2, prod_SCS_3, prod_SCS_4,
    prod_perc_SCS_15, prod_perc_SCS_2, prod_perc_SCS_3,prod_perc_SCS_4,
    first_scen_out, prod_perc_bool, df_first_scen_out_regional,
    df_first_scen_out_lim)
    gc()

    ## CROP DIVERSITY ##

    #sum matrix layers together to get crop diversity potential
    crops_baseline <- length(crop_list)

    in_div_tot_baseline <- sum(in_tot_baseline, na.rm = TRUE)
    in_div_tot_15 <- sum(in_tot_15, na.rm = TRUE)
    in_div_tot_2 <- sum(in_tot_2, na.rm = TRUE)
    in_div_tot_3 <- sum(in_tot_3, na.rm = TRUE)
    in_div_tot_4 <- sum(in_tot_4, na.rm = TRUE)
    in_div_tot <- c(in_div_tot_15, in_div_tot_2, in_div_tot_3, in_div_tot_4)
    in_div_tot_perc <- in_div_tot / crops_baseline

    # transform in & prob to percentage change compared to baseline
    div_change_tot <- (in_div_tot - in_div_tot_baseline) / in_div_tot_baseline

    # replace inf values in div_change_tot with the future potential crop diversity.
    # These are cropland areas that don't have production in baseline scenario but could
    # in the future. %-change is -1 - 1 and values above 1 areas with emerging climatic potential
    inf_areas <- div_change_tot == Inf
    emerg_potential <- inf_areas * in_div_tot
    div_change_tot[div_change_tot > 1] <- NA
    div_change_tot <- cover(div_change_tot, emerg_potential)

    # Isolate non-land and non cropland areas

    div_change_tot[(r_tot_cropland == 0 & r_land == 1)] <- -9999
    div_change_tot[r_tot_cropland == 0 & r_land == 0] <- NA

    in_div_tot[(r_tot_cropland == 0 & r_land == 1)] <- -9999
    in_div_tot[r_tot_cropland == 0 & r_land == 0] <- NA

    in_div_tot_baseline[(r_tot_cropland == 0 & r_land == 1)] <- -9999
    in_div_tot_baseline[r_tot_cropland == 0 & r_land == 0] <- NA

    in_div_tot_perc[(r_tot_cropland == 0 & r_land == 1)] <- -9999
    in_div_tot_perc[r_tot_cropland == 0 & r_land == 0] <- NA

    # make own category for cropland areas where diversity is 0 in baseline and
    # future scenarios
    for (j in 1:4) {
      change_lyr <- div_change_tot[[j]]
      in_div_lyr <- in_div_tot[[j]]
      change_lyr[in_div_lyr == 0 & in_div_tot_baseline == 0] <- 9999

      if (j > 1){
        div_change_tot_stack <- c(div_change_tot_stack, change_lyr)
      } else {
        div_change_tot_stack <- change_lyr
      }
    }

    # add baseline data as the first layer in in_div_tot
    in_div_tot <- c(in_div_tot_baseline, in_div_tot)

    # save diversity results for crop group
    div_change_filename <- paste(results_path, group, '/diversity_change_total_', group, '.tiff', sep="")
    in_div_tot_filename <- paste(results_path, group, '/in_div_tot_', group, '.tiff', sep="")
    in_div_perc_filename <- paste(results_path, group, '/in_div_perc_', group, '.tiff', sep="")

    writeRaster(div_change_tot_stack, div_change_filename, overwrite=TRUE)
    writeRaster(in_div_tot, in_div_tot_filename, overwrite=TRUE)
    writeRaster(in_div_tot_perc, in_div_perc_filename, overwrite=TRUE)

    # calculate percentage of cropland area in each region, latitude region
    # and elevation zone that will face each category of percentual change
    # in diversity

    #include % change and no diversity croplands, other land area -> NA
    div_perc_nodiv <- div_change_tot_stack
    div_perc_nodiv[div_perc_nodiv == -9999] <- NA

    # loop through change categories
    cats <- list(c(-Inf, -1), c(-0.9999, -0.75), c(-0.75, -0.5), c(-0.5, -0.25),
              c(-0.25, -0.01), c(-0.01, 0.01), c(0.01, 0.25), c(0.25, 0.5), c(0.5, 0.75), c(0.75, 1),
              c(1, crops_baseline), c(crops_baseline, Inf))
    cat_names <- c("-100", "-99.99 to -75", "-75 to -50", "-50 to -25", "-25 to 0", "no change",
                    "0 to +25", "+25 to +50", "+50 to +75", "+75 to +100",
                   "emerging clim potential", "marginal baseline outside SCS scen")

    columns <- c('region', 'scenario', cat_names, 'tot_area_region', 'perc_area_region')
    df_div_cat_change_all <- data.frame(matrix(ncol = 16, nrow = 0))
    colnames(df_div_cat_change_all) <- columns

    if (spam_year == '2020' & !comb_2020) {

      columns_lat <- c('lat_region', 'scenario', cat_names, 'tot_area_region', 'perc_area_region')
      df_div_cat_change_all_lats <- data.frame(matrix(ncol = 16, nrow = 0))
      colnames(df_div_cat_change_all_lats) <- columns_lat

      columns_elev <- c('elev_region', 'scenario', cat_names, 'tot_area_region', 'perc_area_region')
      df_div_cat_change_all_elev <- data.frame(matrix(ncol = 16, nrow = 0))
      colnames(df_div_cat_change_all_elev) <- columns_elev

    }



    for (lyr in 1:4){

      if (lyr > 1) {
        temp = as.character(lyr)
      } else {
        temp = "1.5"
      }
      layer <- div_perc_nodiv[[lyr]]

      # regions
      df_div_cat_change_scen <- data.frame(matrix(ncol = 2, nrow = 0))

      df_div_cat_change_scen[1:nrow(df_region_geom), 1] <- df_region_geom[,1]
      df_div_cat_change_scen[1:nrow(df_region_geom), 2] <- temp
      df_div_cat_change_scen <- df_div_cat_change_scen %>% rbind(data.frame('X1' = 'Global', 'X2' = temp))


      if (spam_year == '2020' & !comb_2020) {

        # latitudes
        df_div_cat_change_scen_lats <- data.frame(matrix(ncol = 2, nrow = 0))

        df_div_cat_change_scen_lats[1:nrow(df_lat_zones), 1] <- df_lat_zones[,1]
        df_div_cat_change_scen_lats[1:nrow(df_lat_zones), 2] <- temp
        df_div_cat_change_scen_lats <- df_div_cat_change_scen_lats %>% rbind(data.frame('X1' = 'Global', 'X2' = temp))

        # elevation zones
        df_div_cat_change_scen_elev <- data.frame(matrix(ncol = 2, nrow = 0))

        df_div_cat_change_scen_elev[1:nrow(df_dem_zones), 1] <- df_dem_zones[,1]
        df_div_cat_change_scen_elev[1:nrow(df_dem_zones), 2] <- temp
        df_div_cat_change_scen_elev <- df_div_cat_change_scen_elev %>% rbind(data.frame('X1' = 'Global', 'X2' = temp))
      }


      for (cat in cats) {

        r_cat_area <- (layer > cat[1] & layer <= cat[2]) * r_cropland_area

        # regional/zonal area in category
        cat_areas <- terra::extract(r_cat_area, df_region_geom, fun = 'sum',
                                    na.rm = TRUE)


        if (spam_year == '2020' & !comb_2020) {

          cat_areas_lat <- terra::extract(r_cat_area, df_lat_zones, fun = 'sum',
                                       na.rm = TRUE)

          cat_areas_dem <- terra::extract(r_cat_area, df_dem_zones, fun = 'sum',
                                          na.rm = TRUE)
        }


        # add global area in category, join with region/zone names, transform to % area
        cat_areas <- cat_areas %>%
          rbind(data.frame('ID' = (max(cat_areas$ID)+1), 'sum' = sum(cat_areas$sum, na.rm = TRUE))) %>%
          left_join(sums_regional_cropland, by = "ID") %>%
          mutate(perc = sum/area)

        df_div_cat_change_scen <- df_div_cat_change_scen %>%
          cbind(cat_areas$perc)


        if (spam_year == '2020' & !comb_2020) {

          cat_areas_lat <- cat_areas_lat %>%
            rbind(data.frame('ID' = (max(cat_areas_lat$ID)+1), 'sum' = sum(cat_areas_lat$sum, na.rm = TRUE))) %>%
            left_join(sums_lat_zone_cropland, by = "ID") %>%
            mutate(perc = sum/area)

          df_div_cat_change_scen_lats <- df_div_cat_change_scen_lats %>%
            cbind(cat_areas_lat$perc)

          cat_areas_dem <- cat_areas_dem %>%
            rbind(data.frame('ID' = (max(cat_areas_dem$ID)+1), 'sum' = sum(cat_areas_dem$sum, na.rm = TRUE))) %>%
            left_join(sums_elev_zone_cropland, by = "ID") %>%
            mutate(perc = sum/area)

          df_div_cat_change_scen_elev <- df_div_cat_change_scen_elev %>%
            cbind(cat_areas_dem$perc)

        }

      }

      # add total and percentual regional/zonal cropland area, clean up colnames
      # and combine scenario results to summary dataframe'
      colnames(df_div_cat_change_scen) <- c('region', 'scenario', cat_names)
      df_div_cat_change_scen <- df_div_cat_change_scen %>%
        mutate(area_region = sums_regional_cropland$area,
               perc_area_region = sums_regional_cropland$area / (sum(sums_regional_cropland$area, na.rm = TRUE)/2))

      colnames(df_div_cat_change_scen) <- c('region', 'scenario', cat_names, 'tot_area_region', 'perc_area_region')
      df_div_cat_change_all <- rbind(df_div_cat_change_all, df_div_cat_change_scen)


      if (spam_year == '2020' & !comb_2020) {

        colnames(df_div_cat_change_scen_lats) <- c('lat_region', 'scenario', cat_names)
        df_div_cat_change_scen_lats <- df_div_cat_change_scen_lats %>%
          mutate(area_region = sums_lat_zone_cropland$area,
                 perc_area_region = sums_lat_zone_cropland$area / (sum(sums_lat_zone_cropland$area, na.rm = TRUE)/2))

        colnames(df_div_cat_change_scen_lats) <- c('lat_region', 'scenario', cat_names, 'tot_area_region', 'perc_area_region')
        df_div_cat_change_all_lats <- rbind(df_div_cat_change_all_lats, df_div_cat_change_scen_lats)

        colnames(df_div_cat_change_scen_elev) <- c('elev_region', 'scenario', cat_names)
        df_div_cat_change_scen_elev <- df_div_cat_change_scen_elev %>%
          mutate(area_region = sums_elev_zone_cropland$area,
                 perc_area_region = sums_elev_zone_cropland$area / (sum(sums_elev_zone_cropland$area, na.rm = TRUE)/2))

        colnames(df_div_cat_change_scen_elev) <- c('elev_region', 'scenario', cat_names, 'tot_area_region', 'perc_area_region')
        df_div_cat_change_all_elev <- rbind(df_div_cat_change_all_elev, df_div_cat_change_scen_elev)

      }
    }

    # save regional diversity change table
    reg_div_change_filename <- paste(results_path, 'tables/', group, '/div_change_regional_area_cat_', group, '.csv', sep="")
    write.csv(df_div_cat_change_all, reg_div_change_filename, row.names = FALSE)


    if (spam_year == '2020' & !comb_2020) {

      elev_div_change_filename <- paste(results_path, 'tables/', group, '/div_change_elev_zones_area_cat_', group, '.csv', sep="")
      write.csv(df_div_cat_change_all_elev, elev_div_change_filename, row.names = FALSE)

      lat_div_change_filename <- paste(results_path, 'tables/', group, '/div_change_lat_zones_area_cat_', group, '.csv', sep="")
      write.csv(df_div_cat_change_all_lats, lat_div_change_filename, row.names = FALSE)
    }

    # remove unnecessary variables
    rm(list=setdiff(ls(), c("groupings", "crop_list", "working_dir",
                            "groups", "group_list", "group_codes", "group", "crop",
                            "results_path", "spam_year", "cntry_region_id",
                            "r_region_id", "df_region_geom", "out_limit",
                            "df_dem_zones", "df_lat_zones", "start_time", "comb_2020",
                            "only_seasonal")))
    gc()

  }
}    

end_time <- Sys.time()
runtime <- end_time - start_time

print(runtime)
