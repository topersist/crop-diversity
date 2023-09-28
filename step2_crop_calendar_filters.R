library(ncdf4)
library(terra)
library(dplyr)
library(raster)

# set working directory to the folder of this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
working_dir <- getwd()

# list of crops
crops <- c('bar', 'bea', 'cas', 'mai', 'mil', 'pea', 'pot', 'ri1', 'ri2', 
           'sor', 'soy', 'swh', 'wwh')

for (crop in crops) {
  
  # read crop calendar data for rainfed and irrigated systems, select
  # planting day and maturity day layers
  r_ir <- rast(paste0("crop_calendar/", crop, "_ir_ggcmi_crop_calendar_phase3_v1.01.nc4")) %>%
    subset(c('planting_day', 'maturity_day'))
  r_rf <- rast(paste0("crop_calendar/", crop, "_rf_ggcmi_crop_calendar_phase3_v1.01.nc4"))%>%
    subset(c('planting_day', 'maturity_day'))
  
  # separate between winter and summer growing season
  r_ir_summer_gs_plnt_day <- (r_ir$planting_day < r_rf$maturity_day) *  r_ir$planting_day
  r_ir_summer_gs_mat_day <- (r_ir$planting_day < r_rf$maturity_day) *  r_ir$maturity_day
  
  r_ir_winter_gs_plnt_day <- (r_ir$planting_day > r_rf$maturity_day) *  r_ir$planting_day
  r_ir_winter_gs_mat_day <- (r_ir$planting_day > r_rf$maturity_day) *  r_ir$maturity_day
  
  r_rf_summer_gs_plnt_day <- (r_rf$planting_day < r_rf$maturity_day) *  r_rf$planting_day
  r_rf_summer_gs_mat_day <- (r_rf$planting_day < r_rf$maturity_day) *  r_rf$maturity_day
  
  r_rf_winter_gs_plnt_day <- (r_rf$planting_day > r_rf$maturity_day) *  r_rf$planting_day
  r_rf_winter_gs_mat_day <- (r_rf$planting_day > r_rf$maturity_day) *  r_rf$maturity_day
  
  # combine rainfed and irrigated by selecting the earlier planting day and
  # later maturity day in cell
  r_summer_gs_plnt_day <- min(c(r_ir_summer_gs_plnt_day, r_rf_summer_gs_plnt_day))
  r_winter_gs_plnt_day <- min(c(r_ir_winter_gs_plnt_day, r_rf_winter_gs_plnt_day))
  
  r_summer_gs_mat_day <- max(c(r_ir_summer_gs_mat_day, r_rf_summer_gs_mat_day))
  r_winter_gs_mat_day <- max(c(r_ir_winter_gs_mat_day, r_rf_winter_gs_mat_day))
  
  r_summer_days <- c(r_summer_gs_plnt_day, r_summer_gs_mat_day)
  r_winter_days <- c(r_winter_gs_plnt_day, r_winter_gs_mat_day)
  
  names(r_summer_days) <- c('plnt_day', 'mat_day')
  names(r_winter_days) <- c('plnt_day', 'mat_day')
  
  # reclassification matrix (from-to-becomes), day of year to month
  rcl_mat <- matrix(c(-Inf,0,NA,
                    1, 31, 1,
                    32, 59, 2,
                    60, 90, 3,
                    91, 120, 4,
                    121, 151, 5,
                    152, 181, 6,
                    182, 212, 7,
                    213, 243, 8,
                    244, 273, 9,
                    274, 304, 10,
                    305, 334, 11,
                    335, 365, 12),
                    ncol = 3, byrow = TRUE)
  
  # transform day of year unit to month
  r_summer_months <- classify(r_summer_days, rcl_mat, include.lowest = TRUE,
                                     right = NA)
  r_winter_months <- classify(r_winter_days, rcl_mat, include.lowest = TRUE,
                                     right = NA)
  names(r_summer_months) <- c('plnt_m', 'mat_m')
  names(r_winter_months) <- c('plnt_m', 'mat_m')
  
  # downscale to 5 arc min resolution
  r_summer_months_ds <- disagg(r_summer_months, fact = 6)
  r_winter_months_ds <- disagg(r_winter_months, fact = 6)
  
  # create raster stack to be used for filtering growing season months
  # from climate data
  
  r_month_stack <- rep(r_summer_months_ds, 6)
  names(r_month_stack) <- paste0('m_',(seq(1,12,1)))
  r_month_stack <- setValues(r_month_stack, NaN)
  
  # in each layer, set cell value to 1 if that month is within the
  # growing season in the cell
  for (i in 1:12) {
  
    r_within_summer_gs <- (r_summer_months_ds$plnt_m <= i & r_summer_months_ds$mat_m >= i) * 1
    r_within_winter_gs <- (r_winter_months_ds$mat_m >= i | r_winter_months_ds$plnt_m <= i) * 1
    
    r_within_gs <- sum(r_within_summer_gs, r_within_winter_gs, na.rm = TRUE)
      
    r_month_stack[[i]] <- r_within_gs
  }
  
  names(r_month_stack) <- paste0('m_', seq(1,12,1))
  
  writeRaster(r_month_stack, paste0('crop_calendar/growing_season_filter_', crop,  '.tif'), overwrite = TRUE)

}