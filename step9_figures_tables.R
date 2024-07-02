library(ggplot2)
library(dplyr)
library(tidyr)
library(openxlsx)
library(forcats)
library(colorspace)
library(scico)
library(tmap)
library(terra)
library(matrixStats)
library(raveio)
library(RColorBrewer)
library(rnaturalearth)
library(sf)
library(ggtern)
library(reshape2)
library(ggalt)

#### FUNCTIONS SECTION BEGINS, do not modify ####

# bar plots of % outside SCS at five global warming levels
plot_bar <- function(table_long, group, results_path, pal, aggreg = '', food_list, area) {
  
  # plot labels
  lbl_ctgs <- unique(table_long$CROP)
  ncrops <- length(lbl_ctgs)
  
  # plot color palettes
  if (group == 'CEREALS') {
    pal_range <- colorRampPalette(c(lighten(pal['CEREALS1'], 0.5), darken(pal['CEREALS1'], 0.5),
                                    lighten(pal[group], 0.5), darken(pal[group], 0.5)))
    pal_ctgs <- pal_range(ncrops)
    legend_grouping = 'Crop'
    
  } else if (group == 'FRUIT AND VEG') { 

      pal_range <- colorRampPalette(c(lighten(pal['FRUIT AND VEG1'], 0.5), darken(pal['FRUIT AND VEG1'], 0.5),
                                      lighten(pal[group], 0.5), darken(pal[group], 0.5)))
      pal_ctgs <- pal_range(ncrops)
      legend_grouping = 'Crop'
    
  } else if (! group %in% c("FOOD", "NONFOOD")) {
    group_color <- pal[group]
    pal_range <- colorRampPalette(c(lighten(group_color, 0.7), group_color, darken(group_color, 0.7)))
    pal_ctgs <- pal_range(ncrops)
    legend_grouping = 'Crop'
    
  } else if (group == "FOOD" && aggreg != '') {
    pal_ctgs <- pal[food_list]
    #pal_ctgs <- pal_range(ncrops)
    legend_grouping = 'Group'
    
  } else if (group == "NONFOOD" && aggreg != '') {
    pal_ctgs <- pal[!(names(pal) %in% food_list)]
    #pal_ctgs <- pal_range(ncrops)
    legend_grouping = 'Group'
  }
  
  # create a bar plot
  SCS_table_long_desc <- table_long %>%
    mutate(warming = as.character(warming))
  
  plt_out_SCS_group_col <- ggplot(data = SCS_table_long_desc,
                                  aes(x = warming, y = perc_out_SCS,
                                      group=CROP)) + 
    geom_col(data = SCS_table_long_desc, aes(x = warming, y = perc_out_SCS,
                 fill= CROP), position = 'dodge') +
    geom_errorbar(data = SCS_table_long_desc, aes(x = warming,
                      ymin=limit_low, ymax=limit_high),
                  position = position_dodge(width=0.9),
                  width = 0.25) +
    scale_fill_manual(name = paste(legend_grouping, '(median)', sep = " "),
                      values = pal_ctgs) +
    scale_x_discrete(expand = c(0,0),
                     labels = c("1.5 C","2 C", "3 C", "4 C")) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,100)) + 
    ylab("% of current production outside SCS") +
    xlab("Global warming") +
    ggtitle(paste(group, area, sep = " ")) +
    theme(plot.background = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(colour = 'grey70'), panel.grid.major.x = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  plot(plt_out_SCS_group_col)
  
  # save bar plots
  bar_plot_file_pdf <- paste(results_path, group, '/perc_out_SCS_bar_', group, aggreg, '_', area, '.pdf', sep="")
  bar_plot_file_png <- paste(results_path, group, '/perc_out_SCS_bar_', group, aggreg, '_', area, '.png', sep="")
  ggsave(bar_plot_file_pdf, plt_out_SCS_group_col, height = 9.5 , width = 16.5, units = "cm")
  ggsave(bar_plot_file_png, plt_out_SCS_group_col, height = 9.5 , width = 16.5, units = "cm")

}

# bar plots of gained and lost potential cropland area at four global warming levels
plot_area_change <- function(table_long, group, results_path, pal, aggreg = '', food_list, area) {
  
  # plot labels
  lbl_ctgs <- unique(table_long$CROP)
  ncrops <- length(lbl_ctgs)
  
  # plot color palettes
  if (group == 'CEREALS') {
    pal_range <- colorRampPalette(c(lighten(pal['CEREALS1'], 0.5), darken(pal['CEREALS1'], 0.5),
                                    lighten(pal[group], 0.5), darken(pal[group], 0.5)))
    pal_ctgs <- pal_range(ncrops)
    legend_grouping = 'Crop'
    
  } else if (group == 'FRUIT AND VEG') { 
    
    pal_range <- colorRampPalette(c(lighten(pal['FRUIT AND VEG1'], 0.5), darken(pal['FRUIT AND VEG1'], 0.5),
                                    lighten(pal[group], 0.5), darken(pal[group], 0.5)))
    pal_ctgs <- pal_range(ncrops)
    legend_grouping = 'Crop'
    
  }else if (! group %in% c("FOOD", "NONFOOD")) {
    group_color <- pal[group]
    pal_range <- colorRampPalette(c(lighten(group_color, 0.7), group_color, darken(group_color, 0.7)))
    pal_ctgs <- pal_range(ncrops)
    legend_grouping = 'Crop'
    
  } else if (group == "FOOD" && aggreg != '') {
    pal_ctgs <- pal[food_list]
    #pal_ctgs <- pal_range(ncrops)
    legend_grouping = 'Group'
    
  } else if (group == "NONFOOD" && aggreg != '') {
    pal_ctgs <- pal[!(names(pal) %in% food_list)]
    #pal_ctgs <- pal_range(ncrops)
    legend_grouping = 'Group'
  }
  
  
  # data adjustment for plot aesthetics
  area_table_long_desc <- table_long %>%
    mutate(warming = as.character(warming),
           perc_change_area = ifelse(change_dir == "lost", -perc_change_area, perc_change_area),
           limit_low = ifelse(change_dir == "lost", -limit_low, limit_low),
           limit_high = ifelse(change_dir == "lost", -limit_high, limit_high))
  
  area_table_total <- table_long %>%
    filter(change_dir == 'total') %>%
    mutate(warming = as.character(warming))
  
  min_low <- min(area_table_long_desc$limit_high)
  max_high <- max(area_table_long_desc$limit_high)
  
  # create plot
  plt_area_change<- ggplot(data = area_table_long_desc,
                                  aes(x = warming, y = perc_change_area,
                                      group=CROP)) + 
    geom_col(data = area_table_long_desc, aes(x = warming, y = perc_change_area,
                                             fill= CROP), position = 'dodge') +
    geom_errorbar(data = area_table_long_desc, aes(x = warming,
                                                  ymin=limit_low, ymax=limit_high,
                                                  group = CROP),
                  position = position_dodge(width=0.9),
                  width = 0.25) +
    geom_point(data = area_table_total, aes(x = warming, y = perc_change_area,
                                            group=CROP), 
               position = position_dodge(width=0.9), shape = 18, color = "white")+
    geom_errorbar(data = area_table_total, aes(x = warming,
                                               ymin=limit_low, ymax=limit_high,
                                               group = CROP),
                  position = position_dodge(width=0.9),
                  width = 0.25, color = "white") +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    scale_fill_manual(name = paste(legend_grouping, '(median)', sep = " "),
                      values = pal_ctgs) +
    scale_x_discrete(expand = c(0,0),
                     labels = c(c("1.5 C","2 C", "3 C", "4 C"))) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(-80, 40)) + 
    ylab("Change in cropland area within SCS compared to baseline (%)") +
    xlab("Global warming") +
    ggtitle(paste(group, area, sep = " ")) + 
    theme(plot.background = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = 'grey80'),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  plot(plt_area_change)
  
  # save bar plots
  bar_plot_file_pdf <- paste(results_path, group, '/lost_gained_area_bar_', group, aggreg, '_', area, '.pdf', sep="")
  bar_plot_file_png <- paste(results_path, group, '/lost_gained_area_bar_', group, aggreg, '_', area, '.png', sep="")
  ggsave(bar_plot_file_pdf, plt_area_change, height = 16.5 , width = 16.5, units = "cm")
  ggsave(bar_plot_file_png, plt_area_change, height = 16.5 , width = 16.5, units = "cm")
  
}

# vertical bar plots of net change in cropland area within SCS by crop
# crop diversity potential maps, % change
plot_diversity <- function(r_input, group, ncrops, temp, results_path, coastline, pal = 'vik', tocrs = NA, r_land) {
  
  # crop raster to exclude Antarctica from map
  r_input <- terra::crop(r_input, ext(-180, 180, -56, 90))
  r_land <- terra::crop(r_land, ext(-180, 180, -56, 90))
  
  # change -9999 (non-cropland) to NA, plot land from a separate raster
  r_input[r_input == -9999] <- NA
  r_land[r_land == 0] <- NA
  
  # project to another crs
  if (!is.na(tocrs)){
    r_input <- terra::project(r_input, tocrs, mask = TRUE)
    r_land <-  terra::project(r_land, tocrs, mask = TRUE)
  }
  
  # create breaks and palette
  breakvals <-  c(-1.001, -0.999, -0.75, -0.5, -0.25, -0.01, 0.01, 0.25, 0.5, 0.75, ncrops, ncrops + 1, Inf)
  breaklab <- c("-100", "-99.99 to -75", "-75 to -50", "-50 to -25", "-25 to 0", "no change",
                "0 to +25", "+25 to +50", "+50 to +75", "+75 to +100", "cropland with emerging climatic potential",
                "marginal in baseline, outside SCS at warming level")
  
  cols <- scico(n = 10, palette = pal, direction = -1)
  colorpal <- c(cols, "orangered", "grey60")
  
  # create tmap object
  diversity_map <-  tm_shape(r_land) +
    tm_raster(palette = "white",
              showNA = F,
              colorNA = NULL,
              title = 'Other land areas',
              labels = c('Non-cropland')) +
    tm_shape(r_input) +
    tm_raster(palette = colorpal,
              breaks = breakvals,
              labels = breaklab,
              midpoint = 0,
              interval.closure = 'right',
              legend.reverse = TRUE,
              showNA = F,
              colorNA = NULL,
              title = "% Change in crop diversity potential") + 
    tm_shape(coastline, projection = "ESRI:54030") + 
    tm_lines(col = "black", lwd = 0.25)
  
  diversity_map
  
  save_path <- paste(results_path, group, '/map_crop_diversity_potential_change',temp, '_', group, '.pdf', sep="")
  tmap_save(diversity_map, save_path, dpi = 600)
  
  
}

# vertical bar plots of net change in cropland area within SCS by crop
plot_total_area_change <- function(table_area, group, results_path, aggreg = '') {
  
  
  # fix order of crops based on ascending order in the 1.5 C scenario,
  # filter only total change
  if (! group %in% c('FOOD', 'NONFOOD')) {
    table_area <- table_area %>%
      filter(change_dir == 'total') %>%
      arrange(`4`) %>%
      mutate(CROP = paste0(c(letters[1:nrow(.)]), '_', CROP))
  } else {
    prefixes <- c(letters, 'zw', 'zx', 'zy', 'zz')
    
    table_area <- table_area %>%
      filter(change_dir == 'total') %>%
      arrange(desc(fao_group), `4`) %>%
      mutate(CROP = paste0(prefixes[1:nrow(.)], '_', CROP))
  }
  
  # plots including total change from all scenarios
  
  # transform area change to long format
  group_area_change_table <- table_area %>%
    filter(change_dir == 'total') %>%
    dplyr::select(CROP, `1.5`, `2`, `3`, `4`, change_dir, fao_group) %>%
    pivot_longer(cols=-c(CROP, change_dir, fao_group),
                 names_to = "warming",
                 values_to = "perc_change_area") %>%
    arrange(fao_group,perc_change_area)
  
  plt_area_change_all_scens <- ggplot(data = group_area_change_table, 
                                      aes(x = CROP, y = perc_change_area,
                                          group = warming)) + 
    geom_col(aes(fill = warming), position = position_identity(), width = 0.9) +
    scale_y_continuous(limits = c(-80, 30), expand = c(0,0))+
    scale_x_discrete(expand = c(0,0)) +
    scale_fill_manual(values = scico(n = 4, palette = 'lajolla', begin = 0.15, end = 0.85, direction = 1)) +
    ylab(paste("Net change in cropland area within SCS(%)")) +
    xlab("Crop / Crop group") +
    theme(plot.background = element_blank(), 
          panel.grid.minor.x = element_line(colour = 'grey80'),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_line(colour = 'grey80'),
          panel.grid.major.y = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    coord_flip()
  
  plot(plt_area_change_all_scens)
  
  all_scens_file_pdf <- paste(results_path, group, '/total_area_change_scens', aggreg, '.pdf', sep="")
  ggsave(all_scens_file_pdf, plt_area_change_all_scens, height = 20 , width = 15, units = "cm")
  
  
}


# crop diversity potential maps, total diversity
plot_total_diversity <- function(r_input, group, ncrops, temp, results_path, coastline, pal = 'bamako', tocrs = "+proj=robin +over", r_land) {
  
  # crop raster to exclude Antarctica from map
  r_input <- terra::crop(r_input, ext(-180, 180, -56, 90))
  r_land <- terra::crop(r_land, ext(-180, 180, -56, 90))
  
  # change -9999 (non-cropland) and sea in land mask to NA
  r_input[r_input == -9999] <- NA
  r_land[r_land == 0] <- NA
  
  # project to another crs
  if (!is.na(tocrs)){
    r_input <- terra::project(r_input, tocrs, mask = TRUE)
    r_land <- terra::project(r_land, tocrs, mask = TRUE)
  }
  
  # create breaks and palette
  if (! group %in% c('FOOD', 'NONFOOD')) {
    breakvals <-  c(seq(-1, ncrops, 1))
    breaklab <- c(as.character(seq(0, ncrops, 1)))
  } else {
    breakvals <-  c(-1, seq(0, ncrops, 3))
    breaklab <- c(as.character(seq(0, ncrops, 3)))
  }
  
  colorpal <- scico(n = length(breaklab), palette = pal, direction = -1)
  
  # create tmap object
  diversity_map <- tm_shape(r_land) +
    tm_raster(palette = "white",
              showNA = F,
              colorNA = NULL,
              title = 'Other land areas',
              labels = c('Non-cropland')) +
    tm_shape(r_input) +
    tm_raster(palette = colorpal,
              breaks = breakvals,
              labels = breaklab,
              showNA = F,
              colorNA = NULL,
              interval.closure = 'right',
              legend.reverse = TRUE,
              title = paste0("Total potential diversity,\n", temp, "C")) + 
    tm_shape(coastline, projection = "ESRI:54030") + 
    tm_lines(col = "black", lwd = 0.25) +
    tm_layout(legend.position = c('left', 'bottom'))
  
  (diversity_map)
  
  save_path <- paste(results_path, group, '/map_crop_total_diversity_potential',temp, '_', group, '.pdf', sep="")
  tmap_save(diversity_map, save_path, dpi = 600)
  
  
}

# first scenario to push cell outside SCS maps (Figure 1)
plot_first_scen_out <- function(map, group, results_path, coastline, limit, tocrs = NA, pal = 'lajolla', r_land) {
  
  
  # crop raster to exclude Antarctica from map
  map <- terra::crop(map, ext(-180, 180, -56, 90))
  r_land <- terra::crop(r_land, ext(-180, 180, -56, 90))
  
  # change -9999 (non-cropland) and sea in land mask to NA
  map[map == -9999] <- NA
  r_land[r_land == 0] <- NA
  
  
  # project to another crs
  if (!is.na(tocrs)){
    map <- terra::project(map, tocrs, mask = TRUE)
    r_land <- terra::project(r_land, tocrs, mask = TRUE)
  }
  
  # create breaks and palette
  breakvals <-  c(-1, 0, 1.5, 2, 3, 4)
  breaklab <- c("Within SCS at all warming levels", "1.5 C","2 C", "3 C", "4 C")
  
  cols <- scico(n = 4, palette = pal, direction = 1, begin = 0.15, end = 0.85)
  colorpal <- c('lightblue3', cols)
  
  legend_title <- paste0("Lowest global warming level to push ", limit * 100, 
                        "% of\ncurrent crop production outside the SCS")
  
  # create tmap object
  scen_out_map <- tm_shape(r_land) +
    tm_raster(palette = "white",
              showNA = F,
              colorNA = NULL,
              title = 'Other land areas',
              labels = c('Non-cropland')) +
    tm_shape(map) +
    tm_raster(palette = colorpal,
              breaks = breakvals,
              labels = breaklab,
              interval.closure = "right",
              title = legend_title,
              showNA = F,
              colorNA = NULL) + 
    tm_shape(coastline, projection = "ESRI:54030") + 
    tm_lines(col = "black", lwd = 0.25) + 
    tm_layout(legend.position = c(0, 0.25),
              legend.title.size = 1,
              bg.color = NA, frame = FALSE)
  
  scen_out_map
  
  save_path <- paste(results_path, group, '/first_scen_out_', group, '_', limit, '.pdf', sep="")
  tmap_save(scen_out_map, save_path, dpi = 600)
  
}

# stacked bar plot of regional % of area in diversity change categories
plot_regional_area_change_cats <- function(reg_area_change_long, group, results_path, region, pal = 'vik') {
  
  cols <- scico(n = 10, palette = pal)
  colorpal <- c("grey60", "orangered", cols)
  
  reg_area_change_long <- reg_area_change_long %>%
    mutate(scenario = as.character(scenario))
  
  plt_reg_change_cats <- ggplot(data = reg_area_change_long,
                                  aes(x = scenario, y = perc_area_in_cat,
                                      group = change_cat)) + 
    geom_col(data = reg_area_change_long, aes(x = scenario, y = perc_area_in_cat,
                                             fill= change_cat), position = 'stack') +
    scale_x_discrete(expand = c(0,0), 
                     labels = c("1.5 C","2 C", "3 C", "4 C")) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,101)) + 
    scale_fill_manual(values = colorpal,
                      name = "Change in potential crop diversity (%)") +
    ylab("Share of cropland area in diversity change category (%)") +
    xlab("Global warming") +
    ggtitle(paste(group, region, paste=' ')) +
    theme(plot.background = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = 'grey80'),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  plot(plt_reg_change_cats)

  
  # save bar plots
  area_change_cats_pdf <- paste(results_path, group, '/area_change_cats_', group, '_', region, '.pdf', sep="")
  area_change_cats_png <- paste(results_path, group, '/area_change_cats_', group, '_', region, '.png', sep="")
  ggsave(area_change_cats_pdf, plt_reg_change_cats, height = 16 , width = 18, units = "cm")
  ggsave(area_change_cats_png, plt_reg_change_cats, height = 16 , width = 18, units = "cm")
  
}

# stacked bar plot of regional % of area in first scenario out scenarios
plot_regional_first_scen_out <- function(reg_first_scen_long, group, results_path, region, pal = 'lajolla') {
  
  cols <- scico(n = 4, palette = pal, direction = -1, begin = 0.15, end = 0.85)
  colorpal <- c('lightblue3', cols)
  
  plt_reg_first_scen <- ggplot(data = reg_first_scen_long,
                                aes(x = region, y = perc_area_in_cat,
                                    group = first_scen_out)) + 
    geom_col(aes(x = region, y = perc_area_in_cat,
              fill= first_scen_out), position = 'stack') +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,101)) + 
    scale_fill_manual(values = colorpal,
                      guide = guide_legend(reverse = TRUE),
                      name = "Lowest warming level to push\nat least 25%\nof current production\noutside SCS") +
    ylab("Share of cropland area (%)") +
    xlab("Region") +
    coord_flip() +
    theme(plot.background = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = 'grey80'),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  plot(plt_reg_first_scen)
  
  
  # save bar plots
  save_pdf <- paste(results_path, group, '/first_scen_out_reg_', group, '.pdf', sep="")
  save_png <- paste(results_path, group, '/first_scen_out_reg_', group, '.png', sep="")
  ggsave(save_pdf, plt_reg_first_scen, height = 16 , width = 22, units = "cm")
  ggsave(save_png, plt_reg_first_scen, height = 16 , width = 22, units = "cm")
  
}

# holdridge diagrams
plot_ternary <- function(data_folder, spam_year, crop_name) {
  
  clim_scens <- c('_current', 'ssp26_15wp', 'ssp45_2wp', 'ssp70_3wp', 'ssp85_4wp')
  
  for (clim_scen in clim_scens) {
    
    # plot types for scenario
    if (clim_scen == '_current') {
      # plotting
      plot_types <- list(c('SCS', 'prod_present'),
                         c('production','prod_present', 'prod_scen'),
                         c('area', 'prod_present', 'area_scen'))
    } else {
      plot_types <- list(c('production','prod_present', 'prod_scen'),
                         c('area', 'prod_present', 'area_scen'))
    }
  
    # data for various PET and prec values
    df = read.csv(paste0(data_folder, 'ternary_mapping/', crop_name, '/holdridge_PET_precip_tabulted_crop_spam_',crop_name, spam_year, '_', clim_scen, '.csv'))
    names(df) <- c('PET','prec','prod_present','prod_scen', 'area_present', 'area_scen')
    
    # log2 and normalise between 0 and 1
    # PET scaled between 0.125 and 32
    # precip scaled between 62.5 and 16000 mm/yr
    
    df['PET_trans'] <- (log2(df['PET']) - log2(0.125) ) / (log2(32)-log2(0.125))
    
    df['prec_trans'] <- (log2(df['prec']) - log2(62.5) ) / (log2(16000)-log2(62.5))
    
    df[df['PET_trans'] < 0,'PET_trans'] = 0
    df[df['PET_trans'] > 1,'PET_trans'] = 1
    
    df[df['prec_trans'] < 0,'prec_trans'] = 0
    df[df['prec_trans'] > 1,'prec_trans'] = 1
    
    # temp with the function of PET and prec
    df['temp_trans'] <- 1 - df['PET_trans'] - df['prec_trans']
    
    ## PLOTTING
    
    # Define tick locations and tick labels to be potentially used in the plots
    breaks_9 <- seq(0, 1, length=9)
    PET_labels <- c("0.125", "0.25","0.5","1","2","4","8","16","32")
    prec_labels <- (c("62.5", "125","250","500","1000","2000","4000","8000","16000"))
    
    for (type in plot_types) {
      
      list_to_include <- type[-c(1)]
      
      df_plot <- df %>%
        dplyr::select(all_of(c('PET_trans','prec_trans', 'temp_trans', list_to_include)))
     
      # Filter table from unnecessary data and zero values
      df_long <- df_plot %>%
        pivot_longer(all_of(list_to_include),
                     names_to = 'scenario',
                     values_to = 'ref_val') %>%
        filter(ref_val != 0)
      
      # Initialize ternary plot
      tern_plot <-  ggtern(data=df_long, aes(x=PET_trans,y=temp_trans,z=prec_trans)) +
        theme_nogrid_minor() +
        scale_T_continuous(breaks=breaks_9,labels=rev(PET_labels)) +
        scale_L_continuous(breaks=breaks_9,labels=PET_labels) +
        scale_R_continuous(breaks=breaks_9,labels=prec_labels) +
        # labs( x       = "",
        #       xarrow  = "PET",
        #       y       = "",
        #       yarrow  = "Temperature",
        #       z       = "",
        #       zarrow  = "Precipitation") +
        # theme_showarrows() +
        theme_notitles() +
        #theme_nolabels() +
        theme(legend.position="none") +
        theme_hideprimary() #+
      #theme_noticks()
      
      # Create a density plot as a reference for the present day (year 2020) scenario
      # Only the outlines of this plot are used for the actual results
      df_long_dens <- df_long %>% 
        filter(scenario == list_to_include[1]) %>%
        # scale ref val to 0.5-1 for visualization
        mutate(norm_ref_val = (ref_val - min(ref_val))/(max(ref_val) - min(ref_val))* (1-0.5) + 0.5)
      
      tern_plot <- tern_plot + stat_density_tern(data = df_long_dens, aes(fill = norm_ref_val),
                                                 fill = 'cornflowerblue',
                                                 alpha = 0.5,
                                                 geom = 'polygon',
                                                 h = c(0.025, 0.025),
                                                 base = 'identity',
                                                 bins = 3,
                                                 n = 500,
                                                 na.rm = TRUE)
      
      if ('prod_scen' %in% list_to_include) {
        
        # Create a point plot (scatter) on the ternary using the scenario that is actually investigated
        # The color saturation describes the amount of food crop production in that Holdridge category
        df_long_point = df_long %>% 
          filter(scenario == 'prod_scen')%>%
          # # scale reference data to 0.5 to 1 to use it as the alpha parameter
          mutate(norm_ref_val = (ref_val - min(ref_val))/(max(ref_val) - min(ref_val))* (1-0.5) + 0.5)
          
        tern_plot <- tern_plot + geom_point(data = df_long_point, aes(color = scenario, alpha = norm_ref_val),
                                            size = 2, stroke = NA, color = 'firebrick4',
                                            na.rm = TRUE)
        
      } else if ('area_scen' %in% list_to_include)  {
        # Create a density plot of the cropland area on the ternary using the scenario that is actually investigated
        # Only the outlines of this plot are used for the actual results
    
        df_long_area = df_long %>% 
          filter(scenario == 'area_scen')%>%
          # normalize reference data to 0-1
          # scale reference data to 0.5 to 1 to use it as the alpha parameter
          mutate(norm_ref_val = (ref_val - min(ref_val))/(max(ref_val) - min(ref_val))* (1-0.5) + 0.5)
        
        tern_plot <- tern_plot + geom_point(data = df_long_area, aes(fill = scenario, alpha = norm_ref_val),
                                            size = 2,color = 'aquamarine4', stroke = NA,
                                            na.rm = TRUE)
        
      } else {
        # do nothing
      }
    
      
      filename = paste0(data_folder, crop_name,'/',crop_name, '_',clim_scen,'_', type[1], '.png', sep='')
      ggsave(filename, width = 10, height = 10)
      filename = paste0(data_folder, crop_name,'/',crop_name, '_',clim_scen,'_', type[1], '.pdf', sep='')
      ggsave(filename, width = 10, height = 10)
      
      print(tern_plot) 
      
    
    }
  }
 
}

# Crop specific SCS map
plot_SCS_map <- function(results_path, crop, spam_year, coastline, r_land, tocrs) {
  
  # read SCS map data
  result_path <- paste(results_path, 'matlab/', crop, '/main_results_div', crop, spam_year, '.mat', sep="")
  data <- read_mat(result_path, ram=TRUE)
  
  # initialize crop specific SCS rasters
  r_SCS_tot_baseline <- rast(res=c(1/12, 1/12), names = c(crop))
  r_SCS_tot_15wp <- rast(res=c(1/12, 1/12), names = c(crop))
  r_SCS_tot_2wp <- rast(res=c(1/12, 1/12), names = c(crop))
  r_SCS_tot_3wp <- rast(res=c(1/12, 1/12), names = c(crop))
  r_SCS_tot_4wp <- rast(res=c(1/12, 1/12), names = c(crop))
  
  # fill SCS rasters with crop specific data
  values(r_SCS_tot_baseline) <- data$SCS_out_total_baseline
  values(r_SCS_tot_15wp) <- data$SCS_out_total_15wp
  values(r_SCS_tot_2wp) <- data$SCS_out_total_2wp
  values(r_SCS_tot_3wp) <- data$SCS_out_total_3wp
  values(r_SCS_tot_4wp) <- data$SCS_out_total_4wp
  
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
  
  # stack rasters
  r_SCS <- c(r_SCS_tot_baseline, r_SCS_tot_15wp, r_SCS_tot_2wp, r_SCS_tot_3wp,
             r_SCS_tot_4wp)
  
  # crop raster to exclude Antarctica from map
  map <- terra::crop(r_SCS, ext(-180, 180, -56, 90), mask = TRUE, overwrite = TRUE)
  r_land <- terra::crop(r_land, ext(-180, 180, -56, 90), mask = TRUE, overwrite = TRUE)
  
  # change -9999 (non-cropland) ans sea to NA
  map[map == -9999] <- NA
  r_land[r_land == 0] <- NA

  # project to another crs
  if (!is.na(tocrs)){
    map <- terra::project(map, tocrs, mask = TRUE)
    r_land <- terra::project(r_land, tocrs, mask = TRUE)
  }
  
  # create breaks and palette
  breakvals <-  c(-Inf, 0, 1)
  breaklab <- c("Within", "Outside")
  
  colorpal <- c("#AA4499", "#DDCC77")

  temps <- c('baseline_climate', '1.5C_warming', '2C_warming', '3C_warming', '4C_warming')
  
  names(map) <- temps
  
  for (temp in temps) {
    
    SCS_layer <- subset(map, temp)
  
    # create tmap object
    SCS_map <- tm_shape(r_land) +
      tm_raster(palette = "white",
                showNA = F,
                colorNA = NULL,
                title = 'Other land areas',
                labels = c('Non-cropland')) +
      tm_shape(SCS_layer) +
      tm_raster(palette = colorpal,
                breaks = breakvals,
                labels = breaklab,
                interval.closure = "right",
                title = paste0('Crop specific SCS status\nat ', temp),
                showNA = F,
                colorNA = NULL) + 
      tm_shape(coastline, projection = "ESRI:54030") + 
      tm_lines(col = "black", lwd = 0.25) + 
      tm_layout(legend.position = c(0, 0.25),
                legend.title.size = 1,
                bg.color = NA, frame = FALSE)
    
    SCS_map
    
    save_path <- paste(results_path, crop, '/SCS_map_', crop, '_', temp, '.pdf', sep="")
    tmap_save(SCS_map, save_path, dpi = 600)
    
  }
    
  
}

# background map for regional figures on top of map
plot_background <- function(coastline, r_region, results_path) {
  
  # crop land mask raster to exclude Antarctica from map
  r_region_crop <- crop(r_region, ext(-180, 180, -56, 90))
  
  # project to robinson crs
  r_region_proj <- terra::project(r_region_crop, "+proj=robin +over", mask = TRUE)

  
  breakvals <- c(seq(0,8,1))
  cols <- c('grey95', 'grey75','grey90', 'grey65', 'grey85', 'grey60','grey80', 'grey75')
  
  plt_bg_map <- tm_shape(r_region_proj) +
                tm_raster(palette = cols,
                          breaks = breakvals,
                          legend.show = FALSE) #+
                #tm_shape(coastline, projection = "ESRI:54030") + 
                #tm_lines(col = "grey60", lwd = 0.25)
  
  
  
  save_path <- paste(results_path, 'background_map.pdf', sep="")
  tmap_save(plt_bg_map, save_path)            
  
}

# supplementary table of first scen out map statistics
table_first_scen_out <- function(in_table, results_path, group) {
  
  # changing number formatting
  in_table <- in_table %>%
    filter(region != 'Antarctica') %>%
    mutate_if(is.numeric, function(x) x*100) %>%
    mutate_if(is.numeric, round, 0) %>%
    mutate_if(is.numeric, as.character)
  
  # isolate 25 % and 75% results
  low_table <- in_table %>%
    filter(out_limit == '50') %>%
    dplyr::select(-out_limit)
  colnames(low_table) <- c('region', paste(c(0,1.5,2,3,4),'low', sep="_"))
  
  high_table <- in_table %>%
    filter(out_limit == '75') %>%
    dplyr::select(-out_limit)
  colnames(high_table) <- c('region', paste(c(0,1.5,2,3,4),'high', sep="_"))
  
  # select 50 % out limit values as the base of table & join with others
  out_table <- in_table %>%
    filter(out_limit == '25') %>%
    dplyr::select(-out_limit) %>%
    left_join(low_table, by = 'region') %>%
    left_join(high_table, by = 'region')
  
  # add numbers from 50% and 75% results in brackets after 25 % result
  out_table <- out_table %>%
    mutate(all_1.5 = paste0(cum_1.5, ' (', `1.5_low`, ' - ', `1.5_high`, ')'),
           all_2 = paste0(cum_2, ' (', `2_low`, ' - ', `2_high`, ')'),
           all_3 = paste0(cum_3, ' (', `3_low`, ' - ', `3_high`, ')'),
           all_4 = paste0(cum_4, ' (', `4_low`, ' - ', `4_high`, ')'),
           all_in = paste0(all_in, ' (', `0_low`, ' - ', `0_high`, ')')) %>%
    dplyr::select(region, starts_with('all_'))
  
  save_path <- paste(results_path, 'tables/', group, '/first_scen_out_table_pretty.csv', sep= '')
  write.csv(out_table, save_path, row.names = FALSE)
  
  
}

# supplementary tables combining diversity change for crop groups
table_div_change_groups <- function(results_path, group_codes) {
  
  food_list <- sort(unique(group_codes[group_codes$food == 1,]$fao_group), decreasing = TRUE)
  
  # create tables for collecting data from all crop groups
  df_all_div <- data.frame(matrix(ncol = 14, nrow = 0))
  df_all_emerg <- data.frame(matrix(ncol = 10, nrow = 0))
  
  columns_div <- c("crop_group", "scenario",	"-100", "-99.99 to -75",
                        "-75 to -50",	"-50 to -25",	"-25 to 0",	"no change",
                        "0 to +25",	"+25 to +50",	"+50 to +75",	"+75 to +100",
                        "emerging clim potential",	"marginal baseline outside SCS scen")
  colnames(df_all_div) <- columns_div

    # read tables for all crop groups
    for (group in food_list) {
      
      # read data
      df_group <- read.csv(paste0(results_path, 'tables/', group, '/div_change_regional_area_cat_',
                                  group,'.csv')) 
      
      # filter data for global diversity change
      df_group_div <- df_group %>%
        filter(region == 'Global') %>%
        dplyr::select(-c(region, tot_area_region, perc_area_region)) %>%
        mutate(crop_group = group) %>%
        relocate(crop_group, .before = scenario)
      
      colnames(df_group_div) <- columns_div
      
      # add the data to the larger dataframe
      df_all_div <- rbind(df_all_div, df_group_div)
      
      # filter data for emerging potential
      df_group_emerg <- df_group %>%
        dplyr::select(c(scenario, region, emerging.clim.potential)) %>%
        filter(region != "Antarctica") %>%
        mutate(crop_group = group) %>%
        relocate(crop_group, .before = scenario)
      
      # add the data to the larger dataframe
      df_all_emerg <- rbind(df_all_emerg, df_group_emerg)
      
    }
  
  # sort based on scenario and crop group
  df_all_div_sorted <- df_all_div %>%
    arrange(desc(scenario), desc(crop_group))
  
  # transform emerging table to wide format
  df_emerg_wide <- df_all_emerg %>%
    pivot_wider(id_cols = c(scenario, crop_group),
                id_expand = FALSE, names_from = region,
                values_from = emerging.clim.potential) %>%
    relocate(Global, .after = crop_group)
  
  # sort based on scenario and crop group
  df_all_emerg_sorted <- df_emerg_wide %>%
    arrange(desc(scenario), desc(crop_group))
  
  # save tables
  save_path_div = paste0(results_path, 'tables/div_change_crop_groups.csv')
  write.csv(df_all_div_sorted, save_path_div, row.names = FALSE)
  
  save_path_emerg = paste0(results_path, 'tables/emerging_area_crop_groups.csv')
  write.csv(df_all_emerg_sorted, save_path_emerg, row.names = FALSE)
  
}



#### FUNCTIONS SECTION ENDS; ANALYSIS CODE BEGINS ####

# set working directory to the folder of this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
working_dir <- getwd()

results_path <- paste0(working_dir, '/results_240624_2020_comb/')

# Crop data year? ('2005', '2010', or '2020')
spam_year <- '2020'

# Use 2005/2010 crop types for the year 2020? (Supplementary analysis)
comb_2020 = TRUE

## do not modify from here on ##

# crop group coding

if (spam_year == '2020' & !comb_2020) {
  group_codes <- read.xlsx('ref_data/fao_food_groups2020.xlsx')
  
} else if (spam_year == '2020' & comb_2020) {
  group_codes <- read.xlsx('ref_data/fao_food_groups2020_comb.xlsx')
  
} else{
  group_codes <- read.xlsx('ref_data/fao_food_groups.xlsx')
}

group_list <- c(sort(unique(group_codes$fao_group)[1:5]), unique(group_codes$fao_group)[6:9])
food_list <- c(sort(unique(group_codes[group_codes$food == 1,]$fao_group)), 'TOTAL') 

barplot_pal <- c('#D59977','#3C6691', '#DDCC77', '#44AA99', '#88CCEE', '#CC6677', '#882255',
                 '#44AA99', '#88CCEE', '#CC6677', '#882255', 'grey70')


names(barplot_pal) <- c('CEREALS1', 'FRUIT AND VEG1', group_list, 'TOTAL')

# load land mask, region raster, and coastline
land_data <- read_mat('holdridge_data/hLand.mat')
r_land <- rast(res=c(1/12, 1/12))
values(r_land) <- land_data$hLand

r_region <- rast('ref_data/region_id.tif')
r_region[r_land == 0] <- NA


#coastline <- read_sf('ref_data/ne_50m_coastline.shp')

coastline <- ne_download(scale = 50, type = "coastline",
                         category = "physical", returnclass = "sf")

# make background map for fig 2 and fig 6
plot_background(coastline, r_region, results_path)

# make holdridge diagrams for wheat (Extended data figure 1)
plot_ternary(results_path, spam_year, 'WHEA')

# make SCS maps for wheat
plot_SCS_map(results_path, 'WHEA', spam_year, coastline, r_land, tocrs = "+proj=robin +over")

# make supplementary tables collecting results from all food crop groups
table_div_change_groups(results_path, group_codes)

# groupings for results: individual crop groups and/or individual crops
if (spam_year == '2020' & !comb_2020){
  groupings <- c('group', 'food', 'only_soyb', 'only_maize') #, 'nonfood')
  
} else {
  groupings <- c('food', 'group')
}

# perc thresholds in first scen out analysis:
out_limit <- c(0.25, 0.5, 0.75)

# figures and tables for main analyses, loop through crop groups
for (grouping in groupings){
  
  if (grouping == 'group') {
    groups <- group_list
  } else if (grouping == 'food'){
    groups <- c('FOOD')
  } else if (grouping == 'nonfood'){
    groups <- c('NONFOOD') 
  }
  
  for (group in groups) {
    
    ## first scenario to push a cell outside SCS
    
    if (group %in% c('FOOD', 'NONFOOD')) {

      map_pattern <- paste('first_scen_out_SCS_', group, '_', sep = '')
      map_list <- list.files(path = paste(results_path, group, '/', sep = ''),
                             pattern = map_pattern, all.files = TRUE, full.names = TRUE)
      r_scen_out_map <- rast(map_list)

      for (lyr in 1:nlyr(r_scen_out_map)) {

        map <- r_scen_out_map[[lyr]]
        plot_first_scen_out(map, group, results_path, coastline, out_limit[lyr], tocrs = "+proj=robin +over", r_land = r_land)

      }

      first_scen_out_table_path <- paste(results_path, 'tables/', group,
                                         '/first_scen_out_reg_stats_', group,
                                         '.csv', sep = "")
      first_scen_out_table <- read.csv(first_scen_out_table_path) %>%
        drop_na
      
      first_scen_out_table_cum <- first_scen_out_table %>%
        mutate(all_in = X0,
               cum_1.5 = X1.5,
               cum_2 = X1.5 + X2,
               cum_3 = X1.5 + X2 + X3,
               cum_4 = X1.5 + X2 + X3 + X4) %>%
        dplyr::select(-starts_with("X"))

      # neater version of regional table
      table_first_scen_out(first_scen_out_table_cum, results_path, group)

      # regional bar plots
      first_scen_out_25 <- first_scen_out_table %>%
        filter(out_limit == 0.25) %>%
        rename_with(~ gsub("X", "", .x, fixed = TRUE)) %>%
        dplyr::select(-out_limit)
      
      colnames(first_scen_out_25) <- c('region', 'a_0', 'e_1.5', 'd_2', 'c_3', 'b_4')

      # transform into long format for plotting
      first_scen_out_25_long <- first_scen_out_25 %>%
        arrange(a_0) %>%
        mutate(region = ifelse(region == 'Global','a_Global', paste0(letters[2:9], '_',region))) %>%
        pivot_longer(cols = - region,
                     names_to = 'first_scen_out',
                     values_to = 'perc_area_in_cat') %>%
        mutate(perc_area_in_cat = perc_area_in_cat * 100)

      plot_regional_first_scen_out(first_scen_out_25_long, group, results_path)

      }


    ## % out SCS bar plots and lost vs gained potential area bar plots##

    table_path <- paste(results_path, 'tables/', group, '/SCS_out_perc_', group, '.csv', sep="")
    table_regional_path <- paste(results_path, 'tables/', group, '/SCS_out_perc_regional', group, '.csv', sep="")

    table_area_path <- paste(results_path, 'tables/', group, '/SCS_area_change_perc_', group, '.csv', sep="")

    # percentage of current production table in long format,
    # crop specific within crop groups.

    SCS_table <- read.csv(table_path)
    colnames(SCS_table) <- sub("X", "", colnames(SCS_table))

    SCS_table_regional <- read.csv(table_regional_path)
    colnames(SCS_table_regional) <- sub("X", "", colnames(SCS_table_regional))
    names(SCS_table_regional)[2] <- "subregion"

    regions <- unique(SCS_table_regional$subregion)

    area_table <- read.csv(table_area_path) %>%
      left_join(group_codes, by = c('CROP' = 'spamcrop'))

    colnames(area_table) <- sub("X", "", colnames(area_table))

    ncrops <- length(unique(SCS_table$CROP))


    # perc outside SCS bar plots
    # only for each sub group, not for all food crops and all nonfood crops
    if (! group %in% c('FOOD', 'NONFOOD')){

      ## GLOBAL AGGREGATE PLOTS ##

      # % production outside SCS

      # transform perc out to long format
      SCS_table_out <- SCS_table %>%
        dplyr::select(!c(contains("_"),14)) %>%
        pivot_longer(cols=(-CROP),
                     names_to = "warming",
                     values_to = "perc_out_SCS") %>%
        mutate(warming = as.numeric(warming),
               perc_out_SCS = perc_out_SCS * 100)

      # transform 25th percentile out to long format
      SCS_table_low <- SCS_table %>%
        dplyr::select(c(CROP, contains("_low"))) %>%
        pivot_longer(cols=(-CROP),
                     names_to = c("warming", "bound"),
                     names_sep = ("_"),
                     values_to = "limit_low") %>%
        mutate(warming = as.numeric(warming),
               limit_low = limit_low * 100) %>%
        dplyr::select(-bound)

      # transform 75th percentile out to long format
      SCS_table_high <- SCS_table %>%
        dplyr::select(c(CROP, contains("_high"))) %>%
        pivot_longer(cols=(-CROP),
                     names_to = c("warming", "bound"),
                     names_sep = ("_"),
                     values_to = "limit_high") %>%
        mutate(warming = as.numeric(warming),
               limit_high = limit_high * 100) %>%
        dplyr::select(-bound)

      # merge long format dataframes before plotting
      SCS_table_long <- SCS_table_out %>%
        merge(SCS_table_low, by=c("CROP","warming")) %>%
        merge(SCS_table_high, by=c("CROP","warming")) %>%
        mutate(CROP = fct_reorder(CROP, perc_out_SCS, .desc=FALSE))

      plot_bar(SCS_table_long, group, results_path, barplot_pal, '', food_list, 'global')

      # save plot table as csv
      write.csv(SCS_table_long, paste0(results_path, 'tables/', group, '/perc_out_SCS_barplot', group, '.csv', sep=""))

      # lost & gained cropland area

      # experimental plots
      plot_total_area_change(area_table, group, results_path, aggreg = '')

      # transform area change to long format
      area_change_table <- area_table %>%
        dplyr::select(CROP, `1.5`, `2`, `3`, `4`, change_dir, fao_group) %>%
        pivot_longer(cols=-c(CROP, change_dir, fao_group),
                     names_to = "warming",
                     values_to = "perc_change_area") %>%
        mutate(warming = as.numeric(warming))

      # transform 25th percentile out to long format
      area_change_table_low <- area_table %>%
        dplyr::select(c(CROP, change_dir, fao_group, contains("_low"))) %>%
        pivot_longer(cols=-c(CROP, change_dir, fao_group),
                     names_to = c("warming", "bound"),
                     names_sep = ("_"),
                     values_to = "limit_low") %>%
        mutate(warming = as.numeric(warming)) %>%
        dplyr::select(-bound)

      # transform 75th percentile out to long forma
      area_change_table_high <- area_table %>%
        dplyr::select(c(CROP, change_dir, fao_group, contains("_high"))) %>%
        pivot_longer(cols=-c(CROP, change_dir, fao_group),
                     names_to = c("warming", "bound"),
                     names_sep = ("_"),
                     values_to = "limit_high") %>%
        mutate(warming = as.numeric(warming)) %>%
        dplyr::select(-bound)

      # merge long format dataframes before plotting
      area_table_long <- area_change_table %>%
        merge(area_change_table_low, by=c("CROP","warming", "change_dir", "fao_group")) %>%
        merge(area_change_table_high, by=c("CROP","warming", "change_dir", "fao_group"))
      
      # save table as csv to help plot interpretation
      write.csv(area_table_long, paste0(results_path, 'tables/', group, '/area_change_barplot_table', group, '.csv', sep=""))

      plot_area_change(area_table_long, group, results_path, barplot_pal, '', food_list, 'global')

      ## REGIONAL AGGREGATE PLOTS

      for (region in regions){

        # select region specific data from regional data frame
        SCS_table_region_spec <- SCS_table_regional %>%
          filter(subregion == region) %>%
          dplyr::select(-c(subregion, region_code))

        # transform perc out to long format
        SCS_table_out <- SCS_table_region_spec %>%
          dplyr::select(!c(contains("_"),14)) %>%
          pivot_longer(cols=(-CROP),
                       names_to = "warming",
                       values_to = "perc_out_SCS") %>%
          mutate(warming = as.numeric(warming),
                 perc_out_SCS = perc_out_SCS * 100)

        # transform 5th percentile out to long format
        SCS_table_low <- SCS_table_region_spec %>%
          dplyr::select(c(CROP, contains("_low"))) %>%
          pivot_longer(cols=(-CROP),
                       names_to = c("warming", "bound"),
                       names_sep = ("_"),
                       values_to = "limit_low") %>%
          mutate(warming = as.numeric(warming),
                 limit_low = limit_low * 100) %>%
          dplyr::select(-bound)

        # transform 95th percentile out to long format
        SCS_table_high <- SCS_table_region_spec %>%
          dplyr::select(c(CROP, contains("_high"))) %>%
          pivot_longer(cols=(-CROP),
                       names_to = c("warming", "bound"),
                       names_sep = ("_"),
                       values_to = "limit_high") %>%
          mutate(warming = as.numeric(warming),
                 limit_high = limit_high * 100) %>%
          dplyr::select(-bound)

        # merge long format dataframes before plotting
        SCS_table_long <- SCS_table_out %>%
          merge(SCS_table_low, by=c("CROP","warming")) %>%
          merge(SCS_table_high, by=c("CROP","warming")) %>%
          mutate(CROP = fct_reorder(CROP, perc_out_SCS, .desc=TRUE))

        plot_bar(SCS_table_long, group, results_path, barplot_pal, '', food_list, region)
        
        # save plot table as csv
        write.csv(SCS_table_long, paste0(results_path, 'tables/', group, '/perc_out_SCS_barplot', group, '_', region, '.csv', sep=""))

      }


    }

    # bar plots only for aggregates of crop groups, i.e. food crops and non food crops
    if (group == 'NONFOOD' || group == 'FOOD'){
      # percentage of current production table in long format,
      # aggregated from actual crop specific prods within crop group

      ## GLOBAL AGGREGATE PLOTS ##

      # add column for total prod in reference conditions
      SCS_table_prod <- SCS_table %>%
        mutate_at(vars(`1.5`:`4_high`), ~ .x * ref_tot) %>%
        left_join(group_codes, by= c("CROP" = "spamcrop")) %>%
        dplyr::select(-16)

      # summarize total prod within each crop group
      SCS_table_prod_sums <- SCS_table_prod %>%
        group_by(fao_group) %>%
        summarise_at(vars(`1.5`:ref_tot), sum) %>%
        ungroup()

      # transform into percentage
      group_perc_table <- SCS_table_prod_sums %>%
        mutate_at(vars(`1.5`:`4_high`), ~ .x / ref_tot) %>%
        rename(CROP = fao_group) %>%
        dplyr::select(-14)

      # transform perc out to long format
      group_perc_out_table_long <- group_perc_table %>%
        dplyr::select(!contains("_")) %>%
        pivot_longer(cols=-CROP,
                     names_to = "warming",
                     values_to = "perc_out_SCS") %>%
        mutate(warming = as.numeric(warming),
               perc_out_SCS = perc_out_SCS * 100)

      # transform 5th percentile out to long format
      group_perc_low_table_long <- group_perc_table %>%
        dplyr::select(c(CROP, contains("_low"))) %>%
        pivot_longer(cols=(-CROP),
                     names_to = c("warming", "bound"),
                     names_sep = ("_"),
                     values_to = "limit_low") %>%
        mutate(warming = as.numeric(warming),
               limit_low = limit_low * 100) %>%
        dplyr::select(-bound)

      # transform 95th percentile out to long format
      group_perc_high_table_long <- group_perc_table %>%
        dplyr::select(c(CROP, contains("_high"))) %>%
        pivot_longer(cols=(-CROP),
                     names_to = c("warming", "bound"),
                     names_sep = ("_"),
                     values_to = "limit_high") %>%
        mutate(warming = as.numeric(warming),
               limit_high = limit_high * 100) %>%
        dplyr::select(-bound)

      # merge long format dataframes before plotting
      group_perc_table_long <- group_perc_out_table_long %>%
        merge(group_perc_low_table_long, by=c("CROP","warming")) %>%
        merge(group_perc_high_table_long, by=c("CROP","warming"))

      plot_bar(group_perc_table_long, group, results_path, barplot_pal, aggreg = '_aggreg', food_list, 'global')

      # save plot table as csv
      write.csv(group_perc_table_long, paste0(results_path, 'tables/', group, '/perc_out_SCS_barplot', group, '.csv', sep=""))

      # lost & gained cropland area

      # experimental plots
      plot_total_area_change(area_table, group, results_path, aggreg = '')

      # transform perc area to total area
      area_table_tot <- area_table %>%
        relocate(change_dir, .after = baseline_area) %>%
        mutate_at(vars(`1.5`:`4_high`), ~ .x / 100 * baseline_area) %>%
        dplyr::select(-17)

      # summarize total prod within each crop group
      area_table_tot_sums <- area_table_tot %>%
        group_by(fao_group, change_dir) %>%
        summarise_at(vars(`1.5`:baseline_area), sum) %>%
        ungroup()

      # transform back into percentage
      group_perc_area_table <- area_table_tot_sums %>%
        mutate_at(vars(`1.5`:`4_high`), ~ .x / baseline_area * 100) %>%
        mutate(CROP = fao_group)

      # experimental plots
      plot_total_area_change(group_perc_area_table, group, results_path, aggreg = '_aggreg')

      # transform area change to long format
      group_area_change_table <- group_perc_area_table %>%
        dplyr::select(CROP, `1.5`, `2`, `3`, `4`, change_dir) %>%
        pivot_longer(cols=-c(CROP, change_dir),
                     names_to = "warming",
                     values_to = "perc_change_area") %>%
        mutate(warming = as.numeric(warming))

      # transform 25th percentile out to long format
      group_area_change_table_low <- group_perc_area_table %>%
        dplyr::select(c(CROP, change_dir, contains("_low"))) %>%
        pivot_longer(cols=-c(CROP, change_dir),
                     names_to = c("warming", "bound"),
                     names_sep = ("_"),
                     values_to = "limit_low") %>%
        mutate(warming = as.numeric(warming)) %>%
        dplyr::select(-bound)

      # transform 75th percentile out to long forma
      group_area_change_table_high <- group_perc_area_table %>%
        dplyr::select(c(CROP, change_dir, contains("_high"))) %>%
        pivot_longer(cols=-c(CROP, change_dir),
                     names_to = c("warming", "bound"),
                     names_sep = ("_"),
                     values_to = "limit_high") %>%
        mutate(warming = as.numeric(warming)) %>%
        dplyr::select(-bound)

      # merge long format dataframes before plotting
      group_area_table_long <- group_area_change_table %>%
        merge(group_area_change_table_low, by=c("CROP","warming", "change_dir")) %>%
        merge(group_area_change_table_high, by=c("CROP","warming", "change_dir"))# %>%
        #mutate(CROP = fct_reorder(CROP, perc_change_area, .desc=FALSE))
      
      # save table as csv to help plot interpretation
      write.csv(group_area_table_long, paste0(results_path, 'tables/', group, '/area_change_barplot_table', group, '.csv', sep=""))

      plot_area_change(group_area_table_long, group, results_path, barplot_pal, aggreg = '_aggreg', food_list, 'global')


      ## REGIONAL AGGREGATE PLOTS ##
      
      # read table showing % total production in region outside the SCS.
      # transform to percentage and select only median results
      table_all_prod_reg_path <- table_regional_path <- paste(results_path, 'tables/', group, '/SCS_out_perc_all_crops_regional', group, '.csv', sep="")
      table_all_prod_reg <- read.csv(table_all_prod_reg_path)%>%
        rename(subregion = REGION_WB) %>%
        mutate_at(vars(`X1.5`:`X4_high`), ~.x * 100) %>%
        dplyr::select(-ref_tot)
      
      colnames(table_all_prod_reg) <- sub("X", "", colnames(table_all_prod_reg))
      

      for (region in regions){

        # select region specific data from regional data frame
        SCS_table_region_spec <- SCS_table_regional %>%
          filter(subregion == region) %>%
          dplyr::select(-c(subregion, region_code))
        
        # select region specific data from all production summary data frame
        table_all_prod_reg_spec <- table_all_prod_reg %>%
          filter(subregion == region) %>%
          mutate(CROP = 'TOTAL', .before = `1.5`) %>%
          dplyr::select(-subregion)

        # add column for total prod in reference conditions
        SCS_table_prod <- SCS_table_region_spec %>%
          mutate_at(vars(`1.5`:`4_high`), ~ .x * ref_tot) %>%
          left_join(group_codes, by= c("CROP" = "spamcrop")) %>%
          dplyr::select(-16)

        # summarize total prod within each crop group
        SCS_table_prod_sums <- SCS_table_prod %>%
          group_by(fao_group) %>%
          summarise_at(vars(`1.5`:ref_tot), sum) %>%
          ungroup()

        # transform into percentage
        group_perc_table <- SCS_table_prod_sums %>%
          mutate_at(vars(`1.5`:`4_high`), ~ .x / ref_tot *100) %>%
          rename(CROP = fao_group) %>%
          dplyr::select(-14)
        
        # join with data from all crops summary data frame
        group_perc_table_all <- group_perc_table %>%
          bind_rows(table_all_prod_reg_spec)

        # transform perc out to long format
        group_perc_out_table_long <- group_perc_table_all %>%
          dplyr::select(!contains("_")) %>%
          pivot_longer(cols=-CROP,
                       names_to = "warming",
                       values_to = "perc_out_SCS") %>%
          mutate(warming = as.numeric(warming))

        # transform 5th percentile out to long format
        group_perc_low_table_long <- group_perc_table_all %>%
          dplyr::select(c(CROP, contains("_low"))) %>%
          pivot_longer(cols=(-CROP),
                       names_to = c("warming", "bound"),
                       names_sep = ("_"),
                       values_to = "limit_low") %>%
          mutate(warming = as.numeric(warming)) %>%
          dplyr::select(-bound)

        # transform 95th percentile out to long format
        group_perc_high_table_long <- group_perc_table_all %>%
          dplyr::select(c(CROP, contains("_high"))) %>%
          pivot_longer(cols=(-CROP),
                       names_to = c("warming", "bound"),
                       names_sep = ("_"),
                       values_to = "limit_high") %>%
          mutate(warming = as.numeric(warming)) %>%
          dplyr::select(-bound)

        # merge long format dataframes before plotting
        group_perc_table_long <- group_perc_out_table_long %>%
          merge(group_perc_low_table_long, by=c("CROP","warming")) %>%
          merge(group_perc_high_table_long, by=c("CROP","warming"))# %>%
          #mutate(CROP = fct_reorder(CROP, perc_out_SCS, .desc=TRUE))

        plot_bar(group_perc_table_long, group, results_path, barplot_pal, aggreg = '_aggreg', food_list, region)

        # save barplot table as csv
        write.csv(group_perc_table_long, paste0(results_path, 'tables/', group, '/perc_out_SCS_barplot', group, '_', region, '.csv', sep=""))



      }
    }

    ## diversity change maps ##
    # percentage or total number outside / within SCS by crop group, on map
    div_rast_path <- paste(results_path, group, '/diversity_change_total_', group, '.tiff', sep="")
    r_div_change <- rast(div_rast_path)

    div_total_rast_path <- paste(results_path, group, '/in_div_tot_', group, '.tiff', sep="")
    r_tot_div_change <- rast(div_total_rast_path)
    
    if (group == 'FOOD') {
      ncrops = group_codes %>%
        filter(food == '1') %>%
        summarise(length(food)) %>%
        pull()
    } else if (group == 'NONFOOD') {
      ncrops = group_codes %>%
        filter(food == '0') %>%
        summarise(length(food)) %>%
        pull()
    } else {
      ncrops = group_codes %>%
        filter(fao_group == group) %>%
        summarise(length(fao_group)) %>%
        pull()
    }

    # plot and save percentual change
    for (k in 1:4) {
      layer_perc <- r_div_change[[k]]
      if (k > 1) {
        temp = as.character(k)
      } else {
        temp = "1.5"
      }

     plot_diversity(layer_perc, group, ncrops, temp, results_path, coastline, pal = 'vik', tocrs = "+proj=robin +over", r_land)
    }

    # plot and save total diversity
    for (j in 1:5) {
      layer_tot <- r_tot_div_change[[j]]
      if (j > 2) {
        temp = as.character(j-1)
      } else if (j == 2) {
        temp = "1.5"
      } else {
        temp = 'baseline'
      }

      plot_total_diversity(layer_tot, group, ncrops, temp, results_path, coastline, pal = 'bamako', tocrs = "+proj=robin +over", r_land)
    }
     
    ## regional/zonal area in diversity change categories

    # read data, geographical regions, elevation zones and latitude zones
    reg_area_change_path <- paste(results_path,'tables/', group,  '/div_change_regional_area_cat_', group, '.csv', sep="")
    reg_area_change <- read.csv(reg_area_change_path) %>%
      filter(region != 'Antarctica') %>%
      dplyr::select(-c(tot_area_region, perc_area_region))
    
    
    if (spam_year == '2020' & !comb_2020) {

      lat_area_change_path <- paste(results_path,'tables/', group,  '/div_change_lat_zones_area_cat_', group, '.csv', sep="")
      lat_area_change <- read.csv(lat_area_change_path) %>%
        filter(lat_region != 'Global') %>%
        dplyr::select(-c(tot_area_region, perc_area_region))
  
      elev_area_change_path <- paste(results_path,'tables/', group,  '/div_change_elev_zones_area_cat_', group, '.csv', sep="")
      elev_area_change <- read.csv(elev_area_change_path) %>%
        filter(elev_region != 'Global') %>%
        dplyr::select(-c(tot_area_region, perc_area_region)) %>%
        mutate(elev_region = rep(c("less_than_800", "800_to_1500", "1500_to_2500", "more_than_2500"), 4))
      
    }

      
    col_names <- c("region", "scenario", "l_-100", "k_-99.99 to -75", "j_-75 to -50", "i_-50 to -25", "h_-25 to 0", "g_no change",
                      "f_0 to +25", "e_+25 to +50", "d_+50 to +75", "c_+75 to +100",
                      "b_cropland with emerging climatic potential", "a_marginal in baseline, outside SCS in scen")
    
    colnames(reg_area_change) <- col_names
    
    
    if (spam_year == '2020' & !comb_2020) {
      colnames(lat_area_change) <- col_names
      colnames(elev_area_change) <- col_names
    }

    regions_and_global <- c(regions, 'Global')

    # loop through geographical regions
    for (reg in regions_and_global) {

      # transform into long format for plotting
      reg_area_change_long <- reg_area_change %>%
        filter(region == reg) %>%
        pivot_longer(cols = - c(region, scenario),
                     names_to = 'change_cat',
                     values_to = 'perc_area_in_cat') %>%
        mutate(perc_area_in_cat = perc_area_in_cat * 100)

      plot_regional_area_change_cats(reg_area_change_long, group, results_path, reg)

    }
    
    
    if (spam_year == '2020' & !comb_2020) {

      lat_zones <- unique(lat_area_change$region)
  
      # loop through latitude zones
      for (lat_zone in lat_zones) {
  
        # transform into long format for plotting
        lat_area_change_long <- lat_area_change %>%
          filter(region == lat_zone) %>%
          pivot_longer(cols = - c(region, scenario),
                       names_to = 'change_cat',
                       values_to = 'perc_area_in_cat') %>%
          mutate(perc_area_in_cat = perc_area_in_cat * 100)
  
        plot_regional_area_change_cats(lat_area_change_long, group, results_path, paste0('lat_zone_',lat_zone))
  
      }
  
      elev_zones <- unique(elev_area_change$region)
  
      # loop through elevation zones
      for (elev_zone in elev_zones) {
  
        # transform into long format for plotting
        elev_area_change_long <- elev_area_change %>%
          filter(region == elev_zone) %>%
          pivot_longer(cols = - c(region, scenario),
                       names_to = 'change_cat',
                       values_to = 'perc_area_in_cat') %>%
          mutate(perc_area_in_cat = perc_area_in_cat * 100)
  
        plot_regional_area_change_cats(elev_area_change_long, group, results_path, paste0('elev_zone_',elev_zone))
  
      }
    
    }

   }
    
}
