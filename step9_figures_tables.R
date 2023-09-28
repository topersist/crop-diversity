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

### FUNCTIONS SECTION BEGINS, do not modify ###

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
                     labels = c("1.5 C\n(SSP1-2.6, 2030)","2 C\n(SSP1-2.6, 2090)", 
                                "3 C\nSSP2-4.5, 2090)", "4 C\n(SSP3-7.0, 2090)",
                                "5 C\n(SSP5-8.5, 2090)")) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,100)) + 
    ylab("% of current production outside SCS") +
    xlab("Global warming") +
    ggtitle(paste(group, area, sep = " ")) +
    theme(plot.background = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = 'grey80'),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  plot(plt_out_SCS_group_col)
  
  # save bar plots
  bar_plot_file_pdf <- paste(results_path, group, '/perc_out_SCS_bar_', group, aggreg, '_', area, '.pdf', sep="")
  bar_plot_file_png <- paste(results_path, group, '/perc_out_SCS_bar_', group, aggreg, '_', area, '.png', sep="")
  ggsave(bar_plot_file_pdf, plt_out_SCS_group_col, height = 9.5 , width = 16.5, units = "cm")
  ggsave(bar_plot_file_png, plt_out_SCS_group_col, height = 9.5 , width = 16.5, units = "cm")

}

# bar plots of gained and lost potential cropland area at five global warming levels
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
                     labels = c("1.5 C\n(SSP1-2.6, 2030)","2 C\n(SSP1-2.6, 2090)", 
                                "3 C\nSSP2-4.5, 2090)", "4 C\n(SSP3-7.0, 2090)",
                                "5 C\n(SSP5-8.5, 2090)")) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(min_low - 10, max_high + 10)) + 
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

# vertical bar plots of total diversity change by crop
plot_total_area_change <- function(table_area, group, results_path, aggreg = '') {
  
  scens = c('1.5', '2', '3', '4', '5')
  
  # fix order of crops based on ascending order in the 1.5 C scenario,
  # filter only total change
  if (! group %in% c('FOOD', 'NONFOOD')) {
  table_area <- table_area %>%
    filter(change_dir == 'total') %>%
    arrange(`5`) %>%
    mutate(CROP = paste0(c(letters[1:nrow(.)]), '_', CROP))
  } else {
    table_area <- table_area %>%
      filter(change_dir == 'total') %>%
      arrange(desc(fao_group), `5`) %>%
      mutate(CROP = paste0(c(letters[1:nrow(.)-1], 'zz'), '_', CROP))
  }
  
  
  for (scen in scens) {
    
    # select scenario data
    group_area_change_tot <- table_area %>%
      dplyr::select(CROP, starts_with(scen))
    
    colnames(group_area_change_tot) <- c('CROP', 'median', 'limit_low', 'limit_high')
    
    #group_area_change_tot <- group_area_change_tot %>%
      #mutate(CROP = fct_reorder(CROP, median, .desc = FALSE))
    
    
    plt_area_change <- ggplot(data = group_area_change_tot, aes(x = CROP, y = median)) +
      geom_col(fill = "#E17A4F") +
      geom_errorbar(data = group_area_change_tot, aes(x = CROP,
                                                      ymin=limit_low, ymax=limit_high),
                    position = position_dodge(width=0.9),
                    width = 0.25) +
      scale_y_continuous(limits = c(-60, 60), expand = c(0,0))+
      scale_x_discrete(expand = c(0,0)) +
      ylab(paste("Net change in cropland area within SCS (%),", scen, "C global warming")) +
      xlab("Crop / Crop group") +
      theme(plot.background = element_blank(), 
            panel.grid.minor.x = element_line(colour = 'grey80'),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line(colour = 'grey80'),
            panel.grid.major.y = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      coord_flip()
    
    plot(plt_area_change)
    
    scen_file_pdf <- paste(results_path, group, '/total_area_change_', scen, aggreg, '.pdf', sep="")
    ggsave(scen_file_pdf, plt_area_change, height = 20 , width = 15, units = "cm")
    
    
  }
  
  # plots including total change from all scenarios
  
  # transform area change to long format
  group_area_change_table <- table_area %>%
    filter(change_dir == 'total') %>%
    dplyr::select(CROP, `1.5`, `2`, `3`, `4`, `5`, change_dir, fao_group) %>%
    pivot_longer(cols=-c(CROP, change_dir, fao_group),
                 names_to = "warming",
                 values_to = "perc_change_area") %>%
    arrange(fao_group,perc_change_area)
  
  plt_area_change_all_scens <- ggplot(data = group_area_change_table, 
                                      aes(x = CROP, y = perc_change_area,
                                          group = warming)) + 
    geom_col(aes(fill = warming), position = position_identity(), width = 0.9) +
    scale_y_continuous(limits = c(-80, 40), expand = c(0,0))+
    scale_x_discrete(expand = c(0,0)) +
    scale_fill_manual(values = scico(n = 5, palette = 'lajolla', begin = 0.15, end = 0.85, direction = -1)) +
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

# outside SCS percentage maps
plot_out_SCS_perc <- function(r_input, group, temp, results_path, coastline, pal = 'YlOrBr', tocrs = NA) {
  
  # project to another crs
  if (!is.na(tocrs)){
    r_input <- terra::project(r_input, tocrs, mask = TRUE)
  }
  
  # create breaks and palette
  breakvals <-  c(-9999, seq(0,1,0.125))
  breaklab <- c("non-cropland (group-specific)", "0-12.5", "12.5-25", "25-37.5", "37.5-50", "50-62.5", "62.5-75", 
                "75-87.5", "87.5-100")
  
  cols <- scico(n = 8, begin = 0.25, palette='lajolla')
  colorpal <- c("grey95", cols)
  
  # create tmap object
  perc_map <- tm_shape(r_input) +
    tm_raster(palette = colorpal,
              breaks = breakvals,
              labels = breaklab,
              title = "% Crop production outside SCS") +
    tm_shape(coastline, projection = "ESRI:54030") + 
    tm_lines(col = "black", lwd = 0.25)
  
  perc_map
  
  save_path <- paste(results_path, group, '/map_perc_prod_out_SCS_',temp, '_', group, '.pdf', sep="")
  tmap_save(perc_map, save_path)
    
}

# crop diversity potential maps, % change
plot_diversity <- function(r_input, group, ncrops, temp, results_path, coastline, pal = 'vik', tocrs = NA) {
  
  # crop raster to exclude Antarctica from map
  r_input <- crop(r_input, ext(-180, 180, -56, 90))
  
  # project to another crs
  if (!is.na(tocrs)){
    r_input <- terra::project(r_input, tocrs, mask = TRUE)
  }
  
  # create breaks and palette
  #breakvals <-  c(-9999, -1, -0.999, seq(-0.75,1,0.25), ncrops + 1, 9999)
  breakvals <-  c(-Inf, -1.001, -0.999, -0.75, -0.5, -0.25, -0.01, 0.01, 0.25, 0.5, 0.75, ncrops, ncrops + 1, Inf)
  breaklab <- c("non-cropland", "-100", "-99.99 to -75", "-75 to -50", "-50 to -25", "-25 to 0", "no change",
                "0 to +25", "+25 to +50", "+50 to +75", "+75 to +100", "cropland with emerginc climatic potential",
                "marginal in baseline, outside SCS in scenario")
  
  cols <- scico(n = 10, palette = pal, direction = -1)
  colorpal <- c("grey95", cols, "orangered", "grey60")
  
  # create tmap object
  diversity_map <- tm_shape(r_input) +
    tm_raster(palette = colorpal,
              breaks = breakvals,
              labels = breaklab,
              midpoint = 0,
              interval.closure = 'right',
              legend.reverse = TRUE,
              title = "% Change in crop diversity potential") + 
    tm_shape(coastline, projection = "ESRI:54030") + 
    tm_lines(col = "black", lwd = 0.25)
  
  diversity_map
  
  save_path <- paste(results_path, group, '/map_crop_diversity_potential_change',temp, '_', group, '.pdf', sep="")
  tmap_save(diversity_map, save_path)
  
  
}

# crop diversity potential maps, total diversity
plot_total_diversity <- function(r_input, group, ncrops, temp, results_path, coastline, pal = 'bamako', tocrs = NA) {
  
  # crop raster to exclude Antarctica from map
  r_input <- crop(r_input, ext(-180, 180, -56, 90))
  
  # project to another crs
  if (!is.na(tocrs)){
    r_input <- terra::project(r_input, tocrs, mask = TRUE)
  }
  
  # create breaks and palette
  if (! group %in% c('FOOD', 'NONFOOD')) {
    breakvals <-  c(-9999, seq(-1, ncrops, 1))
    breaklab <- c("non-cropland", as.character(seq(0, ncrops, 1)))
  } else {
    breakvals <-  c(-9999, -1, seq(0, ncrops, 3))
    breaklab <- c("non-cropland", as.character(seq(0, ncrops, 3)))
  }
  
  cols <- scico(n = length(breaklab)-1, palette = pal, direction = -1)
  colorpal <- c("grey95", cols)
  
  # create tmap object
  diversity_map <- tm_shape(r_input) +
    tm_raster(palette = colorpal,
              breaks = breakvals,
              labels = breaklab,
              interval.closure = 'right',
              legend.reverse = TRUE,
              title = paste0("Total potential diversity,\n", temp, "C")) + 
    tm_shape(coastline, projection = "ESRI:54030") + 
    tm_lines(col = "black", lwd = 0.25) +
    tm_layout(legend.position = c('left', 'bottom'))
  
  (diversity_map)
  
  save_path <- paste(results_path, group, '/map_crop_total_diversity_potential',temp, '_', group, '.pdf', sep="")
  tmap_save(diversity_map, save_path)
  
  
}

# first scenario to push cell outside SCS maps (Figure 1)
plot_first_scen_out <- function(map, group, results_path, coastline, limit, tocrs = NA, pal = 'lajolla') {
  
  
  # crop raster to exclude Antarctica from map
  map <- crop(map, ext(-180, 180, -56, 90))
  
  # project to another crs
  if (!is.na(tocrs)){
    map <- terra::project(map, tocrs, mask = TRUE)
  }
  
  # create breaks and palette
  breakvals <-  c(-Inf, -9999, 0, 1.5, 2, 3, 4, 5)
  breaklab <- c("Non-cropland", "Within SCS in all scenarios", "1.5 C (SSP1-2.6, 2030)",
                "2 C (SSP1-2.6, 2090)", "3 C (SSP2-4.5, 2090)", "4 C (SSP3-7.0, 2090)",
                "5 C (SSP5-8.5, 2090)")
  
  cols <- scico(n = 5, palette = pal, direction = -1, begin = 0.15, end = 0.85)
  colorpal <- c("grey95", 'lightblue3', cols)
  
  legend_title <- paste0("First global warming scenario to push ", limit * 100, 
                        "% of\ncurrent crop production outside the SCS")
  
  # create tmap object
  scen_out_map <- tm_shape(map) +
    tm_raster(palette = colorpal,
              breaks = breakvals,
              labels = breaklab,
              interval.closure = "right",
              title = legend_title) + 
    tm_shape(coastline, projection = "ESRI:54030") + 
    tm_lines(col = "black", lwd = 0.25) + 
    tm_layout(legend.position = c(0, 0.25),
              legend.title.size = 1,
              bg.color = NA, frame = FALSE)
  
  scen_out_map
  
  save_path <- paste(results_path, group, '/first_scen_out_', group, '_', limit, '.pdf', sep="")
  tmap_save(scen_out_map, save_path)
  
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
                     labels = c("1.5 C\n(SSP1-2.6, 2030)","2 C\n(SSP1-2.6, 2090)", 
                                "3 C\nSSP2-4.5, 2090)", "4 C\n(SSP3-7.0, 2090)",
                                "5 C\n(SSP5-8.5, 2090)")) +
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
  
  cols <- scico(n = 5, palette = pal, direction = 1, begin = 0.15, end = 0.85)
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
                      name = "First scenario to push\nat least 25%\nof current production\noutside SCS") +
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

# background map for regional figures on top of map
plot_background <- function(coastline, r_land, results_path) {
  
  # crop land mask raster to exclude Antarctica from map
  r_land <- crop(r_land, ext(-180, 180, -56, 90))
  
  # project to robinson crs
  r_land <- terra::project(r_land, "+proj=robin +over", mask = TRUE)

  
  breakvals <- c(-1, 0.1, 1.1)
  cols <- c('white', 'grey95')
  
  plt_bg_map <- tm_shape(r_land) +
                tm_raster(palette = cols,
                          breaks = breakvals,
                          legend.show = FALSE) +
                tm_shape(coastline, projection = "ESRI:54030") + 
                tm_lines(col = "grey60", lwd = 0.25)
  
  
  
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
  colnames(low_table) <- c('region', paste(c(0,1.5,2,3,4,5),'low', sep="_"))
  
  high_table <- in_table %>%
    filter(out_limit == '75') %>%
    dplyr::select(-out_limit)
  colnames(high_table) <- c('region', paste(c(0,1.5,2,3,4,5),'high', sep="_"))
  
  # select 50 % out limit values as the base of table & join with others
  out_table <- in_table %>%
    filter(out_limit == '25') %>%
    dplyr::select(-out_limit) %>%
    left_join(low_table, by = 'region') %>%
    left_join(high_table, by = 'region')
  
  # add numbers from 50% and 75% results in brackets after 25 % result
  out_table <- out_table %>%
    mutate(all_in = paste0(all_in, ' (', `0_low`, ' - ', `0_high`, ')'),
           all_1.5 = paste0(cum_1.5, ' (', `1.5_low`, '-', `1.5_high`, ')'),
           all_2 = paste0(cum_2, ' (', `2_low`, ' - ', `2_high`, ')'),
           all_3 = paste0(cum_3, ' (', `3_low`, ' - ', `3_high`, ')'),
           all_4 = paste0(cum_4, ' (', `4_low`, ' - ', `4_high`, ')'),
           all_5 = paste0(cum_5, ' (', `5_low`, ' - ', `5_high`, ')')) %>%
    dplyr::select(region, starts_with('all_'))
  
  save_path <- paste(results_path, 'tables/', group, '/first_scen_out_table_pretty.csv', sep= '')
  write.csv(out_table, save_path, row.names = FALSE)
  
  
}


### FUNCTIONS SECTION ENDS; ANALYSIS CODE BEGINS ###

# set working directory to the folder of this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
working_dir <- getwd()

results_path <- paste0(working_dir, '/results_review_200323_2010/')

## do not modify from here on ##

# crop group coding
group_codes <- read.xlsx('ref_data/fao_food_groups.xlsx')
group_list <- c(sort(unique(group_codes$fao_group)[1:5]), unique(group_codes$fao_group)[6:9])
food_list <- sort(unique(group_codes[group_codes$food == 1,]$fao_group))

barplot_pal <- c('#D59977', '#DDCC77', '#44AA99', '#88CCEE', '#CC6677', '#882255',
                 '#44AA99', '#88CCEE', '#CC6677', '#882255')


names(barplot_pal) <- c('CEREALS1', group_list)

# load land mask and coastline
land_data <- read_mat('holdridge_data/hLand.mat')
r_land <- rast(res=c(1/12, 1/12))
values(r_land) <- land_data$hLand

#coastline <- read_sf('ref_data/ne_50m_coastline.shp')

coastline <- ne_download(scale = 50, type = "coastline",
                         category = "physical", returnclass = "sf")

# make background map for fig 2 and fig 6
plot_background(coastline, r_land, results_path)

# individual crop groups or individual crops?
groupings <- c('group', 'food')#, 'nonfood')

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
        plot_first_scen_out(map, group, results_path, coastline, out_limit[lyr], tocrs = "+proj=robin +over")

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
               cum_4 = X1.5 + X2 + X3 + X4,
               cum_5 = X1.5 + X2 + X3 + X4 + X5) %>%
        select(-starts_with("X"))

      # neater version of regional table
      table_first_scen_out(first_scen_out_table_cum, results_path, group)

      # regional bar plots
      first_scen_out_25 <- first_scen_out_table %>%
        filter(out_limit == 0.25) %>%
        rename_with(~ gsub("X", "", .x, fixed = TRUE)) %>%
        dplyr::select(-out_limit)
      
      colnames(first_scen_out_25) <- c('region', 'a_0', 'f_1.5', 'e_2', 'd_3', 'c_4', 'b_5')

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
    colnames(SCS_table_regional) <- sub("X", "", colnames(SCS_table))
    names(SCS_table_regional)[19] <- "subregion"
    names(SCS_table_regional)[18] <- "region_code"

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
        dplyr::select(!c(contains("_"),17)) %>%
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
        dplyr::select(CROP, `1.5`, `2`, `3`, `4`, `5`, change_dir, fao_group) %>%
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
          dplyr::select(!c(contains("_"),17)) %>%
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
        mutate_at(vars(`1.5`:`5_high`), ~ .x * ref_tot) %>%
        left_join(group_codes, by= c("CROP" = "spamcrop")) %>%
        dplyr::select(-19)

      # summarize total prod within each crop group
      SCS_table_prod_sums <- SCS_table_prod %>%
        group_by(fao_group) %>%
        summarise_at(vars(`1.5`:ref_tot), sum) %>%
        ungroup()

      # transform into percentage
      group_perc_table <- SCS_table_prod_sums %>%
        mutate_at(vars(`1.5`:`5_high`), ~ .x / ref_tot) %>%
        rename(CROP = fao_group) %>%
        dplyr::select(-17)

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
        mutate_at(vars(`1.5`:`5_high`), ~ .x / 100 * baseline_area) %>%
        dplyr::select(-20)

      # summarize total prod within each crop group
      area_table_tot_sums <- area_table_tot %>%
        group_by(fao_group, change_dir) %>%
        summarise_at(vars(`1.5`:baseline_area), sum) %>%
        ungroup()

      # transform back into percentage
      group_perc_area_table <- area_table_tot_sums %>%
        mutate_at(vars(`1.5`:`5_high`), ~ .x / baseline_area * 100) %>%
        mutate(CROP = fao_group)

      # experimental plots
      plot_total_area_change(group_perc_area_table, group, results_path, aggreg = '_aggreg')

      # transform area change to long format
      group_area_change_table <- group_perc_area_table %>%
        dplyr::select(CROP, `1.5`, `2`, `3`, `4`, `5`, change_dir) %>%
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

      for (region in regions){

        # select region specific data from regional data frame
        SCS_table_region_spec <- SCS_table_regional %>%
          filter(subregion == region) %>%
          dplyr::select(-c(subregion, region_code))

        # add column for total prod in reference conditions
        SCS_table_prod <- SCS_table_region_spec %>%
          mutate_at(vars(`1.5`:`5_high`), ~ .x * ref_tot) %>%
          left_join(group_codes, by= c("CROP" = "spamcrop")) %>%
          dplyr::select(-19)

        # summarize total prod within each crop group
        SCS_table_prod_sums <- SCS_table_prod %>%
          group_by(fao_group) %>%
          summarise_at(vars(`1.5`:ref_tot), sum) %>%
          ungroup()

        # transform into percentage
        group_perc_table <- SCS_table_prod_sums %>%
          mutate_at(vars(`1.5`:`5_high`), ~ .x / ref_tot) %>%
          rename(CROP = fao_group) %>%
          dplyr::select(-17)

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

    ncrops <- max(as.matrix(r_tot_div_change[[1]]), na.rm = TRUE)

    # plot and save percentual change
    for (k in 1:5) {
      layer_perc <- r_div_change[[k]]
      if (k > 1) {
        temp = as.character(k)
      } else {
        temp = "1.5"
      }

     plot_diversity(layer_perc, group, ncrops, temp, results_path, coastline, pal = 'vik', tocrs = "+proj=robin +over")
    }

    # plot and save total diversity
    for (j in 1:6) {
      layer_tot <- r_tot_div_change[[j]]
      if (j > 2) {
        temp = as.character(j-1)
      } else if (j == 2) {
        temp = "1.5"
      } else {
        temp = 'baseline'
      }

      plot_total_diversity(layer_tot, group, ncrops, temp, results_path, coastline, pal = 'bamako', tocrs = "+proj=robin +over")
    }
     
    ## regional/zonal area in diversity change categories

    # read data, geographical regions, elevation zones and latitude zones
    reg_area_change_path <- paste(results_path,'tables/', group,  '/div_change_regional_area_cat_', group, '.csv', sep="")
    reg_area_change <- read.csv(reg_area_change_path) %>%
      filter(region != 'Antarctica') %>%
      dplyr::select(-c(tot_area_region, perc_area_region))

    lat_area_change_path <- paste(results_path,'tables/', group,  '/div_change_lat_zones_area_cat_', group, '.csv', sep="")
    lat_area_change <- read.csv(lat_area_change_path) %>%
      filter(lat_region != 'Global') %>%
      dplyr::select(-c(tot_area_region, perc_area_region))

    elev_area_change_path <- paste(results_path,'tables/', group,  '/div_change_elev_zones_area_cat_', group, '.csv', sep="")
    elev_area_change <- read.csv(elev_area_change_path) %>%
      filter(elev_region != 'Global') %>%
      dplyr::select(-c(tot_area_region, perc_area_region)) %>%
      mutate(elev_region = rep(c("less_than_800", "800_to_1500", "1500_to_2500", "more_than_2500"), 5))

    col_names <- c("region", "scenario", "l_-100", "k_-99.99 to -75", "j_-75 to -50", "i_-50 to -25", "h_-25 to 0", "g_no change",
                      "f_0 to +25", "e_+25 to +50", "d_+50 to +75", "c_+75 to +100",
                      "b_cropland with emerging climatic potential", "a_marginal in baseline, outside SCS in scen")
    
    colnames(reg_area_change) <- col_names
    colnames(lat_area_change) <- col_names
    colnames(elev_area_change) <- col_names

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
