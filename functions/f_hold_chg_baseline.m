function [rast1_cats, rast2_cats] = f_hold_chg_baseline(dir, rcp, hold_src, N_cats, clim)
%% categories:
    
    cd(dir)

    load('holdridge_data/hLand.mat')
    hLand = cat(3,hLand,hLand);
 
    % Import holdridge change data
    [NA, NA, hold_map_change] = f_import_hold_data(dir, hold_src);
    
    % Calculate median change across GCMs
    hold_map_change = squeeze(nanmedian(hold_map_change(:,:,1,:,:),5));

    % Scale the change with 0.1347, which is the average distance
    % between the holdridge centroids
    hold_m = hold_map_change/0.1347;
    clear NA R* hold_map* mapAnalysis
    
    % Change no land areas to NaN and find percentile values based on
    % N_cats number of categories
    hold_m(hLand == 0) = NaN;
    if strcmp(rcp,'rcp_26') | strcmp(rcp,'rcp_85')
        [hold_cats_rcp26, hold_cats_rcp85] = f_hold_rast_categories(hold_m(:,:,1),hold_m(:,:,3), N_cats , N_cats);
    elseif strcmp(rcp,'rcp_45')
        [hold_cats_rcp26, hold_cats_rcp45] = f_hold_rast_categories(hold_m(:,:,1),hold_m(:,:,2), N_cats , N_cats);
    hold_cats = hold_cats_rcp26
    
    % Select data for correct RCP scenario
    if strcmp(rcp,'rcp26')
        j = 1;
    elseif strcmp(rcp,'rcp45')
        j = 2;
    elseif strcmp(rcp,'rcp85')
        j = 3;
    end
    hold_m = hold_m(:,:,j);

    % Create map based on the categoriza defined above
    hold_cats_map = nan(size(hold_m));
    for i = 1:N_cats
        hold_cats_map(hold_m >= hold_cats(i) & ...
         hold_m < hold_cats(i+1)) = i;
    end
    
    cmap_quant = crameri('-batlow',4);
    cmap = crameri('-batlow');

    R = georasterref('LatitudeLimits',[-90 90],['LongitudeLimits'],[-180 180],'RasterSize',[2160 4320])
    load coastlines
    
    % Plot data on a map
    % Percentiles
    figure('units','normalized','outerposition',[0 0 1 1])
    axesm ('robinson', 'Frame', 'off', 'Grid', 'off','MapLatLimit',[-60,90],'MapLonLimit',[-180,180]);
    geoshow(flip(hold_cats_map,1), R,'DisplayType', 'surface');
    geoshow(coastlat,coastlon,'Color', 'black','LineWidth',1)
    set(gca,'XColor','none','YColor','none','XTick',[],'YTick',[])
    colormap(cmap_quant)
    filename = strcat('results_review/holdridge_quant_basis_',rcp,'_map.png')
    export_fig(gcf,filename,'-png', '-transparent');
    close all

    figure('units','normalized','outerposition',[0 0 1 1])
    h=colorbar('southoutside');
    colormap(cmap_quant)
    set(h,'fontsize',50,'Ticks',[0.125,0.125+0.25,0.125+0.5,0.125+0.75],...
        'TickLabels',{'0-25%','25-50%','50-75%','75-100%'},'TickLength',0);
    print(gcf,'results_review/holdridge_quant_basis_colorb.eps','-depsc2','-r300');
    close all


    % Absolute change
    figure('units','normalized','outerposition',[0 0 1 1])
    axesm ('robinson', 'Frame', 'off', 'Grid', 'off','MapLatLimit',[-60,90],'MapLonLimit',[-180,180]);
    geoshow(flip(hold_m,1), R,'DisplayType', 'surface');
    geoshow(coastlat,coastlon,'Color', 'black','LineWidth',1)
    set(gca,'XColor','none','YColor','none','XTick',[],'YTick',[],'CLim',clim)
    colormap(cmap)
    filename = strcat('results_review/holdridge_abs_basis_',rcp,'_map.png')
    export_fig(gcf,filename,'-png', '-transparent');
    close all

    figure('units','normalized','outerposition',[0 0 1 1])
    colormap(cmap)
    h=colorbar('southoutside');
    set(h,'fontsize',50,'TickLength',0);
    set(gca,'CLim',clim)
    cbarrow('right')
    print(gcf,'results_review/holdridge_abs_basis_colorb.eps','-depsc2','-r300');
    close all
    
    R_geotiff = georasterref('RasterSize', [2160 4320], ...
        'RasterInterpretation', 'cells', 'ColumnsStartFrom', 'north', ...
        'LatitudeLimits', [-90 90], 'LongitudeLimits', [-180 180]);
    
    filename_tiff = strcat(dir,'/results_review/holdridge_change_map_',rcp,'_',hold_src,'.tif')
    hold_m = single(hold_m);
    geotiffwrite(filename_tiff, hold_m, R_geotiff);

end




