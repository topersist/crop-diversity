function f_plot_baseline(dir, var_info, crop_file_name, crop_name)
%% categories:
    
    cd(dir)
    load('holdridge_data\hLand.mat')
    %% Import data to plot
    if strcmp(var_info,'res') | strcmp(var_info,'res_cats')
        % Import resilience data for year 2010
        res_time = ncread('ref_data\resilience.nc', 'time');
        res_all = ncread('ref_data\resilience.nc', 'resilience');
        data = res_all(:,:,res_time == 2010)';
        data(data == -9) = NaN;
    
    elseif strcmp(var_info,'dir_rcp26')
        % For visualizing category of change, calculate the mode across
        % GCMs
        load('holdridge_data\holdridge_resultsMap_year2090_2.mat');
        data = squeeze(mode(hold_map_change(:,:,3,1,:),5));

     elseif strcmp(var_info,'dir_rcp45')
        load('holdridge_data\holdridge_resultsMap_year2090_2.mat');
        data = squeeze(mode(hold_map_change(:,:,3,2,:),5));
        
    elseif strcmp(var_info,'dir_rcp85')
        load('holdridge_data\holdridge_resultsMap_year2090_2.mat');
        data = squeeze(mode(hold_map_change(:,:,3,3,:),5));
        
    else
        % If livestock, or food crop production data take log10 to better
        % visualize the data
        data = log10(f_import_ref_data(dir, var_info, crop_file_name));
    end
%%    
    % Cells that are not on land are set to NaN
    data(~hLand) = NaN;
    
    % For making livestock, and food crop production colormap, create a
    % temporary data vector for those, where are except cells with data are
    % removed
    data_prc = data(:);
    data_prc = data_prc(~isnan(data_prc) & ~isinf(data_prc) & (data_prc ~= 0));
%%
	% Create a different colormap for all the data set visualized
    if strcmp(var_info,'livestock_au')
        cmap = crameri('lajolla');
        clim = [0, prctile(data_prc,95)];
        data(isinf(data)) = 0;

    elseif strcmp(var_info,'crop_spam')
        cmap = crameri('-bamako');
        % Modify colormap slightly, to remove sensitivity in the lower
        % range, as most of the data is in the higher range
        cmap = cmap([1:1:127,128:5:256],:);
        clim = [0, prctile(data_prc,95)];
        clim
        data(isinf(data)) = 0;

    elseif strcmp(var_info,'res')
        cmap = crameri('-vik');
        % Modify colormap slighlty to offset the center of the color range to
        % zero
        cmap = cmap([linspace(1,127,64),linspace(129,256,128)],:);
        clim = [-0.5, 1];

    elseif strcmp(var_info,'res_cats')
        cmap = crameri('-vik',4);
        % Find percentile values for four equal interval categories
        [res_cats, temp] = f_hold_rast_categories(data, data, 4, 4);
        res_cats

        res_cats_map = zeros(size(data));
        res_cats_map(isnan(data)) = nan;
        
        % Create map based on the categories defined above
        for i = 1:4
            res_cats_map(data >= res_cats(i) & data < res_cats(i+1)) = i;
        end

        data = res_cats_map;
        clim = [1, 4];

    elseif strcmp(var_info,'dir_rcp26') | strcmp(var_info,'dir_rcp45') | strcmp(var_info,'dir_rcp85')
        cmap = crameri('-roma',3);
        clim = [1,3];
    end

    R = georasterref('LatitudeLimits',[-90 90], ['LongitudeLimits'],[-180 180],'RasterSize',[2160 4320]);
    load coastlines

    %% Plot data on a map
    figure('units','normalized','outerposition',[0 0 1 1])
    axesm ('robinson', 'Frame', 'off', 'Grid', 'off','MapLatLimit',[-60,90],'MapLonLimit',[-180,180]);
    geoshow(flip(data,1), R,'DisplayType', 'surface');
    geoshow(coastlat,coastlon,'Color', 'black','LineWidth',1)
    set(gca,'XColor','none','YColor','none','XTick',[],'YTick',[],'CLim',clim)
    colormap(cmap)

    filename = strcat('results_review\',var_info,'_',crop_name,'_plot_basis_map.png')
    export_fig(gcf,filename,'-png', '-transparent');

    filename_pdf = strcat('results_review\',var_info,'_',crop_name,'_plot_basis_map.pdf')
    print(gcf,filename_pdf,'-dpdf','-r300', '-bestfit');

    close all

    figure('units','normalized','outerposition',[0 0 1 1])
    colormap(cmap)
    h=colorbar('southoutside');
    set(h,'fontsize',50,'TickLength',10);
    set(gca,'CLim',clim)
    if strcmp(var_info,'res')
        cbarrow()
    elseif strcmp(var_info,'dir_rcp26') | strcmp(var_info,'dir_rcp45') | strcmp(var_info,'dir_rcp85')

    else
        cbarrow('right')
    end
    filename_cb = strcat('results_review\',var_info,'_',crop_name,'_colorbar_plot_basis.pdf')
    print(gcf,filename_cb,'-dpdf','-r300', '-bestfit');
    close all

    
end





