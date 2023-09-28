function table_out = f_map_and_table_for_resilience_vs_holdridge_vs_ref(...
    dir,...
    spamcrop_name,...
    ref_src, ...
    ref_m,...
    res_m,...
    hLand,...
    hold_src, ... 
    clim_scen, ...
    cats1, ...
    cats2, ...
    map_yes,...
    colorbar_yes)
    
    % Land data
    %load('holdridge_data/hLand.mat')

    % Import resilience data for year 2010
    %res_time = ncread('ref_data/resilience.nc', 'time');
    %res_all = ncread('ref_data/resilience.nc', 'resilience');
    %res_m = res_all(:,:,res_time == 2010)';
    %res_m(res_m == -9) = NaN;
    %res_m(hLand == 0) = NaN;

    %clear res_all res_time
    
    % Import holdridge data
    [NA, NA, hold_map_change] = f_import_hold_data(dir, hold_src);
    
    % Calculate median change across GCMs
    hold_map_change = squeeze(nanmedian(hold_map_change(:,:,1,:,:),5));

    % Scale the change with 0.1347, which is the average distance
    % between the holdridge centroids
    hold_m = hold_map_change/0.1347;
    clear NA R* hold_map* mapAnalysis

    %ref_m = f_import_ref_data(dir, ref_src, spamcrop_file_name);
    
    % Holdridge baseline is based on RCP2.6
    hold_m_baseline = hold_m(:,:,1);
    hold_m_baseline(hLand == 0) = NaN;

    % Obtain category limits (based on percentiles) for holdridge and reference data
    [hold_cats res_cats] = f_hold_rast_categories(hold_m_baseline, res_m, cats1, cats2);
    %hold_cats
    %res_cats
    
    % Select which climate change scenario to use
    if strcmp(clim_scen,'rcp26')
        hold_m = hold_m(:,:,1);
    elseif strcmp(clim_scen,'rcp45')
        hold_m = hold_m(:,:,2);
    elseif strcmp(clim_scen,'rcp85')
        hold_m = hold_m(:,:,3);
    end
    

    %% Create a 4x4 oolorscale based on RGB values
    rgb_col = [255, 255, 244, 196; ...
        243, 218, 176, 91; ...
        205, 101, 131, 40];

    rgb_row = [222, 189, 156; ...
        234, 216, 195; ...
        245, 238, 228];

    rgb_cube = zeros(4,4,3);

    for i = 1:4

        rgb_cube(i,:,:) = [rgb_row(:,i-1:-1:1), rgb_col(:,1:5-i)]';

    end

    %% Create map and table for holdridge change and resilience categorizations and aggregate reference data to those categories
    
    % Initialize table and global map of RGB values
    hold_res_table = zeros(4,4);

    red = nan(size(res_m));
    green = nan(size(res_m));
    blue = nan(size(res_m));

    % Calculate the sum of reference data in the different resilience and
    % holdridge percentiles. Also, map the quantiles using the rgb coloring
    % defined above.
    for i = 1:4
        for j = 1:4

            cat_i_j = logical(zeros(size(res_m)));
            
            cat_i_j(res_m >= res_cats(i) & ...
             res_m < res_cats(i+1) & ...
             hold_m >= hold_cats(j) & ...
             hold_m < hold_cats(j+1)) = 1;
            
            hold_res_table(i,j) = sum(sum(ref_m(cat_i_j)));
            
            red(cat_i_j) = rgb_cube(i,j,1);
            green(cat_i_j) = rgb_cube(i,j,2);
            blue(cat_i_j) = rgb_cube(i,j,3);

        end
    end
    
    hold_res_table_prop = hold_res_table / sum(hold_res_table(:));
    
    % Check that output is as expected
    sum(hold_res_table_prop(:));
    
    % Flip table along the 1st axis, so that the smalles resilience and
    % largest holdridge change are in the lower right corner of the table
    hold_res_table = flip(hold_res_table,1);
    hold_res_table_prop = flip(hold_res_table_prop,1);

    % Format a table to be exported from the function
    table_out = [hold_res_table; hold_res_table_prop];
    
    %% Setup map data
    if map_yes == true

        hold_m_stacked = repmat(hold_m,1,1,3);
        res_m_stacked = repmat(res_m,1,1,3);
        
        % Divide RGB values with 255 to be used with the image function
        rgb_var = cat(3,red,green,blue)/255;

        rgb_var(isnan(hold_m_stacked)) = 1;
        rgb_var(isnan(res_m_stacked)) = 1;

        figure
        image(rgb_var);

        map2plot = flip(rgb_var,1);

        %% Plot and save map:

        load coastlines

        figure('units','normalized','outerposition',[0 0 1 1])
        R = georasterref('LatitudeLimits',[-90 90],['LongitudeLimits'],[-180 180],'RasterSize',[2160 4320]);
        axesm ('robinson', 'Frame', 'off', 'Grid', 'off','MapLatLimit',[-60,90],'MapLonLimit',[-180,180]);
        geoshow(map2plot, R);
        geoshow(coastlat,coastlon,'Color', 'black')
        set(gca,'XColor','none','YColor','none','XTick',[],'YTick',[])

        % save figure as fig
        filename = strcat('results_review/', spamcrop_name, '/hold_res_map_',ref_src, '_', spamcrop_name,'_',hold_src,'_',clim_scen,'.fig');
        %export_fig(gcf,filename,'-png');
        hgsave(gcf, filename)
        close all
    end

    %% Create colorbar which includes the above tabulated values
    if colorbar_yes == true
        
        % Divide rgb values with 255 to be used with the image function
        colrbar = flip(rgb_cube,1)/255;

        pos = [1000, 1000, 46.081*10, 18.556*10];

        figure('Position', pos)
        I = image(colrbar);
        set(gca,'XTick',[],'YTick',[],'XTickLabel',[],'YTickLabel',[],'visible','off')

        for i = 1:4
            for j = 1:4
                val = hold_res_table_prop(j,i)*100;
                
                % If proportional aggregated values is above 10 round to
                % closest integer, otherwise to 1 decimal accuracy
                if val >= 10
                    str_out = round(hold_res_table_prop(j,i)*100,0);
                else
                    str_out = round(hold_res_table_prop(j,i)*100,1);
                end
                % Add number to the image
                text(i,j,num2str(str_out),'FontName','Arial','FontSize', 20,'HorizontalAlignment','center')
            end
        end

        % save figure as fig
        filename = strcat('results_review/', spamcrop_name, '/hold_res_colorbar_',ref_src, '_', spamcrop_name,'_',hold_src,'_',clim_scen,'.fig');
        %print(gcf,filename,'-dpng','-r300');
        %export_fig(gcf,filename,'-dpng','-q101');
        hgsave(gcf, filename)
        close all
    end
end
