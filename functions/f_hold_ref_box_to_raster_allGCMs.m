function [SCS_out, aggreg_SCS_GCMs, region_table, aggreg_area_GCMs] = f_hold_ref_box_to_raster_allGCMs(ref_data,...
    hold_scen_x, hold_scen_y, bool_present_scenario, hLand, region_m, cropland_area, ...
    total_cropland_mask, ref_mask_baseline)


%% Testing
% ref_data = ref_95_prcnt_pres;
% hold_scen_x = hold_map_future(:,:,2,:);
% hold_scen_y = hold_map_future(:,:,3,:);
% bool_present_scenario = bool_box_present_limited;
% hLand = hLand;
% region_m = region_m;
% cropland_area = total_cropland_area;
% total_cropland_mask = total_cropland_mask;
% ref_mask_baseline = ref_95_present_mask;

%%
    % Initialize grid intervals.
    thresholds = linspace(0.0,1.0,101);
    thresholds(1) = -inf;

    % Vectorize data to allow faster computation
    land_v = hLand(:);
    SCS_map_v = zeros(size(land_v));

    % Create empty tables for aggregated results
    aggreg_SCS_GCMs = zeros(size(hold_scen_x,4),2);
    aggreg_area_GCMs = zeros(size(hold_scen_x,4),5);

    % Create region indices
    region_m(region_m == 0) = max(region_m(:))+99;
    region_idx = double(accumarray(region_m(:), region_m(:),[],@nanmax));
    
    % calculate total production within each region
    ref_tot_per_region = accumarray(region_m(:), ref_data(:), [], @nansum);

    % Create table for regional results - columns: region id, sum
    % of reference data, proportion outside SCS (still empty)
    region_table = [region_idx, ref_tot_per_region, zeros(length(region_idx), size(hold_scen_x,4))];

%%
    % Loop through all GCMs
    for GCM_index = 1:size(hold_scen_x,4)
        %%

        % Isolate those intervals in o a 100 x 100  holdridge grid (0.01 spacing)
        % where there is reference data, based on the holdridge x and y
        % coordinates for each GCM.
        hold_scen_x_k = squeeze(hold_scen_x(:,:,:,GCM_index));
        hold_scen_y_k = squeeze(hold_scen_y(:,:,:,GCM_index));
        
        % 95% of production limit changed to 100% for future data,
        % climate projected on all land area
        [~, ~, ~, bool_GCM_scenario] = f_100x100_holdridge_box(hLand, ref_data, cropland_area, hold_scen_x_k, hold_scen_y_k, 0.99999);

        hold_scen_x_v = hold_scen_x_k(:);
        hold_scen_y_v = hold_scen_y_k(:);

        SCS_map_GCM_v = zeros(size(SCS_map_v));

        for i = 1:length(thresholds)-1
            for j = 1:length(thresholds)-1

                % Set 1 (True) to those areas, which fall outside safe climatic space,
                % i.e. areas projected to be within holdridge intervals
                % where there is reference data in the GCM scenario
                % but not in the present day (year 2010) scenario
                if bool_GCM_scenario(j,i) == 1 && bool_present_scenario(j,i) == 0

                    logical_i_j = hold_scen_x_v > thresholds(i) & ...
                        hold_scen_x_v <= thresholds(i+1) & ...
                        hold_scen_y_v > thresholds(j) & ...
                        hold_scen_y_v <= thresholds(j+1);        

                    SCS_map_GCM_v(logical_i_j) = 1;                    

                end
            end
        end
        

        % Calculate proprtion of reference data that fall within (1st column) and
        % outside (2nd column) SCS for each GCM
        aggreg_SCS_GCMs(GCM_index, 1) = sum((SCS_map_GCM_v == 0).*ref_data(:))/sum(ref_data(:));
        aggreg_SCS_GCMs(GCM_index, 2) = sum((SCS_map_GCM_v == 1).*ref_data(:))/sum(ref_data(:));

        % For each GCM add 1 to those areas, where each GCM show a fall outside
        % SCS
        SCS_map_v = SCS_map_v + SCS_map_GCM_v;

        % same as above but on country level
       
        % Filter reference data that will fall outside of SCS in this GCM
        SCS_map_GCM = reshape(SCS_map_GCM_v, size(hLand));
        ref_out_GCM = ref_data.*(SCS_map_GCM == 1);

        % Calculate sum of gained, lost, unchanged and total potential
        % cropland area (masked to only cover total croplandarea
        % % of all crops)
        gained_area = ((SCS_map_GCM == 0) & (ref_mask_baseline == 0)) .* cropland_area .* total_cropland_mask;
        lost_area = (ref_mask_baseline .* (SCS_map_GCM == 1)) .* cropland_area .* total_cropland_mask;
        unchanged_area = (SCS_map_GCM == 0) .* ref_mask_baseline .* cropland_area .* total_cropland_mask;
        total_potential_area = (SCS_map_GCM == 0) .* cropland_area .* total_cropland_mask;
        baseline_area = ref_mask_baseline .* total_cropland_mask .* cropland_area;
        
        aggreg_area_GCMs(GCM_index, 1) = sum(gained_area(:), "omitmissing");
        aggreg_area_GCMs(GCM_index, 2) = sum(lost_area(:), "omitmissing");
        aggreg_area_GCMs(GCM_index, 3) = sum(unchanged_area(:), "omitmissing");
        aggreg_area_GCMs(GCM_index, 4) = sum(total_potential_area(:), "omitmissing");
        aggreg_area_GCMs(GCM_index, 5) = sum(baseline_area(:), "omitmissing");

        % Add data to country level table, in the column reserved for this
        % GCM. Proportion of ref data outside of SCS.
        region_table(:, GCM_index + 2) = accumarray(region_m(:),ref_out_GCM(:),[],@sum) ./ ref_tot_per_region;    
%%
    end

    %%
    % Reshape the vectorized data to matrix format
    SCS_map = reshape(SCS_map_v, size(hLand));

    % Categorize the data based on the number of GCMs that fall outside safe
    % climtic space
    SCS_out = zeros(size(hLand));
    SCS_out(SCS_map == 0) = 1;
    SCS_out(SCS_map > 0 & SCS_map <= 3) = 2;
    SCS_out(SCS_map > 3 & SCS_map <= 6) = 3;
    SCS_out(SCS_map > 6 & SCS_map <= 8) = 4;
    
    % Isolate non-land areas
    SCS_out(~hLand) = -9999;

    % remoze all zero rows from regional table
    region_table(region_table(:,1) == 0,:) = [];
    
end





