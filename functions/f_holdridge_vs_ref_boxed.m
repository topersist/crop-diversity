function [aggreg_SCS_med, aggreg_SCS_GCMs, glob_ref_table_out,....
    cntry_table, ref_map_GCMs, ref_map_GCMs_total_cropland,....
    ref_map_GCMs_crop_specific, ref_map_med, ref_map_med_total_cropland,....
    ref_map_med_crop_specific, region_table_GCMs, aggreg_area_GCMs] = f_holdridge_vs_ref_boxed(dir,...
    crop_name,...
    ref_src, ...
    ref_m,...
    total_cropland_mask,...
    total_cropland_area,...
    hLand,...
    cntry_m,...
    region_m, ...
    R_region, ...
    hold_src, ...
    clim_scen, ...
    spam_year, ...
    GCMs_yes,...
    cntry_yes,...
    ternary_yes, ...
    masking) % "all" (all masks and no mask), "crop" (no mask and crop-specific), or "total" (no mask and total cropland)

    %% test
    % dir = working_dir;
    % crop_name = spamcrop_name;
    % ref_src = 'crop_spam';
    % ref_m = ref_m;
    % total_cropland_mask = total_cropland_mask;
    % total_cropland_area = total_cropland_area;
    % hLand = hLand;
    % cntry_m = cntry_m;
    % region_m = region_m;
    % R_region = R_region;
    % hold_src = args{1};
    % clim_scen = args{2};
    % spam_year = spam_year;
    % GCMs_yes = args{3};
    % cntry_yes = args{4};
    % ternary_yes = args{5};
    % masking = args{6};
    % 

    %%

    % initialize return variables
    aggreg_SCS_med = NaN;
    aggreg_SCS_GCMs = NaN;
    aggreg_area_GCMs = NaN;
    glob_ref_table_out = NaN;
    cntry_table = NaN;
    region_table_GCMs = NaN;
    ref_map_med = NaN;
    ref_map_med_total_cropland = NaN;
    ref_map_med_crop_specific = NaN;
    ref_map_GCMs = NaN;
    ref_map_GCMs_total_cropland = NaN;
    ref_map_GCMs_crop_specific = NaN;


   
    %% Import holdridge data
    cd(dir)

    % Reference data (i.e. food crop production):    
    ref_m_present = ref_m;
    
    % Holdridge data
    [hold_map_present, hold_map_future, NA] = f_import_hold_data(dir, hold_src);

    
    % Select ssp index
    if strcmp(clim_scen, '')
        ssp_i = 0;
    elseif strcmp(clim_scen,'ssp26')
        ssp_i = 1;
    elseif strcmp(clim_scen,'ssp45')
        ssp_i = 2;
    elseif strcmp(clim_scen,'ssp70')
        ssp_i = 3;
    elseif strcmp(clim_scen,'ssp85')
        ssp_i = 4;
    end

    clear hold_map_change mapAnalysis NA

    %% Holdridge mapping for present day (2010) scnario
    
    % Present day holdrige mapping for food crop production. Limited with
    % 95% productions. aggreg box : total production in holdridge bins, 
    % bool_box : boolean of aggreg box
    [aggreg_box_present, aggreg_box_present_area, ~, bool_box_present_limited] = f_100x100_holdridge_box(ref_m_present, total_cropland_area, hLand, hold_map_present(:,:,2), hold_map_present(:,:,3), 0.95);

    % Creata a spatial mask for current food crop production
    % that includes Holdridge areas that hold 95% of total food crop production
    ref_95_present_mask = f_hold_mask_ref_raster(ref_m_present, hold_map_present(:,:,2), hold_map_present(:,:,3), bool_box_present_limited);
    ref_95_prcnt_pres = ref_m_present.*ref_95_present_mask;

    %% Aggregate all crop production and cropland area to a 100x100 holdridge grid (future)
    % that is based on median holdridge x and y coordinates (i.e. PET and
    % prec) in the future climate scenario. Climate projection on all land area (hLand)
    if ssp_i > 0
        % Calculate median across different GCMs
        hold_med = median(hold_map_future,4, 'omitnan');
    
        [~, aggreg_box_med, aggreg_box_med_area, bool_box_med_nolim] = f_100x100_holdridge_box(hLand, ref_95_prcnt_pres, total_cropland_area, hold_med(:,:,2),hold_med(:,:,3), 0.99999);
       
    else
        [~, aggreg_box_med, aggreg_box_med_area, bool_box_med_nolim] = f_100x100_holdridge_box(hLand, ref_95_prcnt_pres, total_cropland_area, hold_map_present(:,:,2) ,hold_map_present(:,:,3), 0.99999); 
        
    end

    % Spatial cropland masks
    % Total crop-specific cropland
    ref_present_mask = logical(ref_m_present);

    % Total cropland mask: total_cropland_mask, an input argument

    %% Holdridge mapping (median), future

    % Median aggregates to matrix and median global table
    % arguments: crop raster, PET, prec, boolean present holdridge bins,
    % future boolean holdridge bins, land mask
    if ssp_i > 0
        [ref_map_med, aggreg_SCS_med] = f_box_to_raster_median_results(ref_95_prcnt_pres, hold_med(:,:,2), hold_med(:,:,3), bool_box_med_nolim, bool_box_present_limited, hLand);
        % ref_map med: map matrix, 1 means outside SCS and 0 means within
        % aggreg_SCS_med: absolute (1st row) & percentage (2nd row) of production
        % outside (1st col) / within (2nd col) SCS
    else
        % no need to project into the future in baseline case, just use the
        % existing mask but inverted
        ref_map_med =  double(~ref_95_present_mask);
        
        % Calculate the sum an percentage of the reference data 
        % outside and within SCS
    
        abs_outside = sum(sum((ref_map_med == 1) .*ref_95_prcnt_pres, 'omitnan'), 'omitnan');
        abs_within = sum(sum((ref_map_med == 0) .*ref_95_prcnt_pres, 'omitnan'), 'omitnan');
    
        perc_outside = abs_outside / sum(sum(ref_95_prcnt_pres, 'omitnan'), 'omitnan');
        perc_within = abs_within / sum(sum(ref_95_prcnt_pres, 'omitnan'), 'omitnan');
    
        % Create a table of the global aggregates
        aggreg_SCS_med = [abs_outside, abs_within; perc_outside, perc_within];
        
    end

    % Isolate non-land areas  
    ref_map_med(~hLand) = -9999;


    %% Proportion of GGCMs that  aggregates to matrix and median global table
    if GCMs_yes == true
       
        % ref_map_GCMs: map where higher value -> increasing probability of
        % being outside of SCS (proportion of GCMs that put it outside of SCS)
        % aggreg_SCS_GGCMs: proprtion of reference data that fall within (1st column) and outside (2nd column) SCS
        % aggreg_SCS_GCM_country: proprtion of reference data that fall
        % within (1st column) and outside (2nd column) SCS in each country

        if ssp_i > 0

            [ref_map_GCMs, aggreg_SCS_GCMs, region_table_GCMs, aggreg_area_GCMs] = ...
            f_hold_ref_box_to_raster_allGCMs(ref_95_prcnt_pres,...
                hold_map_future(:,:,2,:), hold_map_future(:,:,3,:),...
                bool_box_present_limited, hLand, region_m, ...
                total_cropland_area, total_cropland_mask, ref_95_present_mask);
        
        else % baseline, i.e. present day results without multiple GCMs
                     
            % Categorize the data based on if the cell will fall outside safe
            % climatic space
            ref_map_GCMs = ref_map_med;
            ref_map_GCMs(ref_map_med == 0) = 1; % within
            ref_map_GCMs(ref_map_med == 1) = 4; % outside

            % Isolate non-land areas
            ref_map_GCMs(~hLand) = -9999;

            aggreg_row = [sum(aggreg_SCS_med(2,:)), 1 - sum(aggreg_SCS_med(2,:))];
            aggreg_SCS_GCMs= repmat(aggreg_row, size(hold_map_future,4),1);

            aggreg_area_GCMs = zeros(size(hold_map_future,4),5);

            % Create region indices
            region_m(region_m == 0) = max(region_m(:))+99;
            region_idx = double(accumarray(region_m(:), region_m(:),[],@(x) max(x, [], 'omitnan')));
            
            % calculate total production within each region
            ref_tot_per_region = accumarray(region_m(:), ref_m(:), [], @(x) sum(x, [], 'omitnan'));
        
            % Create table for regional results - columns: region id, sum
            % of reference data, proportion outside SCS (all zero in baseline scenario)
            region_table_GCMs = [region_idx, ref_tot_per_region, zeros(length(region_idx), 8)];

            % remove all zero rows from country table
            region_table_GCMs(region_table_GCMs(:,1) == 0,:) = [];

        end

    % Aggregate results globally
  
        glob_ref_GCMs_abs = zeros(4,1);
        % here, larger i = increasing probability of being outside of the
        % SCS, hence 1 is the best and 4 the most critical/worst

        for i = 1:4
            % the global aggregated major crop production area within each
            % probability of being outside of SCS
  
            glob_ref_GCMs_abs(i,1) = sum(sum(ref_95_prcnt_pres.*(ref_map_GCMs == i), 'omitnan'), 'omitnan');

        end

        glob_ref_GCMs_prop = glob_ref_GCMs_abs / sum(glob_ref_GCMs_abs);

        glob_ref_table_out = [glob_ref_GCMs_abs, glob_ref_GCMs_prop];
       
    

    %% Aggregate results for each country
    
        if cntry_yes == true
        
            % Create country indices
            cntry_m(cntry_m == 0) = max(cntry_m(:))+999;
            cntry_idx = double(accumarray(cntry_m(:), cntry_m(:),[],@(x) max(x, [], 'omitnan')));
        
            % Calculate sum of ref data outside safe climatic space (at least half of the
            % GCMs indicate so) for each country, using
            % ref_95_prcnt_hold_pres = major production
            ref_xtrm = ref_95_prcnt_pres.*(ref_map_GCMs == 3 | ref_map_GCMs == 4);
    
            ref_tot_per_cntry = accumarray(cntry_m(:), ref_95_prcnt_pres(:), [], @(x) sum(x, 'omitnan'));
    
            % Create a table for country level results - columns: country_id, sum
            % of reference data, proportion outside SCS
            cntry_table = [cntry_idx,...
                ref_tot_per_cntry,...
                accumarray(cntry_m(:),ref_xtrm(:),[],@(x) sum(x, 'omitnan')) ./ ref_tot_per_cntry];
    
            cntry_table(cntry_table(:,1) == 0,:) = [];

        
        end
    end

    %%
    if ternary_yes == true
  %%  % Define min and max for PET-ratio (2nd column) and precipitation (1st column)
        class_bound = [62.5 0.125; 16000 32];

    % Define center of the 
        x = linspace(0.005,0.995,100);
        y = linspace(0.005,0.995,100);

        [X, Y] = meshgrid(x,y);

        X = X(:);
        Y = Y(:);

    % Based on the x- and y-coordinates, calculate the location on the axis in
    % the holdridge triangle for precipitation (t1) and PET-ratio (t2). The
    % equations are reverse engineered from f_holdridge_cartesian_coord.m
        t1 = X-0.5*Y;
        t2 = 1-X-0.5*Y;

    % Calculate precipitation and PET-ratio, based on the location on the
    % triangle axes.
        log2_precip = t1*(log2(class_bound(2,1))-log2(class_bound(1,1))) + log2(class_bound(1,1));
        precip = 2.^log2_precip;

        log2_PET_ratio = t2*(log2(class_bound(2,2))-log2(class_bound(1,2))) + log2(class_bound(1,2));
        PET_ratio = 2.^log2_PET_ratio;

    % Check that the numbers make sense.
        %'min P'
        %min(precip);
        %'max P'
        %max(precip);

        %'min PET'
        %min(PET_ratio);
        %'max PET'
        %max(PET_ratio);

    % Export the data
        aggreg_box_present(~bool_box_present_limited) = 0;
        aggreg_box_med(~bool_box_med_nolim) = 0;

        v_aggreg_box_present = aggreg_box_present(:);
        v_aggreg_box_med = aggreg_box_med(:);
        v_aggreg_box_present_area = aggreg_box_present_area(:);
        v_aggreg_box_med_area = aggreg_box_med_area(:);

        filename = strcat(dir,'/results/ternary_mapping/', crop_name, '/holdridge_PET_precip_tabulted_',ref_src,'_',crop_name, spam_year, '_',clim_scen,'_',hold_src,'.csv')
        
        % columns: 
        % - PET_ratio - pet values in the 100x100 grid 
        % - precip - precipitation values in the 100x100 grid
        % - v_aggreg_box_present - total present production in that PET & precip grid cell,
        % - v_aggreg_box_med - total median future production within that PET & precip grid cell
        % - v_aggreg_box_present_area - total present cropland area in that PET & precip grid cell,
        % - v_aggreg_box_area - total median future cropland area within that PET & precip grid cell

        writematrix([PET_ratio, precip, v_aggreg_box_present, v_aggreg_box_med, v_aggreg_box_present_area, v_aggreg_box_med_area], filename);

        %% Masking the map results
        if strcmp(masking,"all")
            ref_map_med_total_cropland = ref_map_med;
            ref_map_med_total_cropland(~total_cropland_mask) = -9999;
            ref_map_med_crop_specific = ref_map_med;
            ref_map_med_crop_specific(~ref_present_mask) = -9999;
            
            if GCMs_yes
                % in GCM results, assign non-cropland area with no values to -9999 for
                % plotting
    
                ref_map_GCMs_total_cropland = ref_map_GCMs;
                ref_map_GCMs_total_cropland(~total_cropland_mask) = -9999;
                ref_map_GCMs_crop_specific = ref_map_GCMs;
                ref_map_GCMs_crop_specific(~ref_present_mask) = -9999;

            end
            
        elseif strcmp(masking,"total")
            ref_map_med_total_cropland = ref_map_med;
            ref_map_med_total_cropland(~total_cropland_mask) = -9999;
            % in GCM results, assign non-cropland area with no values to -9999 for
            % plotting
            if GCMs_yes
                ref_map_GCMs_total_cropland = ref_map_GCMs;
                ref_map_GCMs_total_cropland(~total_cropland_mask) = -9999;
            end
    
        elseif strcmp(masking, "crop")
            ref_map_med_crop_specific = ref_map_med;
            ref_map_med_crop_specific(~ref_present_mask) = -9999;
            if GCMs_yes == true
                % in GCM results, assign non-cropland area with no values to -9999 for
                % plotting
                ref_map_GCMs_crop_specific = ref_map_GCMs;
                ref_map_GCMs_crop_specific(~ref_present_mask) = -9999;
            end
        
        else
            fprintf("Wrong masking parameter, median and GCM results not masked")
        end

        
    end



