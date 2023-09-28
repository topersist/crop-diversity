clear; clc

% set the folder where .m file is as a working directory
working_dir = fileparts(matlab.desktop.editor.getActiveFilename);
cd(working_dir)

% do we have resilience results?
res_yes = false; %true or false

% are the results for warming potentials or all years and scenarios?
scenarios = "warming"; %"warming" or "all"

% what is the masking type used in step 4?
masking = "all"; % "all" (all masks and no mask), "crop" (no mask and crop-specific), or "total" (no mask and total cropland)

% what is the results folder name?
matlab_folder = 'results_review/matlab/';

% SPAM data year, as text
spam_year = '2010';

%% Reorganizing triton results for each crop, do not modify from here on

% spamcrop_list = {'WHEA','RICE','MAIZ','BARL','PMIL','SMIL','SORG','OCER',...
%     'POTA','SWPO','YAMS','CASS','ORTS','BEAN','CHIC','COWP','PIGE','LENT',...
%     'OPUL','SOYB','GROU','CNUT','BANA','PLNT','TROF','TEMF','VEGE','OILP',...
%     'SUNF','RAPE','SESA','OOIL','SUGC','SUGB','COTT','OFIB','ACOF','RCOF',...
%     'COCO','TEAS','TOBA','REST'};
spamcrop_list = {'SOYB'};

num_crops = numel(spamcrop_list);
%%
for spamcrop = 1:num_crops
   %%
    main_folder = strcat(matlab_folder, spamcrop_list{spamcrop}, '/main_results', spamcrop_list{spamcrop}, spam_year,'.mat');
    main_save_file = strcat(matlab_folder, spamcrop_list{spamcrop}, '/main_results_re', spamcrop_list{spamcrop}, spam_year,'.mat');
    div_save_file = strcat(matlab_folder, spamcrop_list{spamcrop}, '/main_results_div', spamcrop_list{spamcrop}, spam_year,'.mat');
    
    if res_yes
        tabl_folder = strcat(matlab_folder, spamcrop_list{spamcrop}, '/tbl_hold_res', spamcrop_list{spamcrop}, spam_year,'.mat');
        tabl_save_file = strcat(matlab_folder, spamcrop_list{spamcrop}, '/tbl_hold_res_re', spamcrop_list{spamcrop}, spam_year,'.mat');
    end

    % Main results
    % get main result files for the crop and save under new names
    load(main_folder, '-mat')

%%
    if strcmp(scenarios, "all")
        
        % total land results & masking independent results
        key_mask = strcmp(keys_check, 'agg_rcp85_2070');
        key_loc = find(key_mask);
        crop_med_85_2070 = crop_med(:,:,key_loc);
        crop_GCMs_85_2070 = crop_GCMs(:,:,key_loc);
        SCS_out_85_2070 = SCS_out(:,:,key_loc);
        SCS_nores_out_85_2070 = SCS_nores_out(:,:,key_loc);
        SCS_med_out_85_2070 = SCS_med_out(:,:,key_loc);
        area_GCMs_85_2070 = area_GCMs(:,:,key_loc);
       
        key_mask = strcmp(keys_check, 'agg_rcp85_2050');
        key_loc = find(key_mask);
        crop_med_85_2050 = crop_med(:,:,key_loc);
        crop_GCMs_85_2050 = crop_GCMs(:,:,key_loc);
        SCS_out_85_2050 = SCS_out(:,:,key_loc);
        SCS_nores_out_85_2050 = SCS_nores_out(:,:,key_loc);
        SCS_med_out_85_2050 = SCS_med_out(:,:,key_loc);
        area_GCMs_85_2050 = area_GCMs(:,:,key_loc);
    
        key_mask = strcmp(keys_check, 'agg_rcp85_2030');
        key_loc = find(key_mask);
        crop_med_85_2030 = crop_med(:,:,key_loc);
        crop_GCMs_85_2030 = crop_GCMs(:,:,key_loc);
        SCS_out_85_2030 = SCS_out(:,:,key_loc);
        SCS_nores_out_85_2030 = SCS_nores_out(:,:,key_loc);
        SCS_med_out_85_2030 = SCS_med_out(:,:,key_loc);
        area_GCMs_85_2030 = area_GCMs(:,:,key_loc);
    
        key_mask = strcmp(keys_check, 'agg_rcp70_2070');
        key_loc = find(key_mask);
        crop_med_70_2070 = crop_med(:,:,key_loc);
        crop_GCMs_70_2070 = crop_GCMs(:,:,key_loc);
        SCS_out_70_2070 = SCS_out(:,:,key_loc);
        SCS_nores_out_70_2070 = SCS_nores_out(:,:,key_loc);
        SCS_med_out_70_2070 = SCS_med_out(:,:,key_loc);
        area_GCMs_70_2070 = area_GCMs(:,:,key_loc);
        
        key_mask = strcmp(keys_check, 'agg_rcp70_2050');
        key_loc = find(key_mask);
        crop_med_70_2050 = crop_med(:,:,key_loc);
        crop_GCMs_70_2050 = crop_GCMs(:,:,key_loc);
        SCS_out_70_2050 = SCS_out(:,:,key_loc);
        SCS_nores_out_70_2050 = SCS_nores_out(:,:,key_loc);
        SCS_med_out_70_2050 = SCS_med_out(:,:,key_loc);
        area_GCMs_70_2050 = area_GCMs(:,:,key_loc);
    
        key_mask = strcmp(keys_check, 'agg_rcp70_2030');
        key_loc = find(key_mask);
        crop_med_70_2030 = crop_med(:,:,key_loc);
        crop_GCMs_70_2030 = crop_GCMs(:,:,key_loc);
        SCS_out_70_2030 = SCS_out(:,:,key_loc);
        SCS_nores_out_70_2030 = SCS_nores_out(:,:,key_loc);
        SCS_med_out_70_2030 = SCS_med_out(:,:,key_loc);
        area_GCMs_70_2030 = area_GCMs(:,:,key_loc);

        key_mask = strcmp(keys_check, 'agg_rcp45_2070');
        key_loc = find(key_mask);
        crop_med_45_2070 = crop_med(:,:,key_loc);
        crop_GCMs_45_2070 = crop_GCMs(:,:,key_loc);
        SCS_out_45_2070 = SCS_out(:,:,key_loc);
        SCS_nores_out_45_2070 = SCS_nores_out(:,:,key_loc);
        SCS_med_out_45_2070 = SCS_med_out(:,:,key_loc);
        area_GCMs_45_2070 = area_GCMs(:,:,key_loc);
    
        key_mask = strcmp(keys_check, 'agg_rcp45_2050');
        key_loc = find(key_mask);
        crop_med_45_2050 = crop_med(:,:,key_loc);
        crop_GCMs_45_2050 = crop_GCMs(:,:,key_loc);
        SCS_out_45_2050 = SCS_out(:,:,key_loc);
        SCS_nores_out_45_2050 = SCS_nores_out(:,:,key_loc);
        SCS_med_out_45_2050 = SCS_med_out(:,:,key_loc);
        area_GCMs_45_2050 = area_GCMs(:,:,key_loc);
    
        key_mask = strcmp(keys_check, 'agg_rcp45_2030');
        key_loc = find(key_mask);
        crop_med_45_2030 = crop_med(:,:,key_loc);
        crop_GCMs_45_2030 = crop_GCMs(:,:,key_loc);
        SCS_out_45_2030 = SCS_out(:,:,key_loc);
        SCS_nores_out_45_2030 = SCS_nores_out(:,:,key_loc);
        SCS_med_out_45_2030 = SCS_med_out(:,:,key_loc);
        area_GCMs_45_2030 = area_GCMs(:,:,key_loc);
    
        key_mask = strcmp(keys_check, 'agg_rcp26_2070');
        key_loc = find(key_mask);
        crop_med_26_2070 = crop_med(:,:,key_loc);
        crop_GCMs_26_2070 = crop_GCMs(:,:,key_loc);
        SCS_out_26_2070 = SCS_out(:,:,key_loc);
        SCS_nores_out_26_2070 = SCS_nores_out(:,:,key_loc);
        SCS_med_out_26_2070 = SCS_med_out(:,:,key_loc);
        area_GCMs_26_2070 = area_GCMs(:,:,key_loc);
    
        key_mask = strcmp(keys_check, 'agg_rcp26_2050');
        key_loc = find(key_mask);
        crop_med_26_2050 = crop_med(:,:,key_loc);
        crop_GCMs_26_2050 = crop_GCMs(:,:,key_loc);
        SCS_out_26_2050 = SCS_out(:,:,key_loc);
        SCS_nores_out_26_2050 = SCS_nores_out(:,:,key_loc);
        SCS_med_out_26_2050 = SCS_med_out(:,:,key_loc);
        area_GCMs_26_2050 = area_GCMs(:,:,key_loc);
    
        key_mask = strcmp(keys_check, 'agg_rcp26_2030');
        key_loc = find(key_mask);
        crop_med_26_2030 = crop_med(:,:,key_loc);
        crop_GCMs_26_2030 = crop_GCMs(:,:,key_loc);
        SCS_out_26_2030 = SCS_out(:,:,key_loc);
        SCS_nores_out_26_2030 = SCS_nores_out(:,:,key_loc);
        SCS_med_out_26_2030 = SCS_med_out(:,:,key_loc);
        area_GCMs_26_2030 = area_GCMs(:,:,key_loc);
    
        key_mask = strcmp(keys_check, 'all_rcp26');
        key_loc = find(key_mask);
        crop_med_26 = crop_med(:,:,key_loc);
        crop_GCMs_26 = crop_GCMs(:,:,key_loc);
        crop_res_26 = crop_res(:,:,key_loc);
        crop_cntry_26 = crop_cntry(:,:,key_loc);
        region_GCMs_26 = region_table_GCMs(:,:, key_loc);
        SCS_out_26 = SCS_out(:,:,key_loc);
        SCS_nores_out_26 = SCS_nores_out(:,:,key_loc);
        SCS_med_out_26 = SCS_med_out(:,:,key_loc);
        area_GCMs_26 = area_GCMs(:,:,key_loc);
    
        key_mask = strcmp(keys_check, 'all_rcp45');
        key_loc = find(key_mask);
        crop_med_45 = crop_med(:,:,key_loc);
        crop_GCMs_45 = crop_GCMs(:,:,key_loc);
        crop_res_45 = crop_res(:,:,key_loc);
        crop_cntry_45 = crop_cntry(:,:,key_loc);
        region_GCMs_45 = region_table_GCMs(:,:, key_loc);
        SCS_out_45 = SCS_out(:,:,key_loc);
        SCS_nores_out_45 = SCS_nores_out(:,:,key_loc);
        SCS_med_out_45 = SCS_med_out(:,:,key_loc);
        area_GCMs_45 = area_GCMs(:,:,key_loc);
    
        key_mask = strcmp(keys_check, 'all_rcp70');
        key_loc = find(key_mask);
        crop_med_70 = crop_med(:,:,key_loc);
        crop_GCMs_70 = crop_GCMs(:,:,key_loc);
        crop_res_70 = crop_res(:,:,key_loc);
        crop_cntry_70 = crop_cntry(:,:,key_loc);
        region_GCMs_70 = region_table_GCMs(:,:, key_loc);
        SCS_out_70 = SCS_out(:,:,key_loc);
        SCS_nores_out_70 = SCS_nores_out(:,:,key_loc);
        SCS_med_out_70 = SCS_med_out(:,:,key_loc);
        area_GCMs_70 = area_GCMs(:,:,key_loc);
    
        key_mask = strcmp(keys_check, 'all_rcp85');
        key_loc = find(key_mask);
        crop_med_85 = crop_med(:,:,key_loc);
        crop_GCMs_85 = crop_GCMs(:,:,key_loc);
        crop_res_85 = crop_res(:,:,key_loc);
        crop_cntry_85 = crop_cntry(:,:,key_loc);
        region_GCMs_85 = region_table_GCMs(:,:, key_loc);
        SCS_out_85 = SCS_out(:,:,key_loc);
        SCS_nores_out_85 = SCS_nores_out(:,:,key_loc);
        SCS_med_out_85 = SCS_med_out(:,:,key_loc);
        area_GCMs_85 = area_GCMs(:,:,key_loc);

        if strcmp(masking, "all")
            % If maksing = all, make total cropland and crop specific
            % cropland results

            key_mask = strcmp(keys_check, 'agg_rcp85_2070');
            key_loc = find(key_mask);
            SCS_out_total_85_2070 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_85_2070 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_tot_85_2070 = SCS_med_crop_specific(:,:,key_loc);
            SCS_out_crop_85_2070 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_85_2070 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_85_2070 = SCS_med_crop_specific(:,:,key_loc);
           
            key_mask = strcmp(keys_check, 'agg_rcp85_2050');
            key_loc = find(key_mask);
            SCS_out_total_85_2050 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_85_2050 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_tot_85_2050 = SCS_med_crop_specific(:,:,key_loc);
            SCS_out_crop_85_2050 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_85_2050 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_85_2050 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp85_2030');
            key_loc = find(key_mask);
            SCS_out_total_85_2030 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_85_2030 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_tot_85_2030 = SCS_med_crop_specific(:,:,key_loc);
            SCS_out_crop_85_2030 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_85_2030 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_85_2030 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp70_2070');
            key_loc = find(key_mask);
            SCS_out_total_70_2070 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_70_2070 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_tot_70_2070 = SCS_med_crop_specific(:,:,key_loc);
            SCS_out_crop_70_2070 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_70_2070 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_70_2070 = SCS_med_crop_specific(:,:,key_loc);
            
            key_mask = strcmp(keys_check, 'agg_rcp70_2050');
            key_loc = find(key_mask);
            SCS_out_total_70_2050 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_70_2050 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_tot_70_2050 = SCS_med_crop_specific(:,:,key_loc);
            SCS_out_crop_70_2050 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_70_2050 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_70_2050 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp70_2030');
            key_loc = find(key_mask);
            SCS_out_total_70_2030 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_70_2030 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_tot_70_2030 = SCS_med_crop_specific(:,:,key_loc);
            SCS_out_crop_70_2030 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_70_2030 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_70_2030 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp45_2070');
            key_loc = find(key_mask);
            SCS_out_total_45_2070 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_45_2070 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_tot_45_2070 = SCS_med_crop_specific(:,:,key_loc);
            SCS_out_crop_45_2070 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_45_2070 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_45_2070 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp45_2050');
            key_loc = find(key_mask);
            SCS_out_total_45_2050 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_45_2050 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_tot_45_2050 = SCS_med_crop_specific(:,:,key_loc);
            SCS_out_crop_45_2050 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_45_2050 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_45_2050 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp45_2030');
            key_loc = find(key_mask);
            SCS_out_total_45_2030 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_45_2030 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_tot_45_2030 = SCS_med_crop_specific(:,:,key_loc);
            SCS_out_crop_45_2030 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_45_2030 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_45_2030 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp26_2070');
            key_loc = find(key_mask);
            SCS_out_total_26_2070 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_26_2070 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_tot_26_2070 = SCS_med_crop_specific(:,:,key_loc);
            SCS_out_crop_26_2070 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_26_2070 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_26_2070 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp26_2050');
            key_loc = find(key_mask);
            SCS_out_total_26_2050 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_26_2050 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_tot_26_2050 = SCS_med_crop_specific(:,:,key_loc);
            SCS_out_crop_26_2050 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_26_2050 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_26_2050 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp26_2030');
            key_loc = find(key_mask);
            SCS_out_total_26_2030 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_26_2030 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_tot_26_2030 = SCS_med_crop_specific(:,:,key_loc);
            SCS_out_crop_26_2030 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_26_2030 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_26_2030 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'all_rcp26');
            key_loc = find(key_mask);
            SCS_out_total_26 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_26 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_tot_26 = SCS_med_crop_specific(:,:,key_loc);
            SCS_out_crop_26 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_26 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_26 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'all_rcp45');
            key_loc = find(key_mask);
            SCS_out_total_45 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_45 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_tot_45 = SCS_med_crop_specific(:,:,key_loc);
            SCS_out_crop_45 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_45 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_45 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'all_rcp70');
            key_loc = find(key_mask);
            SCS_out_total_70 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_70 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_tot_70 = SCS_med_crop_specific(:,:,key_loc);
            SCS_out_crop_70 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_70 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_70 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'all_rcp85');
            key_loc = find(key_mask);
            SCS_out_total_85 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_85 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_tot_85 = SCS_med_crop_specific(:,:,key_loc);
            SCS_out_crop_85 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_85 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_85 = SCS_med_crop_specific(:,:,key_loc);

        
        elseif strcmp(masking, "total")
            % If maksing = total cropland mask,
            % also make total cropland results

            key_mask = strcmp(keys_check, 'agg_rcp85_2070');
            key_loc = find(key_mask);
            SCS_out_total_85_2070 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_85_2070 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_85_2070 = SCS_med_crop_specific(:,:,key_loc);
           
            key_mask = strcmp(keys_check, 'agg_rcp85_2050');
            key_loc = find(key_mask);
            SCS_out_total_85_2050 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_85_2050 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_85_2050 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp85_2030');
            key_loc = find(key_mask);
            SCS_out_total_85_2030 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_85_2030 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_85_2030 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp70_2070');
            key_loc = find(key_mask);
            SCS_out_total_70_2070 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_70_2070 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_70_2070 = SCS_med_crop_specific(:,:,key_loc);
            
            key_mask = strcmp(keys_check, 'agg_rcp70_2050');
            key_loc = find(key_mask);
            SCS_out_total_70_2050 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_70_2050 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_70_2050 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp70_2030');
            key_loc = find(key_mask);
            SCS_out_total_70_2030 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_70_2030 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_70_2030 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp45_2070');
            key_loc = find(key_mask);
            SCS_out_total_45_2070 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_45_2070 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_45_2070 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp45_2050');
            key_loc = find(key_mask);
            SCS_out_total_45_2050 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_45_2050 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_45_2050 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp45_2030');
            key_loc = find(key_mask);
            SCS_out_total_45_2030 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_45_2030 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_45_2030 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp26_2070');
            key_loc = find(key_mask);
            SCS_out_total_26_2070 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_26_2070 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_26_2070 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp26_2050');
            key_loc = find(key_mask);
            SCS_out_total_26_2050 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_26_2050 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_26_2050 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp26_2030');
            key_loc = find(key_mask);
            SCS_out_total_26_2030 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_26_2030 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_26_2030 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'all_rcp26');
            key_loc = find(key_mask);
            SCS_out_total_26 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_26 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_26 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'all_rcp45');
            key_loc = find(key_mask);
            SCS_out_total_45 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_45 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_45 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'all_rcp70');
            key_loc = find(key_mask);
            SCS_out_total_70 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_70 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_70 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'all_rcp85');
            key_loc = find(key_mask);
            SCS_out_total_85 = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_85 = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_85 = SCS_med_crop_specific(:,:,key_loc);

        elseif strcmp(masking, "crop")
            % If maksing includes the crop specific cropland mask,
            % also make crop-specific cropland results

            key_mask = strcmp(keys_check, 'agg_rcp85_2070');
            key_loc = find(key_mask);
            SCS_out_crop_85_2070 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_85_2070 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_85_2070 = SCS_med_crop_specific(:,:,key_loc);
           
            key_mask = strcmp(keys_check, 'agg_rcp85_2050');
            key_loc = find(key_mask);
            SCS_out_crop_85_2050 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_85_2050 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_85_2050 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp85_2030');
            key_loc = find(key_mask);
            SCS_out_crop_85_2030 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_85_2030 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_85_2030 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp70_2070');
            key_loc = find(key_mask);
            SCS_out_crop_70_2070 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_70_2070 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_70_2070 = SCS_med_crop_specific(:,:,key_loc);
            
            key_mask = strcmp(keys_check, 'agg_rcp70_2050');
            key_loc = find(key_mask);
            SCS_out_crop_70_2050 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_70_2050 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_70_2050 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp70_2030');
            key_loc = find(key_mask);
            SCS_out_crop_70_2030 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_70_2030 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_70_2030 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp45_2070');
            key_loc = find(key_mask);
            SCS_out_crop_45_2070 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_45_2070 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_45_2070 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp45_2050');
            key_loc = find(key_mask);
            SCS_out_crop_45_2050 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_45_2050 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_45_2050 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp45_2030');
            key_loc = find(key_mask);
            SCS_out_crop_45_2030 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_45_2030 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_45_2030 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp26_2070');
            key_loc = find(key_mask);
            SCS_out_crop_26_2070 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_26_2070 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_26_2070 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp26_2050');
            key_loc = find(key_mask);
            SCS_out_crop_26_2050 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_26_2050 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_26_2050 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'agg_rcp26_2030');
            key_loc = find(key_mask);
            SCS_out_crop_26_2030 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_26_2030 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_26_2030 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'all_rcp26');
            key_loc = find(key_mask);
            SCS_out_crop_26 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_26 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_26 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'all_rcp45');
            key_loc = find(key_mask);
            SCS_out_crop_45 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_45 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_45 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'all_rcp70');
            key_loc = find(key_mask);
            SCS_out_crop_70 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_70 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_70 = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, 'all_rcp85');
            key_loc = find(key_mask);
            SCS_out_crop_85 = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_85 = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_85 = SCS_med_crop_specific(:,:,key_loc);

        end

    elseif strcmp(scenarios, "warming")
        key_mask = strcmp(keys_check, 'baseline');
        key_loc = find(key_mask);
        crop_med_baseline = crop_med(:,:,key_loc);
        crop_GCMs_baseline = crop_GCMs(:,:,key_loc);
        crop_res_baseline = crop_res(:,:,key_loc);
        crop_cntry_baseline = crop_cntry(:,:,key_loc);
        region_GCMs_baseline = region_table_GCMs(:,:, key_loc);
        SCS_out_baseline = SCS_out(:,:,key_loc);
        SCS_nores_out_baseline = SCS_nores_out(:,:,key_loc);
        SCS_med_baseline = SCS_med_out(:,:,key_loc);
        area_GCMs_baseline = area_GCMs(:,:,key_loc);
    

        key_mask = strcmp(keys_check, '15wp');
        key_loc = find(key_mask);
        crop_med_15wp = crop_med(:,:,key_loc);
        crop_GCMs_15wp = crop_GCMs(:,:,key_loc);
        crop_res_15wp = crop_res(:,:,key_loc);
        crop_cntry_15wp = crop_cntry(:,:,key_loc);
        region_GCMs_15wp = region_table_GCMs(:,:, key_loc);
        SCS_out_15wp = SCS_out(:,:,key_loc);
        SCS_nores_out_15wp = SCS_nores_out(:,:,key_loc);
        SCS_med_15wp = SCS_med_out(:,:,key_loc);
        area_GCMs_15wp = area_GCMs(:,:,key_loc);
    
        key_mask = strcmp(keys_check, '2wp');
        key_loc = find(key_mask);
        crop_med_2wp = crop_med(:,:,key_loc);
        crop_GCMs_2wp = crop_GCMs(:,:,key_loc);
        crop_res_2wp = crop_res(:,:,key_loc);
        crop_cntry_2wp = crop_cntry(:,:,key_loc);
        region_GCMs_2wp = region_table_GCMs(:,:, key_loc);
        SCS_out_2wp = SCS_out(:,:,key_loc);
        SCS_nores_out_2wp = SCS_nores_out(:,:,key_loc);
        SCS_med_2wp = SCS_med_out(:,:,key_loc);
        area_GCMs_2wp = area_GCMs(:,:,key_loc);
    
        key_mask = strcmp(keys_check, '3wp');
        key_loc = find(key_mask);
        crop_med_3wp = crop_med(:,:,key_loc);
        crop_GCMs_3wp = crop_GCMs(:,:,key_loc);
        crop_res_3wp = crop_res(:,:,key_loc);
        crop_cntry_3wp = crop_cntry(:,:,key_loc);
        region_GCMs_3wp = region_table_GCMs(:,:, key_loc);
        SCS_out_3wp = SCS_out(:,:,key_loc);
        SCS_nores_out_3wp = SCS_nores_out(:,:,key_loc);
        SCS_med_3wp = SCS_med_out(:,:,key_loc);
        area_GCMs_3wp = area_GCMs(:,:,key_loc);
    
        key_mask = strcmp(keys_check, '4wp');
        key_loc = find(key_mask);
        crop_med_4wp = crop_med(:,:,key_loc);
        crop_GCMs_4wp = crop_GCMs(:,:,key_loc);
        crop_res_4wp = crop_res(:,:,key_loc);
        crop_cntry_4wp = crop_cntry(:,:,key_loc);
        region_GCMs_4wp = region_table_GCMs(:,:, key_loc);
        SCS_out_4wp = SCS_out(:,:,key_loc);
        SCS_nores_out_4wp = SCS_nores_out(:,:,key_loc);
        SCS_med_4wp = SCS_med_out(:,:,key_loc);
        area_GCMs_4wp = area_GCMs(:,:,key_loc);

        key_mask = strcmp(keys_check, '5wp');
        key_loc = find(key_mask);
        crop_med_5wp = crop_med(:,:,key_loc);
        crop_GCMs_5wp = crop_GCMs(:,:,key_loc);
        crop_res_5wp = crop_res(:,:,key_loc);
        crop_cntry_5wp = crop_cntry(:,:,key_loc);
        region_GCMs_5wp = region_table_GCMs(:,:, key_loc);
        SCS_out_5wp = SCS_out(:,:,key_loc);
        SCS_nores_out_5wp = SCS_nores_out(:,:,key_loc);
        SCS_med_5wp = SCS_med_out(:,:,key_loc);
        area_GCMs_5wp = area_GCMs(:,:,key_loc);

        if strcmp(masking, "total")
            % If maksing includes the total cropland mask,
            % also make total cropland results

            key_mask = strcmp(keys_check, 'baseline');
            key_loc = find(key_mask);
            SCS_out_total_baseline = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_baseline = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_baseline = SCS_med_total_cropland(:,:,key_loc);
        
            key_mask = strcmp(keys_check, '15wp');
            key_loc = find(key_mask);
            SCS_out_total_15wp = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_15wp = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_15wp = SCS_med_total_cropland(:,:,key_loc);
        
            key_mask = strcmp(keys_check, '2wp');
            key_loc = find(key_mask);
            SCS_out_total_2wp = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_2wp = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_2wp = SCS_med_total_cropland(:,:,key_loc);
        
            key_mask = strcmp(keys_check, '3wp');
            key_loc = find(key_mask);
            SCS_out_total_3wp = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_3wp = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_3wp = SCS_med_total_cropland(:,:,key_loc);
        
            key_mask = strcmp(keys_check, '4wp');
            key_loc = find(key_mask);
            SCS_out_total_4wp = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_4wp = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_4wp = SCS_med_total_cropland(:,:,key_loc);

            key_mask = strcmp(keys_check, '5wp');
            key_loc = find(key_mask);
            SCS_out_total_5wp = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_5wp = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_5wp = SCS_med_total_cropland(:,:,key_loc);

        elseif strcmp(masking, "crop")
            % If maksing includes the total cropland mask,
            % also make crop-specific cropland results
            key_mask = strcmp(keys_check, 'baseline');
            key_loc = find(key_mask);
            SCS_out_crop_baseline = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_baseline = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_baseline = SCS_med_crop_specific(:,:,key_loc);

            key_mask = strcmp(keys_check, '15wp');
            key_loc = find(key_mask);
            SCS_out_crop_15wp = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_15wp = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_15wp = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, '2wp');
            key_loc = find(key_mask);
            SCS_out_crop_2wp = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_2wp = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_2wp = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, '3wp');
            key_loc = find(key_mask);
            SCS_out_crop_3wp = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_3wp = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_3wp = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, '4wp');
            key_loc = find(key_mask);
            SCS_out_crop_4wp = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_4wp = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_4wp = SCS_med_crop_specific(:,:,key_loc);

            key_mask = strcmp(keys_check, '5wp');
            key_loc = find(key_mask);
            SCS_out_crop_5wp = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_5wp = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_5wp = SCS_med_crop_specific(:,:,key_loc);

        elseif strcmp(masking, "all")
            % if masking = all, make results for total cropland and
            % crop specific cropland
            key_mask = strcmp(keys_check, 'baseline');
            key_loc = find(key_mask);
            SCS_out_total_baseline = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_baseline = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_baseline = SCS_med_total_cropland(:,:,key_loc);
            SCS_out_crop_baseline = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_baseline = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_baseline = SCS_med_crop_specific(:,:,key_loc);

            key_mask = strcmp(keys_check, '15wp');
            key_loc = find(key_mask);
            SCS_out_total_15wp = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_15wp = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_15wp = SCS_med_total_cropland(:,:,key_loc);
            SCS_out_crop_15wp = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_15wp = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_15wp = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, '2wp');
            key_loc = find(key_mask);
            SCS_out_total_2wp = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_2wp = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_2wp = SCS_med_total_cropland(:,:,key_loc);
            SCS_out_crop_2wp = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_2wp = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_2wp = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, '3wp');
            key_loc = find(key_mask);
            SCS_out_total_3wp = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_3wp = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_3wp = SCS_med_total_cropland(:,:,key_loc);
            SCS_out_crop_3wp = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_3wp = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_3wp = SCS_med_crop_specific(:,:,key_loc);
        
            key_mask = strcmp(keys_check, '4wp');
            key_loc = find(key_mask);
            SCS_out_total_4wp = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_4wp = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_4wp = SCS_med_total_cropland(:,:,key_loc);
            SCS_out_crop_4wp = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_4wp = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_4wp = SCS_med_crop_specific(:,:,key_loc);

            key_mask = strcmp(keys_check, '5wp');
            key_loc = find(key_mask);
            SCS_out_total_5wp = SCS_total_cropland(:,:,key_loc);
            SCS_nores_out_total_5wp = SCS_nores_total_cropland(:,:,key_loc);
            SCS_med_total_5wp = SCS_med_total_cropland(:,:,key_loc);
            SCS_out_crop_5wp = SCS_crop_specific(:,:,key_loc);
            SCS_nores_out_crop_5wp = SCS_nores_crop_specific(:,:,key_loc);
            SCS_med_crop_5wp = SCS_med_crop_specific(:,:,key_loc);


        end
    end

    %save(main_save_file, 'SCS*', 'crop*', 'keys_check')
    save(div_save_file, 'SCS_nores_out_total*', 'crop_med*', 'crop_GCMs*', ...
        'region_GCMs_*','area_GCMs_*')
    clear crop* SCS* key* 


    % Resilience results
    if res_yes
        %get res tables and save under new variable names
        load(tabl_folder, '-mat')
    
        key_mask = strcmp(keys_check_2, 'res_rcp26');
        key_loc = key_mask;
        tbl_hold_res_crop_2090_26 = tbl_hold_res_crop_2090(:,:,key_loc);
    
        key_mask = strcmp(keys_check_2, 'res_rcp45');
        key_loc = find(key_mask);
        tbl_hold_res_crop_2090_45 = tbl_hold_res_crop_2090(:,:,key_loc);
    
        key_mask = strcmp(keys_check_2, 'res_rcp70');
        key_loc = find(key_mask);
        tbl_hold_res_crop_2090_70 = tbl_hold_res_crop_2090(:,:,key_loc);
    
        key_mask = strcmp(keys_check_2, 'res_rcp85');
        key_loc = find(key_mask);
        tbl_hold_res_crop_2090_85 = tbl_hold_res_crop_2090(:,:,key_loc);
    
        key_mask = strcmp(keys_check_2, 'sens_rcp26');
        key_loc = find(key_mask);
        tbl_hold_res_crop_2090_26_sens = tbl_hold_res_crop_2090(:,:,key_loc);
    
        key_mask = strcmp(keys_check_2, 'sens_rcp45');
        key_loc = find(key_mask);
        tbl_hold_res_crop_2090_45_sens = tbl_hold_res_crop_2090(:,:,key_loc);
    
        key_mask = strcmp(keys_check_2, 'sens_rcp70');
        key_loc = find(key_mask);
        tbl_hold_res_crop_2090_70_sens = tbl_hold_res_crop_2090(:,:,key_loc);
    
        key_mask = strcmp(keys_check_2, 'sens_rcp85');
        key_loc = find(key_mask);
        tbl_hold_res_crop_2090_85_sens = tbl_hold_res_crop_2090(:,:,key_loc);
    
        save(tabl_save_file, 'tbl*', 'keys_check_2')
        clear tbl* key* 
    end
end