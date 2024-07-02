clear; clc

% set the folder where .m file is as a working directory
working_dir = fileparts(matlab.desktop.editor.getActiveFilename);
cd(working_dir)

% what is the masking type used in step 6?
masking = "all"; % "all" (all masks and no mask), "crop" (no mask and crop-specific), or "total" (no mask and total cropland)

% what is the results folder name?
matlab_folder = 'results_260624_2020_cropcal/matlab/';

% SPAM data year, as text
spam_year = '2020';

% For supplementary analysis: has 2020 analysis been done with 2010/2005
% crop types?
comb_2020 = false;

% Analysis only for supplementary analysis with seasonal SCS?
cropcal = true;

%% Reorganizing triton results for each crop, do not modify from here on

if (cropcal)
    spamcrop_list = {'MAIZ', 'SOYB'};

elseif (strcmp(spam_year, '2020') & ~comb_2020)
    spamcrop_list = {'WHEA','RICE','MAIZ','BARL','MILL','PMIL','SORG','OCER','POTA','SWPO',...
        'YAMS','CASS','ORTS','BEAN','CHIC','COWP','PIGE','LENT','OPUL',...
        'SOYB','GROU','CNUT','BANA','PLNT','CITR','TROF','TEMF','TOMA',...
        'ONIO','VEGE','OILP','SUNF','RAPE','SESA','OOIL','SUGC','SUGB',...
        'COTT','OFIB','COFF','RCOF','COCO','TEAS','TOBA','RUBB','REST'};

elseif (strcmp(spam_year, '2020') & comb_2020)
    spamcrop_list = {'WHEA','RICE','MAIZ','BARL','MILL','PMIL','SORG','OCER','POTA','SWPO',...
    'YAMS','CASS','ORTS','BEAN','CHIC','COWP','PIGE','LENT','OPUL',...
    'SOYB','GROU','CNUT','BANA','PLNT','TROF_COMB','TEMF',...
    'VEGE_COMB','OILP','SUNF','RAPE','SESA','OOIL','SUGC','SUGB',...
    'COTT','OFIB','COFF','RCOF','COCO','TEAS','TOBA','REST_COMB'};

else
    spamcrop_list = {'WHEA','RICE','MAIZ','BARL','PMIL','SMIL','SORG','OCER',...
        'POTA','SWPO','YAMS','CASS','ORTS','BEAN','CHIC','COWP','PIGE','LENT',...
        'OPUL','SOYB','GROU','CNUT','BANA','PLNT','TROF','TEMF','VEGE','OILP',...
        'SUNF','RAPE','SESA','OOIL','SUGC','SUGB','COTT','OFIB','ACOF','RCOF',...
        'COCO','TEAS','TOBA','REST'};
end

num_crops = numel(spamcrop_list);
%%
for spamcrop = 1:num_crops
   
    main_folder = strcat(matlab_folder, spamcrop_list{spamcrop}, '/main_results', spamcrop_list{spamcrop}, spam_year,'.mat');
    main_save_file = strcat(matlab_folder, spamcrop_list{spamcrop}, '/main_results_re', spamcrop_list{spamcrop}, spam_year,'.mat');
    div_save_file = strcat(matlab_folder, spamcrop_list{spamcrop}, '/main_results_div', spamcrop_list{spamcrop}, spam_year,'.mat');

    % Main results
    % get main result files for the crop and save under new names
    load(main_folder, '-mat')


   
    key_mask = strcmp(keys_check, 'baseline');
    key_loc = find(key_mask);
    crop_med_baseline = crop_med(:,:,key_loc);
    crop_GCMs_baseline = crop_GCMs(:,:,key_loc);
    crop_agg_baseline = crop_agg(:,:,key_loc);
    crop_cntry_baseline = crop_cntry(:,:,key_loc);
    region_GCMs_baseline = region_table_GCMs(:,:, key_loc);
    SCS_out_baseline = SCS_out(:,:,key_loc);
    SCS_med_baseline = SCS_med_out(:,:,key_loc);
    area_GCMs_baseline = area_GCMs(:,:,key_loc);


    key_mask = strcmp(keys_check, '15wp');
    key_loc = find(key_mask);
    crop_med_15wp = crop_med(:,:,key_loc);
    crop_GCMs_15wp = crop_GCMs(:,:,key_loc);
    crop_agg_15wp = crop_agg(:,:,key_loc);
    crop_cntry_15wp = crop_cntry(:,:,key_loc);
    region_GCMs_15wp = region_table_GCMs(:,:, key_loc);
    SCS_out_15wp = SCS_out(:,:,key_loc);
    SCS_med_15wp = SCS_med_out(:,:,key_loc);
    area_GCMs_15wp = area_GCMs(:,:,key_loc);

    key_mask = strcmp(keys_check, '2wp');
    key_loc = find(key_mask);
    crop_med_2wp = crop_med(:,:,key_loc);
    crop_GCMs_2wp = crop_GCMs(:,:,key_loc);
    crop_agg_2wp = crop_agg(:,:,key_loc);
    crop_cntry_2wp = crop_cntry(:,:,key_loc);
    region_GCMs_2wp = region_table_GCMs(:,:, key_loc);
    SCS_out_2wp = SCS_out(:,:,key_loc);
    SCS_med_2wp = SCS_med_out(:,:,key_loc);
    area_GCMs_2wp = area_GCMs(:,:,key_loc);

    key_mask = strcmp(keys_check, '3wp');
    key_loc = find(key_mask);
    crop_med_3wp = crop_med(:,:,key_loc);
    crop_GCMs_3wp = crop_GCMs(:,:,key_loc);
    crop_agg_3wp = crop_agg(:,:,key_loc);
    crop_cntry_3wp = crop_cntry(:,:,key_loc);
    region_GCMs_3wp = region_table_GCMs(:,:, key_loc);
    SCS_out_3wp = SCS_out(:,:,key_loc);
    SCS_med_3wp = SCS_med_out(:,:,key_loc);
    area_GCMs_3wp = area_GCMs(:,:,key_loc);

    key_mask = strcmp(keys_check, '4wp');
    key_loc = find(key_mask);
    crop_med_4wp = crop_med(:,:,key_loc);
    crop_GCMs_4wp = crop_GCMs(:,:,key_loc);
    crop_agg_4wp = crop_agg(:,:,key_loc);
    crop_cntry_4wp = crop_cntry(:,:,key_loc);
    region_GCMs_4wp = region_table_GCMs(:,:, key_loc);
    SCS_out_4wp = SCS_out(:,:,key_loc);
    SCS_med_4wp = SCS_med_out(:,:,key_loc);
    area_GCMs_4wp = area_GCMs(:,:,key_loc);

    if strcmp(masking, "total")
        % If maksing includes the total cropland mask,
        % also make total cropland results

        key_mask = strcmp(keys_check, 'baseline');
        key_loc = find(key_mask);
        SCS_out_total_baseline = SCS_total_cropland(:,:,key_loc);
        SCS_med_total_baseline = SCS_med_total_cropland(:,:,key_loc);
    
        key_mask = strcmp(keys_check, '15wp');
        key_loc = find(key_mask);
        SCS_out_total_15wp = SCS_total_cropland(:,:,key_loc);
        SCS_med_total_15wp = SCS_med_total_cropland(:,:,key_loc);
    
        key_mask = strcmp(keys_check, '2wp');
        key_loc = find(key_mask);
        SCS_out_total_2wp = SCS_total_cropland(:,:,key_loc);
        SCS_med_total_2wp = SCS_med_total_cropland(:,:,key_loc);
    
        key_mask = strcmp(keys_check, '3wp');
        key_loc = find(key_mask);
        SCS_out_total_3wp = SCS_total_cropland(:,:,key_loc);
        SCS_med_total_3wp = SCS_med_total_cropland(:,:,key_loc);
    
        key_mask = strcmp(keys_check, '4wp');
        key_loc = find(key_mask);
        SCS_out_total_4wp = SCS_total_cropland(:,:,key_loc);
        SCS_med_total_4wp = SCS_med_total_cropland(:,:,key_loc);

    elseif strcmp(masking, "crop")
        % If maksing includes the total cropland mask,
        % also make crop-specific cropland results
        key_mask = strcmp(keys_check, 'baseline');
        key_loc = find(key_mask);
        SCS_out_crop_baseline = SCS_crop_specific(:,:,key_loc);
        SCS_med_crop_baseline = SCS_med_crop_specific(:,:,key_loc);

        key_mask = strcmp(keys_check, '15wp');
        key_loc = find(key_mask);
        SCS_out_crop_15wp = SCS_crop_specific(:,:,key_loc);
        SCS_med_crop_15wp = SCS_med_crop_specific(:,:,key_loc);
    
        key_mask = strcmp(keys_check, '2wp');
        key_loc = find(key_mask);
        SCS_out_crop_2wp = SCS_crop_specific(:,:,key_loc);
        SCS_med_crop_2wp = SCS_med_crop_specific(:,:,key_loc);
    
        key_mask = strcmp(keys_check, '3wp');
        key_loc = find(key_mask);
        SCS_out_crop_3wp = SCS_crop_specific(:,:,key_loc);
        SCS_med_crop_3wp = SCS_med_crop_specific(:,:,key_loc);
    
        key_mask = strcmp(keys_check, '4wp');
        key_loc = find(key_mask);
        SCS_out_crop_4wp = SCS_crop_specific(:,:,key_loc);
        SCS_med_crop_4wp = SCS_med_crop_specific(:,:,key_loc);

    elseif strcmp(masking, "all")
        % if masking = all, make results for total cropland and
        % crop specific cropland
        key_mask = strcmp(keys_check, 'baseline');
        key_loc = find(key_mask);
        SCS_out_total_baseline = SCS_total_cropland(:,:,key_loc);
        SCS_med_total_baseline = SCS_med_total_cropland(:,:,key_loc);
        SCS_out_crop_baseline = SCS_crop_specific(:,:,key_loc);
        SCS_med_crop_baseline = SCS_med_crop_specific(:,:,key_loc);

        key_mask = strcmp(keys_check, '15wp');
        key_loc = find(key_mask);
        SCS_out_total_15wp = SCS_total_cropland(:,:,key_loc);
        SCS_med_total_15wp = SCS_med_total_cropland(:,:,key_loc);
        SCS_out_crop_15wp = SCS_crop_specific(:,:,key_loc);
        SCS_med_crop_15wp = SCS_med_crop_specific(:,:,key_loc);
    
        key_mask = strcmp(keys_check, '2wp');
        key_loc = find(key_mask);
        SCS_out_total_2wp = SCS_total_cropland(:,:,key_loc);
        SCS_med_total_2wp = SCS_med_total_cropland(:,:,key_loc);
        SCS_out_crop_2wp = SCS_crop_specific(:,:,key_loc);
        SCS_med_crop_2wp = SCS_med_crop_specific(:,:,key_loc);
    
        key_mask = strcmp(keys_check, '3wp');
        key_loc = find(key_mask);
        SCS_out_total_3wp = SCS_total_cropland(:,:,key_loc);
        SCS_med_total_3wp = SCS_med_total_cropland(:,:,key_loc);
        SCS_out_crop_3wp = SCS_crop_specific(:,:,key_loc);
        SCS_med_crop_3wp = SCS_med_crop_specific(:,:,key_loc);
    
        key_mask = strcmp(keys_check, '4wp');
        key_loc = find(key_mask);
        SCS_out_total_4wp = SCS_total_cropland(:,:,key_loc);
        SCS_med_total_4wp = SCS_med_total_cropland(:,:,key_loc);
        SCS_out_crop_4wp = SCS_crop_specific(:,:,key_loc);
        SCS_med_crop_4wp = SCS_med_crop_specific(:,:,key_loc);

    end
   

    %save(main_save_file, 'SCS*', 'crop*', 'keys_check')
    save(div_save_file, 'SCS_out_total*', 'crop_med*', 'crop_GCMs*', ...
        'crop_agg*', 'region_GCMs_*','area_GCMs_*')
    clear crop* SCS* key* 
    
end