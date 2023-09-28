function step6_holdridge_analyses_main(env, spamcrop_name, res_yes, scenarios, spam_year)
%% Set working directory  
    if strcmp(env, 'triton')
        % set the triton work directory as working directory
        working_dir = '/scratch/work/heikons1/scs';
    else
        % set the folder where .m file is as a working directory
        working_dir = fileparts(matlab.desktop.editor.getActiveFilename);
        
    end
    cd(working_dir)

    % Add paths to search path:
        addpath('functions');
        addpath('ext_functions/wprctile');
        addpath('ext_functions/crameri')
        addpath('ext_functions/cbarrow')
        addpath('ext_functions/export_fig')
    
    %% Create folders for outputs
    
    results_folder = fullfile(working_dir,'results_review');
    ternary_folder = fullfile(working_dir,'results_review','ternary_mapping');
    tables_folder = fullfile(working_dir,'results_review','matlab');
    
    if exist(results_folder, 'dir') ~= 7
        mkdir( results_folder );
    end
    
    if exist(ternary_folder, 'dir') ~= 7
        mkdir( ternary_folder );
    end
    
    if exist(tables_folder, 'dir') ~= 7
        mkdir( tables_folder );
    end
    
    % make subforders for the crop    
    crop_folder_results = fullfile(working_dir, 'results_review', spamcrop_name);
    if exist(crop_folder_results, 'dir') ~= 7
        mkdir(crop_folder_results)
    end

    crop_folder_tern = fullfile(working_dir, 'results_review','ternary_mapping', spamcrop_name);
    if exist(crop_folder_tern, 'dir') ~= 7
        mkdir(crop_folder_tern)
    end

    crop_folder_tabl = fullfile(working_dir, 'results_review', 'matlab', spamcrop_name);
    if exist(crop_folder_tabl, 'dir') ~= 7
        mkdir(crop_folder_tabl)
    end
    
    
    %% Preparation steps: loading input data and initializing parallel pool

    % Land data
    data = load('holdridge_data/hLand.mat');
    hLand = data.hLand;
    %size(hLand)
    
    % Resilience data:
    res_time = ncread('ref_data/resilience.nc', 'time');
    res_all = ncread('ref_data/resilience.nc', 'resilience');
    res_m = res_all(:,:,res_time == 2010);
    res_m(res_m == -9) = NaN;
    res_m = transpose(res_m);
    res_m(~hLand) = NaN;
    %size(res_m)

    % Reference data (food crop production):
    spamcrop_file_name = strcat('/',spamcrop_name,'_cropcal.tif')
    [ref_m, R_crop] = readgeoraster(strcat('spam', spam_year, spamcrop_file_name));
    
    % zero-pad 2005 crop production raster to make extent match with other
    % data
    if strcmp(spam_year, '2005')
        ref_m = [ref_m; zeros(307,4320)];
    end

    size(ref_m)

    % Total cropland
    [total_cropland_mask, R_total] = readgeoraster(strcat('ref_data/total_cropland_mask_', spam_year, '_soy.tif'));

    % Change no data or no land cells to zero
    ref_m(ref_m < 0) = 0;
    ref_m(ref_m == -1) = 0;
    ref_m(isnan(ref_m)) = 0;
    ref_m(hLand == 0) = 0;
    %size(ref_m)

    % Country id raster
    [cntry_m, R_cntry] = readgeoraster('ref_data/cntry_raster_fao_id_plus.tif');
    cntry_m(~hLand) = 0;
    
    % Regional id raster
    [region_m, R_region] = readgeoraster('ref_data/region_id.tif', 'OutputType','int16');
    region_m(~hLand) = 0;

    %%
    if strcmp(env, 'triton')
        % Initialize the parallel pool
        c=parcluster();
        
        % Create a temporary folder for the workers working on this job,
        % in order not to conflict with other jobs.
        t=tempname();
        mkdir(t);
        
        % set the worker storage location of the cluster
        c.JobStorageLocation=t;
    
        % get the number of workers based on the available CPUS from SLURM
        num_workers = str2double(getenv('SLURM_CPUS_PER_TASK'));
    
        % start the parallel pool
        parpool(c,num_workers);
    else
        p = gcp('nocreate');
        if isempty(p)
           parpool('local');
        end
    end

    %% SCS main outputs:

    if strcmp(scenarios, "all")
        % all SSPs and years
        input_map = containers.Map({'all_rcp26', 'all_rcp45', 'all_rcp70', 'all_rcp85', 'agg_rcp26_2030', ...
           'agg_rcp26_2050', 'agg_rcp26_2070', 'agg_rcp45_2030', 'agg_rcp45_2050', 'agg_rcp45_2070', ...
           'agg_rcp70_2030', 'agg_rcp70_2050', 'agg_rcp70_2070',...
           'agg_rcp85_2030', 'agg_rcp85_2050', 'agg_rcp85_2070'}, ...
           {{'hold_2090', 'rcp26', true, true, true, 'all'}, {'hold_2090', 'rcp45', true, true, true,'all'}, ...
           {'hold_2090', 'rcp70', true, true, true,'all'},  {'hold_2090', 'rcp85', true, true, true,'all'},...
           {'hold_2030', 'rcp26', true, false, false,'all'},...
           {'hold_2050', 'rcp26', true, false, false,'all'}, {'hold_2070', 'rcp26', true, false, false,'all'},...
           {'hold_2030', 'rcp45', true, false, false,'all'}, {'hold_2050', 'rcp45', true, false, false,'all'}, ...
           {'hold_2070', 'rcp45', true, false, false,'all'}, {'hold_2030', 'rcp70', true, false, false,'all'}, ...
           {'hold_2050', 'rcp70', true, false, false,'all'}, {'hold_2070', 'rcp70', true, false, false,'all'} ...
           {'hold_2030', 'rcp85', true, false, false,'all'}, ...
           {'hold_2050', 'rcp85', true, false, false,'all'}, {'hold_2070', 'rcp85', true, false, false,'all'}});

    elseif strcmp(scenarios, "warming")
        % only results that correspond to baseline climate and specific warming potentials: 1.5,
        % 2, 3, 4, 5 C warming
        input_map = containers.Map({'baseline' '15wp', '2wp', '3wp', '4wp', '5wp'}, ...
           {{'current', '', true, true, true, 'all'}, {'hold_2030', 'rcp26', true, true, true, 'all'}, {'hold_2090', 'rcp26', true, true, true,'all'}, ...
           {'hold_2090', 'rcp45', true, true, true,'all'},  {'hold_2090', 'rcp70', true, true, true,'all'},...
           {'hold_2090', 'rcp85', true, true, true,'all'}});
    end

%% shorter input maps for testing 
    %input_map = containers.Map({'all_rcp85', 'agg_rcp85_2030'},{{'hold_2090', 'rcp85', true, true, true,'all'}, {'hold_2030', 'rcp85', true, false, false,'all'}});
    %input_map = containers.Map({'baseline'},{{'current', '', true, true, true,'all'}});
    %input_map = containers.Map({'baseline','3wp'},{{'current', '', true, true, true,'all'},...
    %    {'hold_2090', 'rcp45', true, true, true,'all'}});

%%
    % initialize output arrays and array storing related map keys
    crop_med = zeros(2, 2, length(keys(input_map)));
    crop_GCMs = zeros(8, 2, length(keys(input_map)));
    crop_res = zeros(8, 2, length(keys(input_map)));
    crop_cntry = zeros(178, 4, length(keys(input_map)));
    region_table_GCMs = zeros(length(unique(region_m)), 10, length(keys(input_map)));
    SCS_med_out = zeros(2160,4320, length(keys(input_map)));
    SCS_med_total_cropland = zeros(2160,4320, length(keys(input_map)));
    SCS_med_crop_specific = zeros(2160,4320, length(keys(input_map)));
    SCS_out = zeros(2160,4320, length(keys(input_map)));
    SCS_nores_out = zeros(2160,4320, length(keys(input_map)));
    SCS_total_cropland = zeros(2160,4320, length(keys(input_map)));
    SCS_crop_specific = zeros(2160,4320, length(keys(input_map)));
    SCS_nores_total_cropland = zeros(2160,4320, length(keys(input_map)));
    SCS_nores_crop_specific = zeros(2160,4320, length(keys(input_map)));
    area_GCMs = zeros(8,5,length(keys(input_map)));
    keys_check = cell(1,length(keys(input_map)));

  %%
    tic

    parfor k= 1:(length(keys(input_map)))
    %for k = 1:length(keys(input_map))

        keycell = keys(input_map);
        args = input_map(keycell{k});

        % oikea funktiokutsu
        [crop_med(:,:,k), crop_GCMs(:,:,k), crop_res(:,:,k), crop_cntry(:,:,k),...
            SCS_out(:,:,k), SCS_nores_out(:,:,k), SCS_total_cropland(:,:,k),....
            SCS_crop_specific(:,:,k), SCS_nores_total_cropland(:,:,k), SCS_nores_crop_specific(:,:,k),....
            SCS_med_out(:,:,k), SCS_med_total_cropland(:,:,k), SCS_med_crop_specific(:,:,k),...
            region_table_GCMs(:,:,k), area_GCMs(:,:,k)] = f_holdridge_vs_ref_boxed(working_dir,...
            spamcrop_name, 'crop_spam', ref_m, total_cropland_mask, res_m, ...
            hLand, cntry_m, region_m, R_region, args{1}, args{2}, spam_year, args{3}, ...
            args{4}, args{5}, args{6});

        keys_check{1,k} = keycell{k};

        % printing a checkpoint for handled scenario
        keycell{k}
        
    end
    
    cd(working_dir)
 

    % tallennus, kaikki crop taulukkomuuttujat ja keys_check ..._test.mat
    % tiedostoon
    save_filename = strcat('results_review/matlab/', spamcrop_name, '/main_results', spamcrop_name, spam_year, '.mat');
    save(save_filename, 'crop*', 'keys_check', 'SCS_*', 'region_*', 'area_GCMs')
  
    toc

    %clearvars -except working_dir spamcrop_name
    
    %% Analyses about how resilience and holdridge percentiles relate to food crop production
    
    if res_yes == true
        % map false and colorbar false, can be done on laptop later
        input_map = containers.Map({'res_rcp26', 'res_rcp45', 'res_rcp70', 'res_rcp85', ...
            'sens_rcp26', 'sens_rcp45', 'sens_rcp70', 'sens_rcp85'}, ...
            {{'rcp26', 4, 4, true, true}, {'rcp45', 4, 4, false, false}, {'rcp70', 4, 4, false, false}, ...
            {'rcp85', 4, 4, false, false}, {'rcp26',  4, [0, 20, 25, 30, 100], false, false}, ...
            {'rcp85',  4, [0, 20, 25, 30, 100], false, false},{'rcp70',  4, [0, 20, 25, 30, 100], false, false},...
            {'rcp85',  4, [0, 20, 25, 30, 100], false, false}});
    
        % initialize output array
        tbl_hold_res_crop_2090 = zeros(8, 4, length(keys(input_map)));
        keys_check_2 = cell(1,length(keys(input_map)));
    
        tic
        parfor j= 1:length(keys(input_map))
        %for j = 1:length(keys(input_map))
    
            keycell = keys(input_map);
            args = input_map(keycell{j});
    
            % oikea funktiokutsu
            tbl_hold_res_crop_2090(:,:,j) = f_map_and_table_for_resilience_vs_holdridge_vs_ref(working_dir, spamcrop_name,'crop_spam', ref_m, res_m, hLand, 'hold_2090', args{1}, args{2}, args{3}, args{4}, args{5});
            keys_check_2{1,j} = keycell{j};
    
        end

        toc
        cd(working_dir)
        % tallennus, kaikki crop taulukkomuuttujat ja keys_check ..._test.mat
        % tiedostoon
        save_filename = strcat('results_review/matlab/', spamcrop_name, '/tbl_hold_res', spamcrop_name, spam_year, '.mat');
        save(save_filename, 'tbl*', 'keys_check_2');

        clearvars -except working_dir

    end

    % close parallel pool
    delete(gcp('nocreate'))

end
