
%% holdridge - define classes
clear
close all
clc

% set the folder where .m file is as a working directory
folder_wd = fileparts(matlab.desktop.editor.getActiveFilename);
cd(folder_wd)

%% Initialise run

% version of the run
s_version = '1';

% holdridge data folder
folder_holdridge = 'holdridge_data';

%% DO NOT MODIFY FROM HERE ON

% matching warming levels, ssps and years
% ssp key: ssp2.6 = 1, ssp4.5 = 2, ssp7.0 = 3, ssp8.5 = 4
ssp_year_map = containers.Map({'15wp', '2wp', '3wp', '4wp'}, ...
           {{'2030', '2050', 1, 2031, 2051, 2033}, ...
           {'2050', '2070', 2, 2051, 2071, 2053}, ...
           {'2070', '2090', 3, 2071, 2091, 2076}, ...
           {'2070', '2090', 4, 2071, 2091, 2085}});

warming_levels = keys(ssp_year_map);
%%
for level = 1:(length(warming_levels))
 %%   
    warming_level = warming_levels{level};
    args = ssp_year_map(warming_levels{level});

    year_1st_str = args{1};
    year_2nd_str = args{2};
    ssp = args{3};
    year_1st = args{4};
    year_2nd = args{5};
    year_ipcc = args{6};

    %% folder paths
    
    % folder of the output
    folder_results = fullfile(folder_wd,folder_holdridge);
    
    % add paths of all needed functions 
    folder_functions = fullfile(folder_wd,'functions');
    addpath(genpath(folder_functions));
    
    %% define input and out data strings
    
    % date of running the code
    date = datestr(now, 'yyyymmdd');
    
    % saved data from step1
    file_step1_1 = strcat(folder_results, '/holdridge_data_year', year_1st_str, '_', s_version, '.mat');
    file_step1_2 = strcat(folder_results, '/holdridge_data_year', year_2nd_str, '_', s_version, '.mat');

    % output data
    file_resultsPresent = strcat(folder_results, '/holdridge_results_present_', s_version, '.mat'); 
    file_resultsFuture = strcat(folder_results, '/holdridge_results_', warming_level, '_', s_version, '.mat'); 
    
    
    %% load data from step 1

    % the present climate data and the land mask
    % are the same in all the future year .mat files, so we can load them 
    % from the first future timestep file only
    data_holdridge_present = load(file_step1_1).data_holdridge_present;
    hLand = load(file_step1_1).hLand;
    
    % for the future data, only select the ssp of interest
    data_holdridge_future_1st = squeeze(load(file_step1_1).data_holdridge_future(:,:,:,ssp,:));
    data_holdridge_future_2nd = squeeze(load(file_step1_2).data_holdridge_future(:,:,:,ssp,:));

    %% present

    %% data to vectors
    
    hIndex = find(hLand);
    %holdridge_index = holdridge_index(holdridge_land);
    
    v_data_hold_present = zeros(size(hIndex,1),size(data_holdridge_present,3),'single');
    
    for i = 1:size(data_holdridge_present,3)
        temp_hold = data_holdridge_present(:,:,i);
        v_data_hold_present(:,i) = temp_hold(hIndex);
        clearvars temp_*
    end
    clearvars i temp_*
    
    
    %% data in cartesian coordinates
    % based on holdridge_coordinates.xlsx
    
    % min precip, min PETratio
    % max precip, max PETratio
    
    hold_class_bound = [62.5 0.125; 16000 32];
    
    % present
    [temp_data_out] = f_holdridge_cartesian_coord(hold_class_bound,v_data_hold_present);
    
    v_data_hold_present(:,6:7) = temp_data_out;
    
    clearvars temp*
    
    %% holdridge classification
    % id without frost
    hold_class_centroids = xlsread(...
        'input/holdridge_coordinates.xlsx','A10:A45');
    % id with frost
    hold_class_centroids(:,2) = xlsread(...
        'input/holdridge_coordinates.xlsx','B10:B45');
    % centroids
    hold_class_centroids(:,3:4) = xlsread(...
        'input/holdridge_coordinates.xlsx','M10:N45');
    
    [temp_data_out] = f_holdridge_classification(hold_class_centroids,v_data_hold_present);
    
    v_data_hold_present(:,9:10) = temp_data_out;
    clearvars temp*
    
    % area for each class & holdridge classes
    
    area_5arcmin = areacell(5/60);
    v_area_5arcmin = area_5arcmin(hIndex);
    clearvars area_5arcmin;
    
    hold_classes = accumarray(int32(v_data_hold_present(:,10)),int32(v_data_hold_present(:,10)),[],@nanmean);
    hold_classes(:,2) = accumarray(int32(v_data_hold_present(:,10)),v_area_5arcmin,[],@nansum);
    hold_classes(:,3) = hold_classes(:,2) ./ nansum(hold_classes(:,2));
    
    fileName_tabulated_present = strcat(folder_results, '/tabulated_holdridge_results_present_', date, '.csv');
    writematrix(hold_classes, fileName_tabulated_present);
    
    
    %% 7 classes
    class_hold_7class = xlsread(...
        'input/holdridge_classification.xlsx','K3:K40');
    class_hold_7class(:,2) = xlsread(...
        'input/holdridge_classification.xlsx','M3:M40');
    
    results_holdridge = zeros(size(hIndex,1),1,'single');
    
    for class = 1:size(class_hold_7class,1)
        temp_hold7 = v_data_hold_present(:,10) == class;
        temp_hold_class7 = zeros(size(hIndex,1),1,'single');
        temp_hold_class7(temp_hold7) = class_hold_7class(class,2);
        
        results_holdridge = results_holdridge + temp_hold_class7;
    end
    
    v_data_hold_7 = results_holdridge;
    clearvars results_holdridge;
    
    % area for each class & holdridge classes
    
    hold_classes7 = accumarray(int32(v_data_hold_7),int32(v_data_hold_7),[],@nanmean);
    hold_classes7(:,2) = accumarray(int32(v_data_hold_7),v_area_5arcmin,[],@nansum);
    hold_classes7(:,3) = hold_classes7(:,2) ./ nansum(hold_classes7(:,2));
    
    v_data_hold_present(:,11) = v_data_hold_7;
    
    fileName_tabulated_7_present = strcat(folder_results, '/tabulated_holdridge_results_7_present_', date, '.csv');
    writematrix(hold_classes7,fileName_tabulated_7_present);
    
    clearvars temp*
    
    %% save present data
    save(file_resultsPresent, 'v_*', 'hLand', 'hIndex', 'hold_class*')
    
    
    %% future
    
    %% linearly interpolate between first and second future period to arrive
    % at the IPCC estimate of future year when warming level is crossed

    data_holdridge_future = (data_holdridge_future_1st * (year_2nd - year_ipcc) ...
        + data_holdridge_future_2nd * (year_ipcc - year_1st)) / (year_2nd - year_1st);

    % round all frost thresholds between 0 and 1 to 0, because it means that frost
    % is present within the period
    
    data_holdridge_future(:,:,4,:) = floor(data_holdridge_future(:,:,4,:));

    %% data to vectors
    hIndex = find(hLand);
    % future
    
    v_data_hold_future = NaN(size(hIndex,1),size(data_holdridge_future,3),size(data_holdridge_future,4),'single');
   
    for gcm = 1:size(data_holdridge_future,4)
        for var = 1:size(data_holdridge_future,3)
            temp_hold = data_holdridge_future(:,:,var,gcm);
            v_data_hold_future(:,var,gcm) = temp_hold(hIndex);
            clearvars temp_*
        end
    end

    clearvars gcm var temp_*
    
    %% data in cartesian coordinates
    % based on holdridge_coordinates.xlsx
    
    % min precip, min PETratio
    % max precip, max PETratio
    
    hold_class_bound = [62.5 0.125; 16000 32];
    
    for gcm = 1:size(data_holdridge_future,4)
            
        [temp_data_out] = f_holdridge_cartesian_coord(hold_class_bound,v_data_hold_future(:,:,gcm));
        v_data_hold_future(:,6:7,gcm) = temp_data_out;
           
    end
    
    clearvars temp*
    
    %% holdridge classification
    % id without frost
    hold_class_centroids = xlsread(...
        'input/holdridge_coordinates.xlsx','A10:A45');
    % id with frost
    hold_class_centroids(:,2) = xlsread(...
        'input/holdridge_coordinates.xlsx','B10:B45');
    % centroids
    hold_class_centroids(:,3:4) = xlsread(...
        'input/holdridge_coordinates.xlsx','M10:N45');
    
    area_5arcmin = areacell(5/60);
    v_area_5arcmin = area_5arcmin(hIndex);
    clearvars area_5arcmin;
    
    hold_classes_fut = NaN(38,3,size(data_holdridge_future,4),'single');
    %%
    
    for gcm = 1:size(data_holdridge_future,4)
        
        [temp_data_out] = f_holdridge_classification(hold_class_centroids,v_data_hold_future(:,:,gcm));
        v_data_hold_future(:,9:10,gcm) = temp_data_out;
        clearvars temp*
        
        % area for each class & holdridge classes
        
        temp = accumarray((int32(v_data_hold_future(:,10,gcm))),...
            int32(v_data_hold_future(:,10,gcm)),[],@nanmean);
        hold_classes_fut(1:size(temp,1),1,gcm) = temp;
        hold_classes_fut(1:size(temp,1),2,gcm) = ...
            accumarray(int32(v_data_hold_future(:,10,gcm)),v_area_5arcmin,[],@nansum);
        hold_classes_fut(1:size(temp,1),3,gcm) = ...
            hold_classes_fut(1:size(temp,1),2,gcm) ./ nansum(hold_classes_fut(1:size(temp,1),2,gcm));
                
    end
    
    
    %% 7 classes
    class_hold_7class = xlsread(...
        'input/holdridge_classification.xlsx','K3:K40');
    class_hold_7class(:,2) = xlsread(...
        'input/holdridge_classification.xlsx','M3:M40');
    
    
    for gcm = 1:size(data_holdridge_future,4)          
    
        results_holdridge = zeros(size(hIndex,1),1,'single');
        
        for class = 1:size(class_hold_7class,1)
            temp_hold7 = v_data_hold_future(:,10,gcm) == class;
            temp_hold_class7 = zeros(size(hIndex,1),1,'single');
            temp_hold_class7(temp_hold7) = class_hold_7class(class,2);
            
            results_holdridge = results_holdridge + temp_hold_class7;
        end
        
            v_data_hold_7_fut(:,gcm) = results_holdridge;
            %clearvars results_holdridge;
            
            % area for each class & holdridge classes
            
            hold_classes7_fut(:,1,gcm) = ...
                accumarray(int32(v_data_hold_7_fut(:,gcm)),int32(v_data_hold_7_fut(:,gcm)),[],@nanmean);
            hold_classes7_fut(:,2,gcm) = accumarray(int32(v_data_hold_7_fut(:,gcm)),v_area_5arcmin,[],@nansum);
            hold_classes7_fut(:,3,gcm) = hold_classes7_fut(:,2,gcm) ./ nansum(hold_classes7_fut(:,2,gcm));
            
    end


    
    
    %% save
    save(file_resultsFuture, 'v_*_fut*', 'hLand', 'hIndex', 'hold_class*_fut*', '-v7.3')
    
    size(v_data_hold_future)

end