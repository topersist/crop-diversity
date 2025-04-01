%% holdridge - download data

clear
close all


% set the folder where .m file is as a working directory
folder_wd = fileparts(matlab.desktop.editor.getActiveFilename);
cd(folder_wd)

%% Initialise run

% folder where data will be stored
data_folder = fullfile(folder_wd,'data')

% version of the run
s_version = '1';

% download all data and calculate parameters (1)
% only download baseline & current data, calculate parameters (2)
% only download future data, calculate parameters (3)
% or you have the data, and just want to calculate parameters (0)
s_download = 0;

% holdridge parameters within growing season (1) or the whole year (0)?
% growing season calculation available only for maize and soybean.
s_crop_cal = 0;


%%
% download crop growing season / crop calendar filter for maize if needed
if (s_crop_cal == 1)
    % change crop calendar filter filename as necessary
    gs_filter_path = strcat(folder_wd, '/crop_calendar/growing_season_filter_soy.tif');
    [gs_filter, R_filter] = readgeoraster(gs_filter_path); % growing season filter
else
    gs_filter = ones(2160, 4320, 12); % a filter that lets all data through
end

%% DO NOT MODIFY FROM HERE ON

% future time period (2030,2050,2070 or 2090)
future_years = {'2030', '2050', '2070', '2090'};

for year = 1:size(future_years, 2)
    
    futureYear = future_years{year};

    %% create folders if needed
    
    % folder of the input data for present (to be downloaded from internet)
    folder_data_present = fullfile(data_folder,append('data_','present'));
    if exist(folder_data_present, 'dir')
        
        % do nothing if exists
    else
        % create folder if not
        mkdir( folder_data_present );
    end
    
    % folder of the input data for baseline (to be downloaded from internet)
    folder_data_baseline = fullfile(data_folder,append('data_','baseline'));
    if exist(folder_data_baseline, 'dir')
        
        % do nothing if exists
    else
        % create folder if not
        mkdir( folder_data_baseline );
    end
    
    % folder of the input data for future (to be downloaded from internet)
    folder_data_future = fullfile(data_folder, append('/data_',futureYear));
    if exist(folder_data_future, 'dir')
        
        % do nothing if exists
    else
        % create folder if not
        mkdir( folder_data_future );
    end
    
    % folder of the output
    folder_results = fullfile(folder_wd,'holdridge_data');
    if exist(folder_results, 'dir')
        
        % do nothing if exists
    else
        % create folder if not
        mkdir( folder_results );
    end
    
    % add paths of all needed functions
    folder_functions = fullfile(folder_wd,'functions');
    addpath(genpath(folder_functions));
    
    %% define input and out data strings
    
    % date of running the code
    date = datestr(now, 'yyyymmdd');
    
    % output data
    file_output_data = sprintf('holdridge_data/holdridge_data_year%s_%s.mat', futureYear, s_version );
    
    % sheet for future URL
    future_sheet = sprintf('future_%s', futureYear);
    
    
    %% download present and baseline data
    
    if ((s_download == 1)||(s_download == 2))
        % download
    
        % specify websave options
        options = weboptions('Timeout', Inf);
        
        % load the URL list for present data
        cd(folder_wd)
        urlList = readtable( ...
            'input/worldclim_data_download_4scen.xlsx','Sheet','current','Range','D18:D26', ReadVariableNames=false)
    
        % go to data folder, store main folder
        mainFolder = cd(folder_data_present);
    
    
        for i = 1:size(urlList,1)    
            fileNameZip = 'temp.zip';
            fileUrl = urlList{i,1}{1};
            tempUrl = websave(fileNameZip,fileUrl, options);
    
            unzip(fullfile(folder_data_present,fileNameZip));
    
            delete *zip
            clearvars fileNameZip fileUrl tempUrl k 
        end
        
    
    % load the URL list for baseline data
        cd(folder_wd)
        urlList = readtable( ... 
            'input/worldclim_data_download_4scen.xlsx','Sheet','current','Range','D6:D8', ReadVariableNames=false);
        
        % go to data folder, store main folder
        mainFolder = cd(folder_data_baseline);
    
    
        for i = 1:size(urlList,1)
            
            fileNameZip = 'temp.zip';
            fileUrl = urlList{i,1}{1}
            tempUrl = websave(fileNameZip,fileUrl, options);
            
            unzip(fullfile(folder_data_baseline,fileNameZip));
            
            delete *zip
            clearvars fileNameZip fileUrl tempUrl k 
        end
        
    
        cd(mainFolder)
    
    else
        % nothing happends
    end
    
     cd(folder_wd)
    
    
    %% calculate holdridge parameters
    % go to data folder, store main folder
    mainFolder = cd(folder_data_present);
    
    % list of variables
    temp_vars = {'*tmin*.tif','*tmax*.tif','*prec*.tif'};
    
    % create empty matrix
    temp_data_present = zeros(2160,4320,12,size(temp_vars, 2),'single'); % lat, lon, month, variable
    
    % loop through variables
    for i = 1:size(temp_vars,2)
        temp_listing = dir(temp_vars{1,i});
    
        % loop trough files
        for k = 1:size(temp_listing,1)
            fileName = getfield(temp_listing,{k,1}, 'name');
            
            % extract data month
            month = str2num(fileName(end-5:end-4));
            
            temp = imread( fullfile(folder_data_present,fileName) );
            
            temp(temp == temp(1,1)) = NaN; % sea to NaN
            % create land and sea mask if not exists
            if exist('hLand','var') == 0
                hSea = isnan(temp);
                hLand = hSea == 0;
            else
                % nothing happends
            end
    
            % sum loaded data with all previous data from the same month
            temp_data_present(:,:,month,i) = temp_data_present(:,:,month,i) + temp;
            clearvars temp
        end
       
    end
    
    % transform cumulative monthly sums of variables to monthly means
    temp_data_present(:,:,:,:) = temp_data_present / (size(temp_listing,1)/12);
    
    clearvars fileName temp_listing
    %%
    % load average temperature data from 1970-2000 to be used for bias
    % correction of mean temperatures from other periods
    
    % go to baseline data folder
    cd(folder_data_baseline)

    % file name
    temp_vars = {'*tmin*.tif','*tmax*.tif', '*tavg*.tif'};
    
    % create empty matrix
    temp_data_baseline = zeros(2160,4320,12,size(temp_vars, 2),'single'); % lat, lon, month, variable
    
      % loop through variables
    for i = 1:size(temp_vars,2)
        temp_listing = dir(temp_vars{1,i});
    
        % loop trough files
        for k = 1:size(temp_listing,1)
            fileName = getfield(temp_listing,{k,1}, 'name')
            
            temp = imread( fullfile(folder_data_baseline,fileName) );
            
            temp(temp == temp(1,1)) = NaN; % sea to NaN
            % create land and sea mask if not exists
            if exist('hLand','var') == 0
                hSea = isnan(temp);
                hLand = hSea == 0;
            else
                % nothing happends
            end
        
            % sum loaded data with all previous data from the same month
            temp_data_baseline(:,:,k,i) = temp;
            clearvars temp
        end
    end
    
    clearvars fileName temp_listing
    
    %%
    % test
    % figure; imagesc(temp_data_present(:,:,1,1))
    % figure; imagesc(temp_data_present(:,:,7,1))
    
    % we can use for future calculations only tmin and tmax; thus, let's make a
    % monthly specific bias correction to that by using baseline tavg
    % to avoid problem with negative
    temp_tmean = squeeze(mean(temp_data_baseline(:,:,1:12,1:2),4, "omitmissing"));
    temp_tavg = temp_data_baseline(:,:,1:12,3);
    
    bias_correction_tavg = 1+ ( (temp_tavg - temp_tmean)./temp_tmean );
    
    % calculate holdridge parameters for present data
    temp_data = temp_data_present;
    
    [temp_data_out] = f_holdridge_parameters_no_Tmax_cap(temp_data,bias_correction_tavg, gs_filter);
    data_holdridge_present = temp_data_out;
    
    % order of layers
    % 1. PET ratio
    % 2. annual precipitation
    % 3. biotemperature
    % 4. frost threshold
    
    % testing
    %figure; imagesc(data_holdridge_present(:,:,1))
    %figure; imagesc(data_holdridge_present(:,:,2))
    %figure; imagesc(data_holdridge_present(:,:,3))
    %figure; imagesc(data_holdridge_present(:,:,4))
    %figure; imagesc(bias_correction_tavg(:,:,3))
    %figure; imagesc(temp_data(:,:,6,2))
    
    % go back to main folder
    cd(mainFolder)
    
    % save data
    if isfile(file_output_data)
        % if file does exist, only append to that one
        save(file_output_data,'data_holdridge_present','-append')
    else
        % if it does not exist, create file and save to that one
        save(file_output_data, 'data_holdridge_present','-v7.3')
        fprintf('output file created')
    end
    
    save(file_output_data,'hSea','-append')
    save(file_output_data,'hLand','-append')
    save(file_output_data,'bias_correction_tavg','-append')
    save('holdridge_data/hLand', 'hLand')
    
    %% calculate PET with three methods
    % PET org holdridge
    % PET modified holdridge
    % PET hargreaves
    % PET Thornthwaite
    
    %[PET_data_out] = f_PET_no_Tmax_cap(temp_data,bias_correction_tavg);
    
    % figure; imagesc(PET_data_out(:,:,1))
    % figure; imagesc(PET_data_out(:,:,2))
    % figure; imagesc(PET_data_out(:,:,3))
    % figure; imagesc(PET_data_out(:,:,4))
    
    
    % nccreate('results/PET_2020_11_12.nc','spatial_data', 'Dimensions', {'y', size(PET_data_out,1),...
    %     'x', size(PET_data_out,2), 'result', size(PET_data_out,3)},...
    %     'Format','netcdf4_classic','DeflateLevel',9);
    % % write netcdf
    % ncwrite('results/PET_2020_11_12.nc','spatial_data',PET_data_out);
    
    
    %% clear vars
    clearvars temp* k url* i
    
    %% download future data
    
    if ((s_download == 1)||(s_download == 3))
        % download
    % specify websave options
    options = weboptions('Timeout', Inf);    
    
    cd(folder_wd)
    [num,urlList,raw] = xlsread(...
        'input/worldclim_data_download_4scen.xlsx',future_sheet,'I7:H141');
    clearvars num raw
    
    % go to data folder, store main folder
    mainFolder = cd(folder_data_future);
    
    % clear empty cells
    urlList = urlList(~cellfun('isempty',urlList));
    
    %  download datasets
    for gcm = 1:8
        %disp(['current gcm to be downloaded is ', num2str(gcm)]);
        for ssp = 1:4
            %disp(['current ssp to be downloaded is ', num2str(ssp)]);
            
            
            for var = 1:3 % we do not need the 4th dimension, i.e. bio variables
                %disp(['current var to be downloaded is ', num2str(var)]);
                %fileNameZip = 'temp.zip';
                fileUrl = urlList{(gcm-1)*16 + (ssp-1)*4 + var,1};
                fileNameZip = fileUrl(58:94);
                   
                % check if already downloaded
                
                %if exist('data_holdridge_future','var') == 0
                %    load(file_output_data)
                %    fprintf('exists')
                %else
                    % nothing happends
                %end
                
                if (gcm == 8) && (ssp == 4) && (var == 2) && (strcmp(futureYear,'2030'))
                    % nothing happens
                else
                    tempUrl = websave(fileNameZip,fileUrl, options);
                
                    unzip(fullfile(folder_data_future,fileNameZip))
                    %temp_listing = dir('*.tif');
                end
                
            end
            
            clearvars fileNameZip fileUrl tempUrl k fileName temp_listing
            
            delete *tif *zip  *txt
            
        end
        
        
    end
    cd(mainFolder)
    else
        % nothing happends
    end
    
    %% calculate the holdridge parameters
    cd(folder_wd)
    [num,urlList,raw] = xlsread(...
        'input/worldclim_data_download_4scen.xlsx',future_sheet,'I7:H141');
    clearvars num raw
    
    % load needed data (if not in workspace)
    if exist('hLand','var') == 0
        load(file_output_data,'hSea','hLand','bias_correction_tavg')
        fprintf('land mask, sea mask and bias correction loaded')
    else
        % nothing happends
    end
    
    % go to data folder, store main folder
    mainFolder = cd(folder_data_future);
    
    % clear empty cells
    urlList = urlList(~cellfun('isempty',urlList));
    
    listing = dir(append(folder_data_future,'/**/*.tif'));
    %%
    data_holdridge_future = zeros(2160,4320,4,4,8,'single'); % lat, lon, holdridge parameter, ssp, GCM
    
    for gcm = 1:8
        disp(['current gcm is ', num2str(gcm)]);
        for ssp = 1:4
            disp(['current ssp is ', num2str(ssp)]);
            
            temp_data = zeros(2160,4320,12,3,'single'); % lat, lon, month, variable
            
            for var = 1:3
                disp(['current var is ', num2str(var)]);
                
                if (gcm == 8) && (ssp == 4) && (strcmp(futureYear,'2030'))
                    % fill by nan because tmax for this  gcm is not available
                    temp = NaN(2160,4320,12); % everything to NaN
                    max(temp, [], 'all')
                    temp_data(1:size(temp,1),:,:,var) = temp;
                    clearvars temp
    
                else
                    % get correct filename
                
                    fileName = getfield(listing,{(gcm-1)*12 + (ssp-1)*3 + (4-var),1}, 'name')
                    
                    folder = getfield(listing,{(gcm-1)*12 + (ssp-1)*3 + (4-var),1}, 'folder');
        
                    temp = imread( fullfile(folder,fileName) );
                    temp(temp == temp(1,1)) = NaN; % sea to NaN
                    temp_data(1:size(temp,1),:,:,var) = temp;
                    clearvars temp
    
                end   
                
            end
            
            clearvars fileNameZip fileUrl tempUrl k fileName
         
            % calculate needed parameters
            [temp_data_out] = f_holdridge_parameters_no_Tmax_cap(temp_data,bias_correction_tavg, gs_filter);
            max(temp_data_out, [], 'all')
            
            % store data
            data_holdridge_future(:,:,:,ssp,gcm) = temp_data_out;
            
            clearvars temp_data temp_data_out
        end
    end
    
    % save
    cd(mainFolder)
    save(file_output_data,'data_holdridge_future','-append')
    
    clearvars ssp gcm
    
    %% plot some data data
    % 
    % if exist('data_holdridge_future','var') == 0
    %      load(file_output_data)
    % else
    %      % nothing happends
    % end
    % 
    % load(file_output_data)
    % size(data_holdridge_future)
     %%
    % % present PET, prec, biotemp, frost threshold 
    % figure; imagesc(data_holdridge_present(:,:,1))
    % figure; imagesc(data_holdridge_present(:,:,2))
    % figure; imagesc(data_holdridge_present(:,:,3))
    % figure; imagesc(data_holdridge_present(:,:,4))
    %%
    % future year PET, prec, biotemp, frost threshold (averaged from gcm's)
    % for ssp 1 = 2.6
    % figure; imagesc(nanmean(data_holdridge_future(:,:,1,1,:),5))%-data_holdridge_present(:,:,1))
    % figure; imagesc(nanmean(data_holdridge_future(:,:,2,1,:),5))%-data_holdridge_present(:,:,2))
    % figure; imagesc(nanmean(data_holdridge_future(:,:,3,1,:),5))%-data_holdridge_present(:,:,3))
    % figure; imagesc(nanmean(data_holdridge_future(:,:,4,1,:),5))
    % %%
    % % for ssp2 = 4.5 
    % figure; imagesc(nanmean(data_holdridge_future(:,:,1,2,:),5))%-data_holdridge_present(:,:,1))
    % figure; imagesc(nanmean(data_holdridge_future(:,:,2,2,:),5))%-data_holdridge_present(:,:,2))
    % figure; imagesc(nanmean(data_holdridge_future(:,:,3,2,:),5))%-data_holdridge_present(:,:,3))
    % figure; imagesc(nanmean(data_holdridge_future(:,:,4,2,:),5))
    % %%
    % % for ssp3 = 7.0
    % figure; imagesc(nanmean(data_holdridge_future(:,:,1,3,:),5))%-data_holdridge_present(:,:,1))
    % figure; imagesc(nanmean(data_holdridge_future(:,:,2,3,:),5))%-data_holdridge_present(:,:,2))
    % figure; imagesc(nanmean(data_holdridge_future(:,:,3,3,:),5))%-data_holdridge_present(:,:,3))
    % figure; imagesc(nanmean(data_holdridge_future(:,:,4,3,:),5))
    % %%
    % % for ssp4 = 8.5
    % figure; imagesc(nanmean(data_holdridge_future(:,:,1,4,:),5))%-data_holdridge_present(:,:,1))
    % figure; imagesc(nanmean(data_holdridge_future(:,:,2,4,:),5))%-data_holdridge_present(:,:,2))
    % figure; imagesc(nanmean(data_holdridge_future(:,:,3,4,:),5))%-data_holdridge_present(:,:,3))
    % figure; imagesc(nanmean(data_holdridge_future(:,:,4,4,:),5))
    
    %% write output
    % 
    % % order of layers
    % % 1. PET ratio
    % % 2. annual precipitation
    % % 3. biotemperature
    % % 4. frost threshold
    % 
    % if exist('data_holdridge_future','var') == 0
    %     load(file_output_data)
    % else
    %     % nothing happends
    % end
    % 
    % 
    % spat_data_col = data_holdridge_present;
    % 
    % % ssp2.6 mean over GCMs
    % spat_data_col(:,:,:,2) = nanmean(data_holdridge_future(:,:,:,1,:),5);
    % % ssp8.5 mean over GCMs
    % spat_data_col(:,:,:,3) = nanmean(data_holdridge_future(:,:,:,2,:),5);
    % 
    % 
    % 
    % 
    % % present
    % nc_namePresent = sprintf('results/holdridge_input_present_%s.mat',date );
    % nccreate(nc_namePresent,'spatial_data', 'Dimensions', {'y', size(spat_data_col,1),...
    %     'x', size(spat_data_col,2), 'result', size(spat_data_col,3)},...
    %     'Format','netcdf4_classic','DeflateLevel',9);
    % % ncdisp(nc_namePresent);
    % % ncinfo(nc_namePresent);
    % % write netcdf
    % ncwrite(nc_namePresent,'spatial_data',spat_data_col(:,:,:,1));
    % 
    % % ssp2.6
    % nc_namessp26 = sprintf('results/holdridge_input_ssp26_year%s_%s.mat', futureYear,date );
    % nccreate(nc_namessp26,'spatial_data', 'Dimensions', {'y', size(spat_data_col,1),...
    %     'x', size(spat_data_col,2), 'result', size(spat_data_col,3)},...
    %     'Format','netcdf4_classic','DeflateLevel',9);
    % % write netcdf
    % ncwrite(nc_namessp26,'spatial_data',spat_data_col(:,:,:,2));
    % 
    % % ssp8.5
    % nc_namessp85 = sprintf('results/holdridge_input_ssp85_year%s_%s.mat', futureYear,date );
    % nccreate(nc_namessp85,'spatial_data', 'Dimensions', {'y', size(spat_data_col,1),...
    %     'x', size(spat_data_col,2), 'result', size(spat_data_col,3)},...
    %     'Format','netcdf4_classic','DeflateLevel',9);
    % % write netcdf
    % ncwrite(nc_namessp85,'spatial_data',spat_data_col(:,:,:,3));
    % 
    % 
    % 
    % % write geotiff
    % R_5arcmin = georasterref('RasterSize', [2160 4320], ...
    %     'RasterInterpretation', 'cells', 'ColumnsStartFrom', 'north', ...
    %     'LatitudeLimits', [-90 90], 'LongitudeLimits', [-180 180]);
    % 
    % 
    % temp = single(data_holdridge_present(:,:,1));
    % temp(temp == 0) = NaN;
    % figure;imagesc(temp);
    % geotiffwrite('results/pet_ratio_present.tif',temp,R_5arcmin);
    % 
    % temp = single(data_holdridge_present(:,:,2));
    % temp(temp == 0) = NaN;
    % figure;imagesc(temp);
    % geotiffwrite('results/prec_present.tif',temp,R_5arcmin);
    % 
    % temp = single(data_holdridge_present(:,:,3));
    % temp(temp == 0) = NaN;
    % figure;imagesc(temp);
    % geotiffwrite('results/biotemp_present.tif',temp,R_5arcmin);
end
