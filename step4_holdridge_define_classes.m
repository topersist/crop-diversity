
%% holdridge - define classes
clear
close all
clc

% set the folder where .m file is as a working directory
folder_wd = fileparts(matlab.desktop.editor.getActiveFilename);
cd(folder_wd)

%% Initialise run

% future time period (2030,2050,2070 or 2090)
futureYear = '2090';

% version of the run
s_version = '2';


%% DO NOT MODIFY FROM HERE ON

%% folder paths

% folder of the output
folder_results = fullfile(folder_wd,'holdridge_data');

% add paths of all needed functions 
folder_functions = fullfile(folder_wd,'functions');
addpath(genpath(folder_functions));

%% define input and out data strings

% date of running the code
date = datestr(now, 'yyyymmdd');

% saved data from step1
file_step1 = sprintf('holdridge_data/holdridge_data_year%s_%s.mat', futureYear,s_version ); 

% output data
file_resultsPresent = sprintf('holdridge_data/holdridge_results_present_%s.mat', s_version ); 
file_resultsFuture = sprintf('holdridge_data/holdridge_results_year%s_%s.mat', futureYear, s_version ); 


%% present
%% load data from step 1
load(file_step1)

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

fileName_tabulated_present = sprintf('holdridge_data/tabulated_holdridge_results_present_%s.csv',date );
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

fileName_tabulated_7_present = sprintf('holdridge_data/tabulated_holdridge_results_7_present_%s.csv',date );
writematrix(hold_classes7,fileName_tabulated_7_present);

clearvars temp*

%% save present data
save(file_resultsPresent, 'v_*', 'hLand', 'hIndex', 'hold_class*')


%% future
%% data to vectors
hIndex = find(hLand);
% future

v_data_hold_future = NaN(size(hIndex,1),size(data_holdridge_future,3),size(data_holdridge_future,4)...
    ,size(data_holdridge_future,5),'single');

for gcm = 1:size(data_holdridge_future,5)
    for rcp = 1:size(data_holdridge_future,4)
        for var = 1:size(data_holdridge_future,3)
            temp_hold = data_holdridge_future(:,:,var,rcp,gcm);
            v_data_hold_future(:,var,rcp,gcm) = temp_hold(hIndex);
            clearvars temp_*
        end
    end
end
clearvars gcm rcp var temp_*

%% data in cartesian coordinates
% based on holdridge_coordinates.xlsx

% min precip, min PETratio
% max precip, max PETratio

hold_class_bound = [62.5 0.125; 16000 32];

for gcm = 1:size(data_holdridge_future,5)
    for rcp = 1:size(data_holdridge_future,4)
        if (gcm==8) && (rcp==4)  && (strcmp(futureYear,'2030'))
            %do nothing because no data
            v_data_hold_future(:,6:7,rcp,gcm) = NaN(size(v_data_hold_future(:,:,rcp,gcm),1),2,'single');
        else
            [temp_data_out] = f_holdridge_cartesian_coord(hold_class_bound,v_data_hold_future(:,:,rcp,gcm));
            v_data_hold_future(:,6:7,rcp,gcm) = temp_data_out;
        end
    end
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

hold_classes_fut = NaN(38,3,size(data_holdridge_future,4)...
    ,size(data_holdridge_future,5),'single');
%%

for gcm = 1:size(data_holdridge_future,5)
    for rcp = 1:size(data_holdridge_future,4)

        if (gcm==8) && (rcp==4) && (strcmp(futureYear,'2030'))
            %do nothing because no data
            [temp_data_out] = NaN(size(v_data_hold_future(:,:,rcp,gcm),1),2,'single');
            v_data_hold_future(:,9:10,rcp,gcm) = temp_data_out;
            clearvars temp*

            % area for each class & holdridge classes
            temp = accumarray(int32(v_data_hold_future(:,10,1,1)),...
                int32(v_data_hold_future(:,10,1,1)),[],@nanmean);
            hold_classes_fut(1:size(temp,1),1,rcp,gcm) = temp;
            hold_classes_fut(1:size(temp,1),2,rcp,gcm) = NaN;
            hold_classes_fut(1:size(temp,1),3,rcp,gcm) = NaN;
                
        else
            [temp_data_out] = f_holdridge_classification(hold_class_centroids,v_data_hold_future(:,:,rcp,gcm));
            v_data_hold_future(:,9:10,rcp,gcm) = temp_data_out;
            clearvars temp*
            
            % area for each class & holdridge classes
            
            temp = accumarray(int32(v_data_hold_future(:,10,rcp,gcm)),...
                int32(v_data_hold_future(:,10,rcp,gcm)),[],@nanmean);
            hold_classes_fut(1:size(temp,1),1,rcp,gcm) = temp;
            hold_classes_fut(1:size(temp,1),2,rcp,gcm) = ...
                accumarray(int32(v_data_hold_future(:,10,rcp,gcm)),v_area_5arcmin,[],@nansum);
            hold_classes_fut(1:size(temp,1),3,rcp,gcm) = ...
                hold_classes_fut(1:size(temp,1),2,rcp,gcm) ./ nansum(hold_classes_fut(1:size(temp,1),2,rcp,gcm));
                
        end

    end
end




%% 7 classes
class_hold_7class = xlsread(...
    'input/holdridge_classification.xlsx','K3:K40');
class_hold_7class(:,2) = xlsread(...
    'input/holdridge_classification.xlsx','M3:M40');


for gcm = 1:size(data_holdridge_future,5)
    for rcp = 1:size(data_holdridge_future,4)
        
        if (gcm==8) && (rcp==4)  && (strcmp(futureYear,'2030'))
            results_holdridge = NaN(size(hIndex,1),1,'single');
            %do nothing because no data
            temp_hold_class7 = NaN(size(hIndex,1),1,'single');
            results_holdridge = results_holdridge + temp_hold_class7;
            v_data_hold_7_fut(:,rcp,gcm) = results_holdridge;
            clearvars results_holdridge;

            hold_classes7_fut(:,1,rcp,gcm) = accumarray(int32(v_data_hold_7_fut(:,1,1)),int32(v_data_hold_7_fut(:,1,1)),[],@nanmean);
            hold_classes7_fut(:,2,rcp,gcm) = NaN;
            hold_classes7_fut(:,3,rcp,gcm) = NaN;
            
        else
            results_holdridge = zeros(size(hIndex,1),1,'single');
            
            for class = 1:size(class_hold_7class,1)
                temp_hold7 = v_data_hold_future(:,10,rcp,gcm) == class;
                temp_hold_class7 = zeros(size(hIndex,1),1,'single');
                temp_hold_class7(temp_hold7) = class_hold_7class(class,2);
                
                results_holdridge = results_holdridge + temp_hold_class7;
            end
            
            v_data_hold_7_fut(:,rcp,gcm) = results_holdridge;
            %clearvars results_holdridge;
            
            % area for each class & holdridge classes
            
            hold_classes7_fut(:,1,rcp,gcm) = ...
                accumarray(int32(v_data_hold_7_fut(:,rcp,gcm)),int32(v_data_hold_7_fut(:,rcp,gcm)),[],@nanmean);
            hold_classes7_fut(:,2,rcp,gcm) = accumarray(int32(v_data_hold_7_fut(:,rcp,gcm)),v_area_5arcmin,[],@nansum);
            hold_classes7_fut(:,3,rcp,gcm) = hold_classes7_fut(:,2,rcp,gcm) ./ nansum(hold_classes7_fut(:,2,rcp,gcm));
        end
    end
end


%% save
save(file_resultsFuture, 'v_*_fut*', 'hLand', 'hIndex', 'hold_class*_fut*', '-v7.3')

size(v_data_hold_future)