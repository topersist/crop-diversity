function [data_out] = f_holdridge_classification(class_centroids,data_in)


% distance to centroid of each honeycomb
temp_distance = zeros(size(data_in,1),size(class_centroids,1),'single');
v_data_hold_cartesian = data_in(:,6:7);

for class = 1:size(class_centroids,1)
    temp_distance(:,class) = sqrt( power((class_centroids(class,3)-v_data_hold_cartesian(:,1)),2)...
        +(power((class_centroids(class,4)-v_data_hold_cartesian(:,2)),2)));
end

% find the minimum value and location
[temp_min(:,1),temp_min(:,2)] = min(temp_distance,[],2);

data_out = zeros(size(data_in,1),2,'single');

% store the index number of closest centroid
v_data_hold_cartesian(:,3) = temp_min(:,2);
data_out(:,1) = temp_min(:,2);

clearvars temp_*

% find the corresponding holdridge ID

results_holdridge = zeros(size(data_in,1),1,'single');

for class = 1:size(class_centroids,1)
    temp_class_no_frost = and(v_data_hold_cartesian(:,3) == class, data_in(:,4) == 1);
    temp_class_frost = and(v_data_hold_cartesian(:,3) == class, data_in(:,4) == 0);
    
    temp_hold_no_frost = zeros(size(data_in,1),1,'single');
    temp_hold_frost = zeros(size(data_in,1),1,'single');
    
    temp_hold_no_frost(temp_class_no_frost) = class_centroids(class,1);
    temp_hold_frost(temp_class_frost) = class_centroids(class,2);
    
    results_holdridge = results_holdridge + temp_hold_no_frost + temp_hold_frost;
    
    clearvars temp_*
end

data_out(:,2) = results_holdridge;



end