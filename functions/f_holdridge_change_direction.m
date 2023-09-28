function [data_out] = f_holdridge_change_direction(data_present,data_future,data_distance,threshold)

%% direction of change
% angle between two points
% atan2(y2-y1,x2-x1)
data_out = ...
    atan2d(data_future-data_present,...
    data_future-data_present);

%% category of direction

v_angle_temp = zeros(size(data_distance,1),1);

% latitudinal region direction (1)
temp = data_out(:,1);
temp_b = temp <= threshold(2) & temp > threshold(3);
v_angle_temp(temp_b) = 1;
clearvars temp*;

% wetter conditions (2)
temp = data_out(:,1);
temp_b = temp <= threshold(3) & temp > threshold(4);
v_angle_temp(temp_b) = 2;
clearvars temp*;

% larger PET ratio (3)
temp = data_out(:,1);
temp_b = or(temp <= threshold(4), temp > threshold(2));
v_angle_temp(temp_b) = 3;
clearvars temp*;

data_out(:,2) = v_angle_temp;

%% to HSV color
% h is angle
% s is distance
% v is darkness

% H: turn angle 30?, so that red color represents the PET ratio (i.e. from -150? to -180?); then scale to 0...1 range
% + reverse the angle to get more intuitive colors
data_out(:,3) = mat2gray(-(data_out(:,1) - 30));

% S: normalise distance to s
data_out(:,4) = (data_distance - ...
    repmat(quantile(data_distance,0.05),size(data_out(:,1),1),1))./...
    (repmat(quantile(data_distance,0.95),size(data_out(:,1),1),1)-...
    repmat(quantile(data_distance,0.05),size(data_out(:,1),1),1));

% V: set up v (here 0.7)
temp_v = 0.7;
data_out(:,5) = zeros(size(data_out(:,1))) + temp_v;

end


