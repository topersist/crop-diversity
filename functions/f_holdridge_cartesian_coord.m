function [v_data_hold_cartesian] = f_holdridge_cartesian_coord(class_bound,data_in)
%%
v_data_hold_cartesian = zeros(size(data_in,1),2,'single');
temp_position = zeros(size(data_in,1),2,'single');

% in case of zero or negative
data_in(data_in<=0) = 0;


% calculate the position 
% precip
temp_position(:,1) = ( log2(data_in(:,2))-log2(class_bound(1,1)) ) / ( log2(class_bound(2,1))-log2(class_bound(1,1)) );
% PET ratio
temp_position(:,2) = ( log2(data_in(:,1))-log2(class_bound(1,2)) ) / ( log2(class_bound(2,2))-log2(class_bound(1,2)) );
%%
% in cases outside the boundaries, above 1 to 1; below 0 to 0
temp_position(temp_position < 0) = 0;
temp_position(temp_position > 1) = 1;

% to cartesian coordinates (x,y)
v_data_hold_cartesian(:,1) = 0.5 + 0.5 .* temp_position(:,1) - 0.5 .* temp_position(:,2);
v_data_hold_cartesian(:,2) = 1 - temp_position(:,1) - temp_position(:,2);

clearvars temp*
end