function [AREA_GRID, LON, LAT] = areacell(RESOLUTION)

% Calculates a square grid for the globe with the area of each gridcell, as well as grids for latitude and longitude coordinates.
% The cell middle point is taken for the calculation.
% Unit is in square km.
% Calculates areas as fraction of 1 dgr, also if resolution is >1
% Formulas used are:
% 1 dgr lon = 111.325 * cos(latitude) [in km]
% 1 dgr lat = 111.133 - 0.559 * cos (2*latitude) [in km]

%% Calculate area
lat_vector = (90-(RESOLUTION/2) : -RESOLUTION : -90+(RESOLUTION/2))';
lat_km = (111.133 - 0.559 * cosd(2.*lat_vector)) .*RESOLUTION; 
lon_km = 111.325 * cosd(lat_vector) .*RESOLUTION;
km2 = lat_km.*lon_km;
AREA_GRID = repmat(km2,1,2*length(lat_vector));

%% Create lat/lon grids
LAT = repmat(lat_vector,1,2*length(lat_vector));
lon_vector = -180+(RESOLUTION/2) : RESOLUTION : 180-(RESOLUTION/2);
LON = repmat(lon_vector,length(lat_vector),1);
