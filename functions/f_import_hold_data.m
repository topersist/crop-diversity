function [hold_map_present, hold_map_future, hold_map_change] = f_import_hold_data(dir, hold_src)

    cd(dir)

    % hold_map_future (dimensions: 2160 x 4320 x 3 x 4 x 11)
    % Third dimension:
    %   1 layer: Holdridge classification (see xls file provided separately)
    %   2 layer: Cartesian x-coordinate
    %   3 layer: Cartesian y-coordinate
    % Fourth dimension: different RCPs (2.6, 4.5, 7.0, 8.5)
    % Fifth dimension: different GCMs

    % hold_map_present (dimensions: 2160 x 4320 x 3)
    % Third dimension:
    %   1 layer: Holdridge classification (see xls file provided separately)
    %   2 layer: Cartesian x-coordinate
    %   3 layer: Cartesian y-coordinate
    
    
    % Import holdridge data for the specified scenario (i.e. year)
    if strcmp(hold_src, 'current')
        data = load('holdridge_data/holdridge_resultsMap_year2090_2.mat');
    elseif strcmp(hold_src, 'hold_2090')
        data = load('holdridge_data/holdridge_resultsMap_year2090_2.mat');
    elseif strcmp(hold_src, 'hold_2070')
        data = load('holdridge_data/holdridge_resultsMap_year2070_2.mat');
    elseif strcmp(hold_src, 'hold_2050')
        data = load('holdridge_data/holdridge_resultsMap_year2050_2.mat');
    elseif strcmp(hold_src, 'hold_2030')
        data = load('holdridge_data/holdridge_resultsMap_year2030_2.mat');
    end

    hold_map_present = data.hold_map_present;
    hold_map_future = data.hold_map_future;
    hold_map_change = data.hold_map_change;
    
end
