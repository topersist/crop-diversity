function [hold_map_present, hold_map_future, hold_map_change] = f_import_hold_data(dir, hold_src)

    cd(dir)

    % hold_map_future, hold_map_change (dimensions: 2160 x 4320 x 3 x 8)
    % Third dimension:
    %   1 layer: Holdridge classification (see xls file provided separately)
    %   2 layer: Cartesian x-coordinate
    %   3 layer: Cartesian y-coordinate
    % Fourth dimension: different GCMs

    % hold_map_present (dimensions: 2160 x 4320 x 3)
    % Third dimension:
    %   1 layer: Holdridge classification (see xls file provided separately)
    %   2 layer: Cartesian x-coordinate
    %   3 layer: Cartesian y-coordinate
    
    
    % Import holdridge data for the specified scenario (i.e. warming level)
    if strcmp(hold_src, 'current')
        data = load('holdridge_data/holdridge_resultsMap_15wp_1.mat');
    elseif strcmp(hold_src, '15wp')
        data = load('holdridge_data/holdridge_resultsMap_15wp_1.mat');
    elseif strcmp(hold_src, '2wp')
        data = load('holdridge_data/holdridge_resultsMap_2wp_1.mat');
    elseif strcmp(hold_src, '3wp')
        data = load('holdridge_data/holdridge_resultsMap_3wp_1.mat');
    elseif strcmp(hold_src, '4wp')
        data = load('holdridge_data/holdridge_resultsMap_4wp_1.mat');
    end

    hold_map_present = data.hold_map_present;
    hold_map_future = data.hold_map_future;
    hold_map_change = data.hold_map_change;
    
end
