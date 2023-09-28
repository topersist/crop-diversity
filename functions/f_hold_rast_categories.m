function [rast1_cats, rast2_cats] = f_hold_rast_categories(rast1, rast2, cats1, cats2)

    % Get area of each raster cell at 5/60 resolustion
    area = areacell(5/60);

    % Vectorize data for more efficient computations
    area1_v = area(:);
    area2_v = area(:);

    rast1_v = rast1(:);
    rast2_v = rast2(:);

    % Remove nan values from the data
    area1_v( isnan(rast1_v) ) = [];
    area2_v( isnan(rast2_v) ) = [];
    rast1_v( isnan(rast1_v) ) = [];
    rast2_v( isnan(rast2_v) ) = [];

    % If only the number of categories is specified (e.g. cats1 = 4) calculate
    % percentiles for equal intervals, number of which is specified by cats1 or
    % cats2. Otherwise calculate percentiles specified by the intervals in
    % cats1 or cats2
    if length(cats1) == 1
        rast1_cats = wprctile(rast1_v, linspace(0,100,cats1+1), area1_v, 6);
    else
        rast1_cats = wprctile(rast1_v, cats1, area1_v,6);
    end

    if length(cats2) == 1
        rast2_cats = wprctile(rast2_v, linspace(0,100,cats2+1), area2_v, 6);
    else
        rast2_cats = wprctile(rast2_v, cats2, area2_v,6);
    end
    
    % Change last value to infinity to ease comparison calculations later
    rast1_cats(end) = inf;
    rast2_cats(end) = inf;

end