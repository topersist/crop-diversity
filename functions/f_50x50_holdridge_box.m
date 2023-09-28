function [aggreg_box, bool] = f_50x50_holdridge_box(ref_data, hold_scen_x, hold_scen_y, perc_limit)

    % Create intervals for the 50x50 grid.
    thresholds = linspace(0.0,1.0,51);

    % Set smallest threshold to -inf to  allow logical indexing later
    thresholds(1) = -inf;


    % Vectorize data to allow faster computation
    ref_v = ref_data(:);
    hold_scen_x_v = hold_scen_x(:);
    hold_scen_y_v = hold_scen_y(:);

    % Aggregate observations of the reference data set to a 50 x 50  holdridge grid (0.01 spacing)
    % 'aggreg_box', based on the holdridge x and y coordinates
    aggreg_box = zeros(50,50);
    for i = 1:length(thresholds)-1
        for j = 1:length(thresholds)-1
            logical_i_j = hold_scen_x_v > thresholds(i) & ...
                hold_scen_x_v <= thresholds(i+1) & ...
                hold_scen_y_v > thresholds(j) & ...
                hold_scen_y_v <= thresholds(j+1);
            aggreg_box(j,i) = sum(ref_v(logical_i_j));
        end
    end

    % Check that the sum of the aggregated reference data ('aggreg_box') 
    % is the same as the sum of the reference data set.
    % Should be one if everything is correct.
    sum(aggreg_box(:))/sum(ref_v)

    % In the next steps the aim is to find the cells in the grid which contain
    % a certain percentage (e.g. 95%)of the reference data (livestock or food crop production).
    % Turn the 50 x 50 'aggreg_box' grid to a vector. 
    aggreg_box_v = aggreg_box(:);

    % Sort the values in vectorized 'aggreg_box', from largest to smallest,
    % obtain the sorting numbers and caluclate a cumulative sum.
    [aggreg_box_v_sorted I] = sort(aggreg_box_v, 'descend');
    aggreg_box_v_perc = cumsum(aggreg_box_v_sorted)/sum(aggreg_box_v_sorted);

    % Create a logical vector with the same size as the 'aggreg_box' vector
    % where everything is 0 (False).
    bool = logical(zeros(size(aggreg_box_v)));

    % In the above defined logical vector, turn those cells that contain a certain
    % percentage (e.g. 95%, defined by perc_limit) of the aggregated reference 
    % data (livestock or food crop production) to True.
    bool(I(aggreg_box_v_perc <= perc_limit)) = 1;

    % Reshape the vector to 50x50 grid.
    bool = reshape(bool,50,50);
    
end
