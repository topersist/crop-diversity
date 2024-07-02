function [aggreg_box_1, aggreg_box_2, aggreg_box_3, bool_limited] = f_100x100_holdridge_box(ref_data_1, ref_data_2, ref_data_3, hold_scen_x, hold_scen_y, perc_limit)

    % aggreg_box_x: 100x100 matrix of data from ref_data_x aggregated to climatic bins
    % bool_limited: boolean version of aggreg_box_1, except that it only
    % contains the highest 'perc_limit' % of aggregated data.
    
    % Create intervals for the 100x100 grid.
    thresholds = linspace(0.0,1.0,101);

    % Set smallest threshold to -inf to  allow logical indexing later
    thresholds(1) = -inf;

    % Vectorize data to allow faster computation
    ref_v_1 = ref_data_1(:);
    ref_v_2 = ref_data_2(:);
    ref_v_3 = ref_data_3(:);
    hold_scen_x_v = hold_scen_x(:); % PET
    hold_scen_y_v = hold_scen_y(:); % prec

    % Aggregate observations of the reference data set to a 100 x 100  holdridge grid (0.01 spacing)
    % 'aggreg_box', based on the holdridge x and y coordinates
    aggreg_box_1 = zeros(100,100);
    aggreg_box_2 = zeros(100,100);
    aggreg_box_3 = zeros(100,100);

    for i = 1:length(thresholds)-1
        for j = 1:length(thresholds)-1
            logical_i_j = hold_scen_x_v > thresholds(i) & ...
                hold_scen_x_v <= thresholds(i+1) & ...
                hold_scen_y_v > thresholds(j) & ...
                hold_scen_y_v <= thresholds(j+1);
            aggreg_box_1(j, i) = sum(ref_v_1(logical_i_j), "omitmissing");
            aggreg_box_2(j, i) = sum(ref_v_2(logical_i_j), "omitmissing");
            aggreg_box_3(j, i)= sum(ref_v_3(logical_i_j), "omitmissing");
        end
    end

    % Check that the sum of the aggregated reference data ('aggreg_box') 
    % is the same as the sum of the reference data set.
    % Should be one if everything is correct.
    %sum(aggreg_box_1(:))/sum(ref_v_1)

    % In the next steps the aim is to find the cells in the grid which contain
    % a certain percentage (e.g. 95%)of the reference data (food crop production).
    % Turn the 100 x 100 'aggreg_box' grid to a vector. 
    aggreg_box_v = aggreg_box_1(:);

    % Sort the values in vectorized 'aggreg_box', from largest to smallest,
    % obtain the sorting numbers and caluclate a cumulative sum.
    [aggreg_box_v_sorted, I] = sort(aggreg_box_v, 'descend');
    aggreg_box_v_perc = cumsum(aggreg_box_v_sorted)/sum(aggreg_box_v_sorted);

    % Create a logical vector with the same size as the 'aggreg_box' vector
    % where everything is 0 (False).
    bool_limited = logical(zeros(size(aggreg_box_v)));

    % In the above defined logical vector, turn those cells that contain a certain
    % percentage (e.g. 95%, defined by perc_limit) of the aggregated reference 
    % data (food crop production) to True.
    bool_limited(I(aggreg_box_v_perc <= perc_limit)) = 1;

    % Reshape the vector to 100x100 grid.
    bool_limited = reshape(bool_limited,100,100);
    %%
end
