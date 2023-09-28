function ref_masked = f_hold_mask_ref_raster(ref_data, hold_scen_present_x, hold_scen_present_y, bool_present_scenario)

    % Initialize grid intervals.
    thresholds = linspace(0.0,1.0,101);
    thresholds(1) = -inf;

    % Vectorize data to allow faster computation
    ref_v = ref_data(:);
    ref_masked_v = zeros(size(ref_v));
    hold_scen_present_x = hold_scen_present_x(:);
    hold_scen_present_y = hold_scen_present_y(:);

    % Allocate value 1 to those areas, where there's crop production 
    % in the present day holdridge condition defined in
    % 'bool_present_scenario'
    for i = 1:length(thresholds)-1
        for j = 1:length(thresholds)-1

            if bool_present_scenario(j,i) == 1

                logical_i_j = hold_scen_present_x > thresholds(i) & ...
                    hold_scen_present_x <= thresholds(i+1) & ...
                    hold_scen_present_y > thresholds(j) & ...
                    hold_scen_present_y <= thresholds(j+1);        

                ref_masked_v(logical_i_j) = 1;                    

            end
        end
    end

    % Change vectorized data back to matrix shape
    ref_masked = reshape(ref_masked_v, size(ref_data));
    ref_masked = logical(ref_masked);

    % Check that aggregates are as they should (only works for 95% limit)
    %should_be_1 = nansum(nansum(ref_masked.*ref_data)) / (0.95*nansum(ref_data(:)));

end





