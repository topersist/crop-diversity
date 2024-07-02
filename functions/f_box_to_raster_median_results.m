function [SCS_out, glob_sum] = f_box_to_raster_median_results(ref_data,...
    hold_scen_x, hold_scen_y, bool_future_scenario, bool_present_scenario, ...
    hLand)

    %% for testing
    % ref_data = ref_95_prcnt_pres;
    % hold_scen_x = hold_map_present(:,:,2);
    % hold_scen_y = hold_map_present(:,:,3);
    % bool_future_scenario = bool_box_present_nolim;
    % bool_present_scenario = bool_box_present_limited;
    % hLand = hLand;

    %%

    % initialize return variables to NaN
    SCS_out = NaN;
    glob_sum = NaN;

    % Initialize grid intervals.
    thresholds = linspace(0.0,1.0,101);
    thresholds(1) = -inf;

    % Vectorize data to allow faster computation
    land_v = hLand(:);
    SCS_map_v = zeros(size(land_v));

    hold_scen_x = hold_scen_x(:);
    hold_scen_y = hold_scen_y(:);

%%
    % Find those areas, which fall outside scs, i,e. areas projected
    % to be within holdridge intervals where where there is reference data
    % in the median GCM scenario but not in the present day reference scenario,
    % and turn those values to one.
    for i = 1:length(thresholds)-1
        for j = 1:length(thresholds)-1

            if bool_present_scenario(j,i) == 0 && bool_future_scenario(j,i) == 1

                logical_i_j = hold_scen_x > thresholds(i) & ...
                    hold_scen_x <= thresholds(i+1) & ...
                    hold_scen_y > thresholds(j) & ...
                    hold_scen_y <= thresholds(j+1);        

                SCS_map_v(logical_i_j) = 1;
            end

        end
    end
%%
    % Reshape the vectorized data to matrix format
    SCS_out = reshape(SCS_map_v, size(hLand));

    % Calculate the sum an percentage of the reference data 
    % outside and within SCS

    abs_outside = sum(sum((SCS_out == 1) .*ref_data, 'omitnan'), 'omitnan');
    abs_within = sum(sum((SCS_out == 0) .*ref_data, 'omitnan'), 'omitnan');

    perc_outside = abs_outside / sum(sum(ref_data, 'omitnan'), 'omitnan');
    perc_within = abs_within / sum(sum(ref_data, 'omitnan'), 'omitnan');

    % Finally, create a table of the global aggregates
    glob_sum = [abs_outside, abs_within; perc_outside, perc_within];

end
