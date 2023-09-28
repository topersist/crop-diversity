function [SCS_out, glob_sum] = f_box_to_raster_median_results(ref_data,...
    hold_scen_x, hold_scen_y, bool_future_scenario, bool_present_scenario, ...
    hLand, res_raster)

    % initialize return variables to NaN
    SCS_out = NaN;
    glob_sum = NaN;

    %Obtain resilience quantiles, and isolate lowest resilience quantile
    [res_cats, temp] = f_hold_rast_categories(res_raster, res_raster, 4, 4);
    
    % Find areas on map where resilience is i the lowest category, i.e.
    % lowest 25% i the main analysis
    res_rast_boolean = res_raster >= res_cats(1) & res_raster < res_cats(2);

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
    % Reshape the vectorized data to matrix format and categorize low
    % resilience areas
    SCS_out = reshape(SCS_map_v, size(hLand));
    SCS_out(SCS_out == 1 & res_rast_boolean) = 2;

    % Calculate the sum an percentage of the reference data 
    % in the resilience categories
    %  Resilience theshold: 25%

    abs_res = nansum(nansum((SCS_out == 1) .*ref_data));
    abs_nores = nansum(nansum((SCS_out == 2) .*ref_data));

    perc_res = abs_res / nansum(nansum(ref_data));
    perc_nores = abs_nores / nansum(nansum(ref_data));

    perc_median_outside_SCS = perc_res+perc_nores;

    % Finally, create a table of the global aggregates
    glob_sum = [abs_res, abs_nores; perc_res, perc_nores];

end
