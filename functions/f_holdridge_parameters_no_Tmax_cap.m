
function [data_out] = f_holdridge_parameters_no_Tmax_cap(data_in, bias, gs_filter)

%% test
 %data_in = temp_data;
 %bias = bias_correction_tavg;
 %gs_filter = gs_filter;
%%

if isnan(max(data_in, [], 'all'))
    % if variables are missing, output is NaN as well
    data_out = NaN(2160,4320,4);
else
    %%
    % annual prec
    data_pr_annual = single(sum(data_in(:,:,:,3).*gs_filter,3, "omitnan"));
    % for areas with prec == 0
    data_pr_annual(data_pr_annual == 0) = 1;
    
    % monthly mean prec
    data_pr_monthly = single(squeeze(data_in(:,:,1:12,3).*gs_filter));
  
    
    % monthly min temperature
    data_tmin_monthly = single(data_in(:,:,1:12,1).*gs_filter);
    % monthly max temp
    data_tmax_monthly = single(data_in(:,:,1:12,2).*gs_filter);
    % monthly mean temperature
    data_tmean_monthly = single(squeeze(mean(data_in(:,:,1:12,1:2).*gs_filter,4, "omitmissing")));
    % bias correction to measured tavg
    data_tmean_monthly = bias .* data_tmean_monthly;
    
    %imagesc(data_tmean_monthly(:,:,6));
    % biotemperature normal (range from 0-35°C)
    range_min = 0;
    range_max = 35;
    
    % all temperature above range_max to range_max
    data_tmean_monthly(data_tmean_monthly > range_max) = range_max;
    
    
    for i = 1:12
        temp_tbio = data_tmean_monthly(:,:,i) > range_min;
        tbio_boolean(:,:,i) = temp_tbio;
        clearvars temp_tbio;
    end
    %%
    % 
    % for i = 1:12
    %     temp_tbio = and(data_tmean_monthly(:,:,i) > range_min,data_tmean_monthly(:,:,i) < range_max);
    %     tbio_boolean(:,:,i) = temp_tbio;
    %     clearvars temp_tbio;
    % end
    
    tbio_month = tbio_boolean .* data_tmean_monthly;
    tbio = squeeze(sum(tbio_month,3, "omitmissing"))./12;
    
    % PET
    % define latitude
    lat = [-90:1/12:90];
    mlat = repmat(-lat(1,1:2160)',1,360*12);
    %imagesc(mlat);
    
    % data_PET_h = f_PET_hargreaves(data_tmean_monthly,data_tmin_monthly,data_tmax_monthly,mlat, data_pr_monthly);
    % data_PET_h_annual = real(nansum(data_PET_h,3));
    % data_PET_t = f_PET_Thornthwaite(data_tmean_monthly,mlat);
    % data_PET_t_annual = nansum(data_PET_t,3);
    
    % data_PET_annual = (data_PET_h_annual + data_PET_t_annual)./2;
    
    % PET 
    data_PET_org_hold = single(median(tbio,3) .* 58.93);
    
    
    
    % frost free days
    daysOfMonth = eomday(1990, 1:12);
    noFrostDays = zeros(2160,4320,12,'single');
    for month = 1:12
        temp_min = data_in(:,:,month,1).*gs_filter(:,:,month);
        temp_max = data_in(:,:,month,2).*gs_filter(:,:,month);
        
        temp_noFrostDays = zeros(2160,4320,'single');
        
        % min temperature larger than zero, all days no frost
        temp_mask_above = temp_min > 0;
        temp_noFrostDays(temp_mask_above) = daysOfMonth(1,month);
        
    %     % max temp below zero, no days 'no frost'
    %     temp_mask_below = temp_max < 0;
    %     temp_noFrostDays(temp_mask_below) = 0;
    %     
    %     % else
    %     temp_mask_else = and(temp_mask_above == 0, temp_mask_below == 0);
    %     
    %     temp_range = temp_max - temp_min;
    %     temp_perc = abs(temp_min ./ temp_range);
    %     temp_else = floor(daysOfMonth(1,month) .* (1-temp_perc));
    %     temp_noFrostDays(temp_mask_else) =  temp_else(temp_mask_else);
    %     
        noFrostDays(:,:,month) = single(temp_noFrostDays);
        clearvars temp*
    end
    
    temp_noFrost = nansum(noFrostDays,3);
    Fmean = temp_noFrost > 364.5;
    
    clearvars temp* daysOfMonth noFrostDays
    %% combine data
    % combine data
    % PET ratio
    %pet_ratio_hold = single(median(tbio,3) .* 58.93)./median(data_pr_annual,3);
    %pet_ratio_new_t = single(median(data_PET_t_annual,3))./median(data_pr_annual,3);
    %pet_ratio_new_h = single(median(data_PET_h_annual,3))./median(data_pr_annual,3);
    
    % 
    % figure; imagesc(pet_ratio_new_h);
    % figure; imagesc(data_PET_h_annual);
    % figure; imagesc(single(median(tbio,3) .* 58.93));
    
    %data_out(:,:,1) = single(median(data_PET_annual,3))./median(data_pr_annual,3);
    data_out(:,:,1) = data_PET_org_hold./median(data_pr_annual,3);
    % annual precipitation
    data_out(:,:,2) = median(data_pr_annual,3);
    % biotemperature 
    data_out(:,:,3) = median(tbio,3);
    % frost threshold
    data_out(:,:,4) = Fmean;
end

end