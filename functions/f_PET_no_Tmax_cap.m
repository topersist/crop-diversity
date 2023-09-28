
function [data_PET_out] = f_PET_no_Tmax_cap(data_in,bias)

%% test
% data_in = temp_data;
% bias = bias_correction_tavg;
%%

% annual prec
data_pr_annual = single(squeeze(nansum(data_in(:,:,1:12,3),3)));
% for areas with prec == 0
data_pr_annual(data_pr_annual == 0) = 1;

% monthly mean prec
data_pr_monthly = single(squeeze(data_in(:,:,1:12,3)));

% monthly min temperature
data_tmin_monthly = single(data_in(:,:,1:12,1));
% monthly max temp
data_tmax_monthly = single(data_in(:,:,1:12,2));
% monthly mean temperature
data_tmean_monthly = single(squeeze(nanmean(data_in(:,:,1:12,1:2),4)));
% bias correction to measured tavg
data_tmean_monthly = bias .* data_tmean_monthly;

%imagesc(data_tmean_monthly(:,:,6));
% biotemperature normal (range from 0-35°C)
range_min = 0;
range_max_30 = 30;
range_max_35 = 35;

% original holdridge tbio
for i = 1:12
    temp_tbio_30 = and(data_tmean_monthly(:,:,i) > range_min,data_tmean_monthly(:,:,i) < range_max_30);
    tbio_boolean_30(:,:,i) = temp_tbio_30;
    clearvars temp_tbio;
end

tbio_month_30 = tbio_boolean_30 .* data_tmean_monthly;
tbio_30 = squeeze(nansum(tbio_month_30,3))./12;


% modified holdridge tbio
% all temperature above range_max to range_max
data_tmean_monthly_35 = data_tmean_monthly;
data_tmean_monthly_35(data_tmean_monthly > range_max_35) = range_max_35;

for i = 1:12
    temp_tbio_35 = data_tmean_monthly(:,:,i) > range_min;
    tbio_boolean_35(:,:,i) = temp_tbio_35;
    clearvars temp_tbio;
end

tbio_month_35 = tbio_boolean_35 .* data_tmean_monthly;
tbio_35 = squeeze(nansum(tbio_month_35,3))./12;



% PET
% PET holdridge
data_PET_org_hold = single(median(tbio_30,3) .* 58.93);
data_PET_mod_hold = single(median(tbio_35,3) .* 58.93);


% other PET methods
% define latitude
lat = [-90:1/12:90];
mlat = repmat(-lat(1,1:2160)',1,360*12);

data_PET_h = f_PET_hargreaves(data_tmean_monthly,data_tmin_monthly,data_tmax_monthly,mlat, data_pr_monthly);
data_PET_h_annual = real(nansum(data_PET_h,3));
data_PET_t = f_PET_Thornthwaite(data_tmean_monthly,mlat);
data_PET_t_annual = nansum(data_PET_t,3);

% data_PET_annual = (data_PET_h_annual + data_PET_t_annual)./2;





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

% PET org
data_PET_out(:,:,1) = data_PET_org_hold;
% PET modified holdridge
data_PET_out(:,:,2) = data_PET_mod_hold;
% PET hargreaves
data_PET_out(:,:,3) = data_PET_h_annual;
% PET Thornthwaite
data_PET_out(:,:,4) = data_PET_t_annual;


end