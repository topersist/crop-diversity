function [ET0] = f_PET_hargreaves(Tave,Tmin,Tmax,lat, Pre)

% test
% Tave = data_tmean_monthly;
% Tmin = data_tmin_monthly;
% Tmax = data_tmax_monthly;
% lat = mlat;
% Pre = data_pr_monthly;


ET0 = Tave .* NaN;
n = size(Tave,3);
c = 12;
T = Tave;
Tr = Tmax - Tmin;
Tr(Tr < 0) = 0;

J = repmat(single(30.5 * c - 14.6),size(Tave,1),size(Tave,2),n);

delta = 0.409 * sin(0.0172 .* J - 1.39);
dr = 1 + 0.033 * cos(0.0172 * J);
latr = lat/57.2957795;
sset = -tan(latr) .* tan(delta);
omegas = sset .* 0;
omegas(sset >= -1 & sset <= 1) = acos(sset(sset >= -1 & sset <= 1));

for i = 1:n
    temp_omegas = omegas(:,:,i);
    
    temp_omegas(sset(:,:,i) < -1) = nanmax(nanmax((temp_omegas)));
    omegas(:,:,i) = temp_omegas;
end
Ra = 37.6 .* dr .* (omegas .* sin(latr) .* sin(delta) + cos(latr) .* cos(delta) .* sin(omegas));
Ra(Ra<0) = 0;

ET0 = 0.0023 .* 0.408 .* Ra .* (T + 17.8) .* Tr.^0.5;


% in case of prec data
%         ab = Tr - 0.0123 * Pre;
%         ET0 = 0.0013 .* 0.408 .* Ra .* (T + 17) .* ab.^ 0.76;
%         ET0(isnan(ab.^0.76)) = 0;

days(1,1,:) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
mdays = repmat(days,size(Tave,1),size(Tave,2),1);
ET0 = mdays .* ET0;
ET0(ET0<0) = 0;

end

