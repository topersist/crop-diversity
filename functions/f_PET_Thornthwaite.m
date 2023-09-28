function [PE] = f_PET_Thornthwaite(Tave,lat)

% test
% Tave = data_tmean_monthly;
% lat = mlat;


PE = Tave .* NaN;
n = size(Tave,3);
%m = ncol(Tave)
%c <- cycle(Tave)
tanLat = tan(lat ./ 57.2957795);
tanDelta(1,1,:) = [-0.37012566, -0.23853358, -0.04679872, 0.16321764, ...
        0.32930908, 0.40677729, 0.3747741, 0.239063, 0.04044485, ...
        -0.16905776, -0.33306377, -0.40743608];
tanLatM = repmat(tanLat, 1,1,n);
tanLatM = tanLatM .* repmat(tanDelta,size(Tave,1),size(Tave,2),1);
    
tanLatM(tanLatM < -1) = -1;
tanLatM(tanLatM > 1) = 1;

omega = acos(-tanLatM);

N = 24 * omega/pi();

days(1,1,:) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
mdays = repmat(days,size(Tave,1),size(Tave,2),1);
K = N/12 .* mdays/30;

T = Tave;
T(T < 0) = 0;
%imagesc(T)

J = nansum((T/5).^1.514,3);
J = repmat(J, 1, 1, n);
J2 = J .* J;
J3 = J2 .* J;

q = 6.75e-07 * J3 - 7.71e-05 * J2 + 0.01792 * J + 0.49239;

Tave(Tave < 0) = 0;

PE = K .* 16 .* ((10 .* Tave ./ J).^q);

end
    
  