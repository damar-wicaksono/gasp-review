function [yy] = sandiatherm_eval(xx, t, x, temp_init)
%SANDIATHERM_EVAL Evaluate the transient temperature at select time and pos.
%
%  xx is an n-by-4 input parameters matrix containing in sequential 
%   column: q, L, k, and rho_cp. n is the number of independent realization
%  t is the time-points array (t) in [s] where the temperature is evaluated
%  x is the lateral location (x) in [m] where the temperature is evaluated
%  temp_init is the initial temperature (temp_init) in [K]
%
%  The Sandia Lab. thermal problem is based on a truncated infinite series 
%  solution of heat conduction problem. It contains in total 7 free parameters 
%  described below. The first 4 are considered input parameters:
%
%   1. q, heat flux at the slab surface, [W.m^-2], [1'000, 3'500]
%   2. L, length of the slab, [m], [1.27E-2, 2.54E-2]
%   3. k, thermal conductivity, [W.m^-1.K^-1], [0.0455, 0.0811]
%   4. rho_cp, volumetric heat capacity, [J.m^-3.K^-1], [3.38E5, 4.69E5]
%
%  The output of the function is n-by-size(t,1) temperature prediction in [K]
%  at each select time points for each input parameters in xx
%
% Reference:
%   (1) Kevin J. Dowding, Martin Pilch, and Richard G. Hills, "Formulation of 
%       the Thermal Problem," Computer Methods in Applied Mechanics and 
%       Engineering, vol. 197, 2008, pp. 2385 - 2389
%
% Assign the input arguments to local variables
q = xx(:,1);
L  = xx(:,2);
k = xx(:,3);
rho_cp = xx(:,4);

% Create an empty array
n_s = size(xx, 1);
n_t = size(t, 2);
yy = zeros(n_s, n_t);

% Loop over time-points and evaluate the temperature
for idx = 1:numel(t)

    if t(idx) < 1E-6
        yy(:,1) = temp_init;
    else
        series_sum = 0;
        for j = 1:7
            series_sum = series_sum + 1/j^2 * ...
                    exp(-1 * j^2 * pi^2 * k ./ rho_cp .* t(idx) ./ L.^2) .* ...
                    cos(j * pi * x ./ L);
        end
        series_sum = series_sum * 2 / pi^2;

        % Compute the temperature
        yy(:,idx) = temp_init + q .* L ./ k .* ... 
            (k ./ rho_cp * t(idx) ./ L.^2 + 1.0/3.0 - x ./ L + ...
            0.5 * (x ./ L).^2 - series_sum);
    end
end

end