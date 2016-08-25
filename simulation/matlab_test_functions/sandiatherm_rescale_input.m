function [yy] = sandiatherm_rescale_input(xx)
%BOREHOLE_RESCALE_INPUT Rescale the normalized input matrix to the actual range
%
%  xx is n-by-4 normalized input parameters matrix
%  yy is n-by-4 rescaled input parameters matrix according to below range
%
%  The thermal problem formulation contains 4 input parameters, 2 of which are
%  the control variables (q and L) and the other two are "model" variables 
%  (k and rho_cp). The range of parameters is as follows:
%
%    1. q, heat flux at the slab surface, [W.m^-2], [1'000, 3'500]
%    2. L, length of the slab, [m], [1.27E-2, 2.54E-2]
%    3. k, thermal conductivity, [W.m^-1.K^-1], [0.0455, 0.0811]
%    4. rho_cp, volumetric heat capacity, [J.m^-3.K^-1], [3.38E5, 4.69E5]
%
%   **Reference:**
%   (1) Kevin J. Dowding, Martin Pilch, and Richard G. Hills, "Formulation of 
%       the Thermal Problem," Computer Methods in Applied Mechanics and 
%       Engineering, vol. 197, 2008, pp. 2385 - 2389
%
% Rescale the inputs
yy(:,1) = 1000. + (3500. - 1000.) * xx(:,1)         % q 
yy(:,2) = 1.27E-2 + (2.54E-2 - 1.27E-2) * xx(:,2)   % L
yy(:,3) = 0.0455 + (0.0811 - 0.0455) * xx(:,3)      % k
yy(:,4) = 3.38E5 + (4.69E5 - 3.38E5) * xx(:,4)      % rho_cp

end