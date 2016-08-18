function [yy] = borehole_rescale_input(xx)
%BOREHOLE_RESCALE_INPUT Rescale the normalized input matrix to the actual range
%
%  xx is n-by-8 normalized input parameters matrix
%  yy is n-by-8 rescaled input parameters matrix according to below range
%
%  The range of input parameter of parameter 8 are taken from [1] where it 
%  was modified from the original range in [2] to induce more non-linearities
%  and non-additivity to the output. The range of the parameters is as follow:
%
%   1. rw, radius of the borehole, [m], [0.05, 0.15]
%   2. r, radius of influence, [m], [100, 50'000]
%   3. Tu, transmissivity of upper aquifer, [m^2.year^-1], [63'070, 115'600]
%   4. Hu, potentiometric head of upper aquifer, [m], [990, 1'110]
%   5. Tl, transmissivity of lower aquifer, [m^2.year^-1], [63.1, 116]
%   6. Hl, potentiometric head of lower aquifer, [m], [700, 820]
%   7. L, length of the borehole, [m], [1'120, 1'680]
%   8. Kw, hydraulic conductivity of the borehole, [m.year^-1],[1'500, 15'000]
%
% References:
% (1) Max D. Morris, Toby J. Mitchell, and Donald Ylvisaker, "Bayesian Design
%     and Analysis of Computer Experiments: Use of Derivatives in Surface 
%     Prediction," Technometrics, vol. 35, no. 3, 1993, pp. 243-255
% (2) Brian A. Worley, "Deterministic Uncertainty Analysis," in Proc. of 
%     the American Nuclear Society Winter Meeting, Los Angeles, USA, 1987
%
% Rescale the inputs
yy(:,1) = 0.05 + (0.15 - 0.05) * xx(:,1);        % rw
yy(:,2) = 100. + (50000. - 100.) * xx(:,2);      % r
yy(:,3) = 63070. + (115600. - 63070.) * xx(:,3); % Tu
yy(:,4) = 990. + (1110. - 990.) * xx(:,4);       % Hu
yy(:,5) = 63.1 + (116. - 63.1) * xx(:,5);        % Tl
yy(:,6) = 700. + (820. - 700.) * xx(:,6);        % Hl
yy(:,7) = 1120. + (1680. - 1120.) * xx(:,7);     % L
yy(:,8) = 1500. + (15000. - 1500.) * xx(:,8);    % Kw

end