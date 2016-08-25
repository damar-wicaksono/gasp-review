function [y] = borehole_eval(xx)
%BOREHOLE_EVAL Evaluate the flow rate through a borehole (the borehole func.)
%
%  xx is n-by-8 input parameters matrix
%  y is the n-by-1 vector of output
%
%  The borehole function is 8-dimensional scalar function. The input is the 
%  following and listed as the column of the input matrix:
%   1. rw, radius of the borehole, [m]
%   2. r, radius of influence, [m]
%   3. Tu, transmissivity of upper aquifer, [m^2.year^-1]
%   4. Hu, potentiometric head of upper aquifer, [m]
%   5. Tl, transmissivity of lower aquifer, [m^2.year^-1]
%   6. Hl, potentiometric head of lower aquifer, [m]
%   7. L, length of the borehole, [m]
%   8. Kw, hydraulic conductivity of the borehole, [m.year^-1]
%
%  The output of the function is the flow rate through the borehole in 
%  [m^3.year^-1]
%
% Reference:
% (1) Max D. Morris, Toby J. Mitchell, and Donald Ylvisaker, "Bayesian Design
%     and Analysis of Computer Experiments: Use of Derivatives in Surface 
%     Prediction," Technometrics, vol. 35, no. 3, 1993, pp. 243-255
%
% Assign the input arguments to local variables
rw = xx(:,1);
r  = xx(:,2);
Tu = xx(:,3);
Hu = xx(:,4);
Tl = xx(:,5);
Hl = xx(:,6);
L  = xx(:,7);
Kw = xx(:,8);

% Calculate the flow rate
y = 2 * pi * Tu .* (Hu - Hl) ./ log(r./rw) ./ ...
    (1 + 2 * L .* Tu ./ log(r./rw) ./ rw.^2 ./ Kw + Tu ./ Tl);

end