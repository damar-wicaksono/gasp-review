function [y] = cos_dampened(x)
%COS_DAMPENED The 1D dampened cosine test function
%
%
% Reference:
%   (1) T. Santner, B. Williams, and W. Notz, "The Design and Analysis of
%   Computer Experiments," pp. 64, Springer-Verlag, New York, 2003.
%
y = exp(-1.4 * x) .* cos(3.5 * pi * x);

end