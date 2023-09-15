% 
% Factorial(s)
% By Anya
%

-module(facs).
-export([facs1/1]).

% 
% -- Public --
%
facs1(N) -> N * facs1(N-1);
facs1(0) -> 1.
