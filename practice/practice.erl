%
% My thrid program.
% By Anastasia.
%

-module(practice).
-export([first/0]).

%
% -- Public -- 
%
first() ->
   io:format("This is a sample program!~n"),
   second().

%
% -- Private -- 
%
second() ->
   io:format("End of Program.~n").
   