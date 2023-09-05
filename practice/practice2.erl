%
% My fourth program.
% By Anastasia.
%

-module(practice2).
-export([start/0]).

%
% -- Public -- 
%
start() ->
   io:format("Beginning of program!~n"),
   ending().

%
% -- Private -- 
%
ending() ->
   io:format("Shutting down...~n").
   