%
% Lab Two - Recursion
% By Anastasia.
%

-module(lab_two).
-export([list_of_lists/2]).


%
% -- Public -- 
%

% case for when input is non-negative
list_of_lists(Seq_length,Number) when Seq_length > 0, Number > 0 ->
    list_of_lists(Seq_length, Number, [], Number).

% reverses list since elements are prepended
list_of_lists(_,_,Result,0) ->
    lists:reverse(Result);
% calls private function to generate each inner list
list_of_lists(Seq_length,Number,Result,Counter) when Counter > 0 ->
    Inner_list = inner_list(Seq_length, Number, Counter, Seq_length),
    list_of_lists(Seq_length, Number, [Inner_list|Result], Counter - 1).


%
% -- Private--
%

inner_list(_, _, 0,_) ->
    [];

% once each element is prepended, the list gets reversed
inner_list(Seq_length,Number,Counter,Start) ->
    Value = Start - (Counter - 1) * Number,
    [Value|inner_list(Seq_length,Number,Counter-1,Start)].