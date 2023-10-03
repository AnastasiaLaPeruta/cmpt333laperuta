%
% Lab Two - Recursion
% By Anastasia.
%

-module(lab_two).
-export([list_of_lists/3]).


list() -> [].

list_of_lists(0,Num_elements,Value_inc) -> io:fwrite("also working"); % goes first so it catches base case of 0
list_of_lists(Num_lists, Num_elements, Value_inc) -> 
    list()++[list_of_lists(Num_lists-1, Num_elements, Value_inc)]. %subtract 1 each time with recursion, was [Num_lists] b4


%
% -- Public -- 
%

%listoflists(valueInc,numLists,numElements) -> makeSmallerList(valueInc,numLists,numElements),
%listoflists(valueInc,numLists-1,numElements);
%listoflists(_,0,_) -> io:fwrite("~w",[finalList]). % base case

%
% -- Private --
%

%newList(valueInc,numLists,numElements) -> [].
%increasingNum() -> numLists.
%makeSmallerList(valueInc,numLists,numElements) -> newList(valueInc,numLists,numElements) ++ [increasingNum()],
%makeSmallerList(valueInc,increasingNum,numElements-1);
%makeSmallerList(_,_,0) -> newList(valueInc,numLists,numElements). % base case

%attempt 2

%list() -> [].
%listoflists([Head|Tail]) -> determineNum(list) ++ []


%determineNum(num) when is_integer(num) -> num + m;
%determineNum(_) -> io:fwrite("Not a valid value").


   