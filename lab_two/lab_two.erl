%
% Lab Two - Recursion
% By Anastasia.
%

-module(lab_two).
-export([listoflists/3]).

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
list() -> [].
listoflists([Head|Tail]) -> determineNum(list) ++ []


determineNum(num) when is_integer(num) -> num + m;
determineNum(_) -> io:fwrite("Not a valid value").


   