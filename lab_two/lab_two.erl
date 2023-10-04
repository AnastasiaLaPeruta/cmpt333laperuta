%
% Lab Two - Recursion
% By Anastasia.
%

-module(lab_two).
-export([start_off/2, sm_list/0, big_list/0]).


%
% -- Public -- 
%

% sets up empty lists for inner and entire list so we can add to it as we go
sm_list()-> [].
big_list() -> [].

% this public function is meant to grab straightforward information and start up our private functions
% which will do all the heavy lifting
start_off(Counter, Element) -> 
    % assigns variables that will be passed to the recursive function 
    Default = Element, % allows final_list to access to reset
    Start_num = Counter, 
    inner_list(sm_list() ++ [Start_num], Start_num, Counter, Element, Default). % starts recursive call for inner list



%
% -- Private--
%

inner_list(smlist, Start_num, Counter, 0, Default) -> final_list(smlist, Start_num - 1, Counter, 0, Default); % base case for when elements of list have all been inserted

% decrease element left by one each time and recursively calls itself to add the next number to the small list
inner_list(smlist, Start_num, Counter, Element, Default) -> smlist ++ [inner_list(smlist, Start_num + Counter, Counter, Element - 1, Default)].


% recursive function that will add all of the smaller lists into one large one
final_list(smlist, 0, Counter, Element, Default) -> io:fwrite(big_list() ++ [smlist],"~w"); % base case for when the correct amount of lists has been added
final_list(smlist, Start_num, Counter, Element, Default) -> big_list() ++ [smlist], % adds small list to the big list
inner_list([Start_num - 1], Start_num - 1, Counter, Element, Default). % resets small list and adds first value
