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
    Start_num = Counter, 
    sm_list() ++ [Start_num], % adds starting number before it gets to function that will increment it
    inner_list(sm_list, Start_num, Counter, Element). % starts recursive call for inner list



%
% -- Private--
%

inner_list(_,_,_,0) -> final_list(sm_list, Start_num - 1, Counter); % base case for when elements of list have all been inserted

% decrease element left by one each time and recursively calls itself to add the next number to the small list
inner_list(sm_list, Start_num, Counter, Element) -> sm_list() ++ [inner_list(sm_list, Start_num + Counter, Counter, Element - 1)].


% recursive function that will add all of the smaller lists into one large one
final_list(sm_list, 0, Counter) -> io:fwrite(big_list() ++ [sm_list],"~w"); % base case for when the correct amount of lists has been added
final_list(sm_list, Start_num, Counter) -> io:fwrite("This second one works").
