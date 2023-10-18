% lab_three.erl - Adventure Game Server

-module(lab_three).
-author('Anastasia M. Labouseur').

%--------
% Public 
%--------

-export([gameLoop/0]).

gameLoop() -> 
  receive
     {FromPid, 0, L}     -> io:format("~p is standing in front of Hancock carrying ~w.~n", [FromPid, L]),
                            gameLoop();

     {FromPid, 1, L}     -> io:format("~p has come to the baseball field and encounters a wild fox. Current inventory includes ~w.~n", [FromPid, L]),
                            gameLoop();

     {FromPid, 2, L}     -> io:format("~p is standing along the edge of the Hudson River and falls in. Current inventory includes ~w.~n", [FromPid, L]),
                            gameLoop();           

     {FromPid, 3, L}     -> io:format("~p is stranded on top of the Dyson Center construction site. Current inventory includes ~w.~n", [FromPid, L]),
                            gameLoop();    

     {FromPid, 4, L}     -> io:format("~p is sitting in the Cannavino Library carrying ~w.~n", [FromPid, L]),
                            gameLoop();    

     {FromPid, 5, L}     -> io:format("~p is in the dining hall carrying ~w.~n", [FromPid, L]),
                            gameLoop();                                                                             

     {FromPid, Other, L} -> io:format("~p is lost in unknown locale: ~p. Perhaps one of these items will help:~w.~n", [FromPid, Other, L]),
                            gameLoop()
  end.
