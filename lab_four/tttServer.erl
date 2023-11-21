%
% tttServer.erl
%
-module(tttServer).
-author('Anastasia M. LaPeruta').

-define(else, true).
-define(id, "-- server: ").


%
% Public
%
-export([start/0]).

start() ->
   io:fwrite("~sTTT server started on node ~w (pid ~w) ", [?id, node(), self()]),
   ServerPid = spawn(fun serverLoop/0),
   % We want to publish this process in Erlang's process registry.
   % Before we do that, we need to un-register it if it's already been registered.
   SomePlace = whereis(tttServer),
   if (SomePlace /= undefined) ->  % "not undefined" = "is defined"  (This is horrible, I know.)
      unregister(tttServer);
   ?else ->
      % The proccess was NOT already published/registered, so we don't really want to do anything here.
      true    % This is dumb, but I couldn't think of a better "no-op".
   end,
   % Publish this process in Erlang's process registry.
   register(tttServer, ServerPid),
   io:fwrite("with pid ~w registered as ~w.~n", [ServerPid, tttServer]).


%
% Private, but accepting messages sent to serverLoop because of the way it was spawned.
%
serverLoop() -> receive
                   {FromNode, start_game} ->
                      io:fwrite("~sReceived [start_game] request from node ~w.~n",[?id, FromNode]),
                      io:fwrite("~sSending [player_turn] response to node ~w.~n",[?id, FromNode]),
                      InitialBoard = [0,0,0, 0,-1,0, 0,0,0], % lets computer go first
                      {tttClient, FromNode} ! {node(), player_turn, InitialBoard},
                      serverLoop();

                   {FromNode, process_player_turn, Board, PlayerPos} ->
                      io:fwrite("~sReceived [process_player_turn] request from node ~w with board ~w and player move ~w.~n",[?id, FromNode, Board, PlayerPos]),
                      NewBoard = processPlayerMove(PlayerPos, Board),
                      % Do more stuff here.
                      {tttClient, FromNode} ! {node(), player_turn, NewBoard},
                      serverLoop();

                   {FromNode, computer_turn, Board} ->
                      io:fwrite("~sReceived [computer_turn] request from node ~w with board ~p.~n",[?id, FromNode, Board]),
                      % Do more stuff here.
                      NewBoard = processPlayerMove(computer_turn, Board),
                      {tttClient, FromNode} ! {node(), player_turn, NewBoard},
                      serverLoop();

                   {FromNode, _Any} ->
                      io:fwrite("~sReceived unknown request [~p] from node ~w.~n",[?id, _Any, FromNode]),
                      serverLoop()
                end.


%
% Private (not even accepting messages)
%
processPlayerMove(Position, Board) ->
   Target = lists:nth(Position, Board),
   if(Target == 0) ->
      io:fwrite("~sPlacing an X into position ~w.~n", [?id, Position]),
      UpdatedBoard = replaceInList(1, Position, Board),
      makeMove(UpdatedBoard); % calls function that will decide computer move
   ?else ->
      io:fwrite("~sCannot place an X into position ~w.~n", [?id, Position]),
      Board
   end. % if


makeMove(Board) -> io:fwrite("Calculating computer move...", []),
                   ComputerMove = computeMove(Board), % index of where computer moves
                   io:fwrite("Placing an O into position ~w.~n", [ComputerMove]),
                   replaceInList(-1, ComputerMove, Board).
                   
% unfortunately only way to avoid illegal guard error
computeMove(Board) ->
   % check to see if top row is almost complete
   case {lists:nth(3, Board), Board} of
        {0, [1, 1, _, _, _, _, _, _, _]} -> 3;
        _ -> 
            % check to see if the next condition is met
            case {lists:nth(2, Board), Board} of
                {0, [1, _, 1, _, _, _, _, _, _]} -> 2;
                _ -> 
                    % check to see if the next condition is met
                    case {lists:nth(1, Board), Board} of
                        {0, [_, 1, 1, _, _, _, _, _, _]} -> 1;
                        _ -> 
                            % check to see if bottom row is almost complete
                            case {lists:nth(9, Board), Board} of
                                {0, [_, _, _, _, _, _, 1, 1, _]} -> 9;
                                _ -> 
                                    % check to see if the next condition is met
                                    case {lists:nth(8, Board), Board} of
                                        {0, [_, _, _, _, _, _, 1, _, 1]} -> 8;
                                        _ -> 
                                            % check to see if the next condition is met
                                            case {lists:nth(7, Board), Board} of
                                                {0, [_, _, _, _, _, _, _, 1, 1]} -> 7;
                                                _ -> 
                                                    % check to see if left column is almost complete
                                                    case {lists:nth(7, Board), Board} of
                                                        {0, [1, _, _, 1, _, _, _, _, _]} -> 7;
                                                        _ -> 
                                                            % check to see if the next condition is met
                                                            case {lists:nth(4, Board), Board} of
                                                                {0, [1, _, _, _, _, _, 1, _, _]} -> 4;
                                                                _ -> 
                                                                    % check to see if the next condition is met
                                                                    case {lists:nth(1, Board), Board} of
                                                                        {0, [_, _, _, 1, _, _, 1, _, _]} -> 1;
                                                                        _ -> 
                                                                            % check to see if right column is almost complete
                                                                            case {lists:nth(9, Board), Board} of
                                                                                {0, [_, _, 1, _, _, 1, _, _, _]} -> 9;
                                                                                _ -> 
                                                                                    % check to see if the next condition is met
                                                                                    case {lists:nth(6, Board), Board} of
                                                                                        {0, [_, _, 1, _, _, _, _, _, 1]} -> 6;
                                                                                        _ -> 
                                                                                            % check to see if the next condition is met
                                                                                            case {lists:nth(3, Board), Board} of
                                                                                                {0, [_, _, _, _, _, 1, _, _, 1]} -> 3;
                                                                                                _ -> findFirst(0, Board)
                                                                                            end
                                                                                    end
                                                                            end
                                                                    end
                                                            end
                                                    end
                                            end
                                    end
                            end
                    end
            end
   end.

% rest of your code remains unchanged



% helper function to determine if spot is empty or not
checkMove(Board, Position) ->
    case lists:nth(Position, Board) of
        0 -> true;
        _ -> false
    end.




findFirst(Target, [Head | Tail]) when (Target == Head) -> 1;                                % This is ugly because if the Target is never found in the list
findFirst(Target, [Head | Tail]) when (Target /= Head) -> 1 + findFirst(Target, Tail);      % this function will return length(List)+1. At least it's not a
findFirst(Target, [])                                  -> 1. 

replaceInList(Value, Position, List) -> {Part1, Part2} = lists:split(Position-1, List),     % Break the list in two just before the specified Position.
                                        [Head | Tail] = Part2,                              % Separate Part2 into Head and Tail, discarding the Head.
                                        Part1 ++ [Value] ++ Tail.                           % Cons together the result: Part1 ++ the new Value ++ the Tail from Part2.




