%
% tttClient.erl
% Assistance with algorithm design provided by ChatGPT from OpenAI.
%

-module(tttClient).
-author('Anastasia M. LaPeruta').

-define(else, true).
-define(id, "-- client: ").


%
% Public
%
-export([start/0, play/0, play/1]).

start() ->
   io:fwrite("~sTTT client started on node ~w (pid ~w) ", [?id, node(), self()]),
   ClientPid = spawn(fun clientLoop/0),
   % We want to publish this process in Erlang's process registry.
   % Before we do that, we need to un-register it if it's already been registered.
   SomePlace = whereis(tttClient),
   if (SomePlace /= undefined) ->  % "not undefined" = "is defined"  (This is horrible, I know.)
      unregister(tttClient);
   ?else ->
      % The proccess was NOT already published/registered, so we don't really want to do anything here.
      true    % This is dumb, but I couldn't think of a better "no-op".
   end,
   % Publish this process in Erlang's process registry.
   register(tttClient, ClientPid),
   io:fwrite("with pid ~w registered as ~w.~n", [ClientPid, tttClient]).


play() ->
   io:fwrite("You must supply a node.~n", []).

play(ServerNode) ->
   io:fwrite("~sSending [start_game] request to node ~w.~n",[?id, ServerNode]),
   {tttServer, ServerNode} ! {node(), start_game}.


%
% Private, but accepting messages sent to clientLoop because of the way it was spawned.
%
clientLoop() ->
    receive
        {FromNode, player_turn, Board} ->
            io:fwrite("~sReceived [player_turn] request from node ~w with board.~n", [?id, FromNode]),
            displayBoard(Board),
            io:fwrite("~s", [?id]),
            checkForWin(Board), % win must be checked first
            checkForTie(Board), % before asking for move
            io:fwrite("~s", [?id]),
            {ok, PlayerMove} = getValidInput(),
            io:fwrite("~sSending [process_player_turn] response to node ~w with board ~w and player move ~w.~n", [?id, FromNode, Board, PlayerMove]),
            {tttServer, FromNode} ! {node(), process_player_turn, Board, PlayerMove},
            clientLoop();
        {FromNode, _Any}  ->
            io:fwrite("~sReceived unknown request [~p] from node ~w.~n", [?id, _Any, FromNode]),
            clientLoop()
    end.

% handles all bad input
getValidInput() ->
    try io:fread("Where do you want to move [1-9]? ", "~d") of
        {ok, [Input]} ->
            if 
                Input >= 1, Input =< 9 ->
                    {ok, Input};
                true ->
                    io:fwrite("Bad input. Try again. ~n"),
                    getValidInput()
            end;
        {error, {fread, integer}} ->
            io:fwrite("Bad input. Try again.~n"),
            getValidInput()
    catch
        _:_ ->
            {error, invalid_input}
    end.
%
% Private; no messages either.
%

% displays board in a nicer format
displayBoard(Board) -> io:fwrite(" ~s | ~s | ~s ~n", [getDisplay(Board,1), getDisplay(Board,2), getDisplay(Board,3)] ),
                    io:fwrite("---+---+---~n", []),
                    io:fwrite(" ~s | ~s | ~s ~n", [getDisplay(Board,4), getDisplay(Board,5), getDisplay(Board,6)] ),
                    io:fwrite("---+---+---~n", []),
                    io:fwrite(" ~s | ~s | ~s ~n", [getDisplay(Board,7), getDisplay(Board,8), getDisplay(Board,9)] ).

% used to populate board
getDisplay(Board,Position) -> case lists:nth(Position, Board) of
                                -1 -> ["O"];
                                 0 -> [" "];
                                 1 -> ["X"]
                              end.

checkForTie(UpdatedBoard) ->
    case lists:member(0, UpdatedBoard) of
        true ->
            ok;
        false ->
            io:fwrite("There has been a tie. GAME OVER."),
            exit(normal)
    end.



checkForWin(Board) ->
   case Board of
      % top row win
      [-1,-1,-1,_,_,_,_,_,_] -> io:fwrite("Computer wins!"),
      exit(normal);
      [1,1,1,_,_,_,_,_,_] -> io:fwrite("Player wins!"),
      exit(normal);
      % middle row win
      [_,_,_,-1,-1,-1,_,_,_] -> io:fwrite("Computer wins!"),
      exit(normal);
      [_,_,_,1,1,1,_,_,_] -> io:fwrite("Player wins!"),
      exit(normal);
      % bottom row win
      [_,_,_,_,_,_,-1,-1,-1] -> io:fwrite("Computer wins!"),
      exit(normal);
      [_,_,_,_,_,_,1,1,1] -> io:fwrite("Player wins!"),
      exit(normal);
      % left column win
      [-1,_,_,-1,_,_,-1,_,_] -> io:fwrite("Computer wins!"),
      exit(normal);
      [1,_,_,1,_,_,1,_,_] -> io:fwrite("Player wins!"),
      exit(normal);
      % middle column win
      [_,-1,_,_,-1,_,_,-1,_] -> io:fwrite("Computer wins!"),
      exit(normal);
      [_,1,_,_,1,_,_,1,_] -> io:fwrite("Player wins!"),
      exit(normal);
      % right column win
      [_,_,-1,_,_,-1,_,_,-1] -> io:fwrite("Computer wins!"),
      exit(normal);
      [_,_,1,_,_,1,_,_,1] -> io:fwrite("Player wins!"),
      exit(normal);
      % left diagonal win
      [-1,_,_,_,-1,_,_,_,-1] -> io:fwrite("Computer wins!"),
      exit(normal);
      [1,_,_,_,1,_,_,_,1] -> io:fwrite("Player wins!"),
      exit(normal);
      % right diagonal win
      [_,_,-1,_,-1,_,-1,_,_] -> io:fwrite("Computer wins!"),
      exit(normal);
      [_,_,1,_,1,_,1,_,_] -> io:fwrite("Player wins!"),
      exit(normal);
      _ -> continue
   end.





