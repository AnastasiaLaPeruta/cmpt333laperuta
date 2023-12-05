% gameClient.erl - A Distributed Adventure Game Client

-module(gameClient).
-author('Anastasia M. LaPeruta').
-define(else, true).  % -- This is to make the if statements (somewhat) readable.
-define(id, "-- game client: ").


%--------
% Public
%--------

-export([start/0, start/1]).

start() ->
   io:fwrite("You must supply a game sever node.~n", []).

start(ServerNode) ->
   % -- Spawn this game client process.
   io:fwrite("~sStarting Distributed Adventure Game Client (pid ~w) on node ~w.~n",[?id, self(), node()]),
   GameClientPid = spawn(fun clientLoop/0),
   io:fwrite("~sSpawned game client with pid ~w",[?id, GameClientPid]),
   % We want to publish this process in Erlang's local process registry.
   % Before we do that, we need to un-register it if it's already been registered.
   SomePlace = whereis(gameClient),
   if (SomePlace /= undefined) ->  % "not undefined" = "is defined"  (This is horrible, I know.)
      unregister(gameClient);
   ?else ->
      % The proccess was NOT already published/registered, so we don't really want to do anything here.
      true    % This is dumb, but I couldn't think of a better "no-op".
   end,
   % Publish this process in Erlang's local process registry.
   register(gameClient, GameClientPid),
   io:fwrite(", registered as ~w.~n",[gameClient]),
   % Initialize server monitoring.
   gameClient ! {monitor, ServerNode},
   % -- Begin the play loop
   playLoop(ServerNode, 1, 120, 0, []).


%---------------------------------
% Private, but accepting messages.
%---------------------------------
clientLoop() ->
   receive
      {monitor, ServerNode} ->
         io:fwrite("~sMonitoring game server on node ~w.~n",[?id, ServerNode]),
         monitor_node(ServerNode, true),
         clientLoop();

      {nodedown, Node} ->
         % This client monitors the server node.
         % The server node has gone down. Notify the admin console...
         io:fwrite("~sServer node ~w has left our cluster and is no longer reachable. Shutting down.~n",[?id, Node]),
         % ...  and shut down.
         % TODO: exit the playLoop too.
         exit(normal);

      {FromNode, _Any}  ->
         io:fwrite("~sReceived message [~p] from node ~w.~n",[?id, _Any, FromNode]),
         clientLoop()
   end.


%---------
% Private
%---------


% Show map. Double-check with mapper().
showMap(CurrentLocale) ->
   io_lib:format("................... ~n",    []) ++
   io_lib:format(".............. ~s .. ~n",   [dispLocale(CurrentLocale, 6)]) ++
   io_lib:format(".............. | .. ~n",    []) ++
   io_lib:format(".............. | .. ~n",    []) ++
   io_lib:format(".. ~s --- ~s --- ~s .. ~n", [dispLocale(CurrentLocale, 1), dispLocale(CurrentLocale, 0), dispLocale(CurrentLocale, 5)]) ++
   io_lib:format(".. |.... | ... | .. ~n",    []) ++
   io_lib:format(".. |.... | ... | .. ~n",    []) ++
   io_lib:format(".. ~s --- ~s --- ~s .. ~n",  [dispLocale(CurrentLocale, 2), dispLocale(CurrentLocale, 3), dispLocale(CurrentLocale, 4)]) ++
   io_lib:format("................... ~n",    []).


dispLocale(CurrentLocale, MapLoc) ->
   if CurrentLocale == MapLoc ->
      "@";
   ?else ->
      integer_to_list(MapLoc)  % Remember, strings are lists of ASCII/Unicode values in Erlang.
   end.


playLoop(ServerNode, TurnCount, Score, CurrentLocale, InventoryList) ->
   % -- Get a line of input from the user.
   io:fwrite("~s", [showMap(CurrentLocale)]),
   % doesn't let score fall below 0
   if (Score >= 0) ->
      io:fwrite("~nScore=~w  Turn ~w ] ", [Score, TurnCount]);
   ?else ->
      io:fwrite("~nScore=~w  Turn ~w ] ", [0, TurnCount])
   end,
   Line = io:get_line(io_lib:format("~s[play] Enter action or help -] ", [?id])),  % Line is returned as a string.
   {ResultAtom, ResultText} = processCommand(Line, ServerNode, TurnCount, Score, CurrentLocale, InventoryList),
   %
   % -- Update the display.
   io:fwrite("~s~s~n", [?id, ResultText]),
   %
   % -- Quit or Recurse/Loop.
   if (ResultAtom == quit orelse CurrentLocale == loc6) ->
      io:fwrite("~sThank you for playing.~n", [?id]);
   ?else ->
     playLoop(ServerNode, TurnCount, Score, CurrentLocale, InventoryList)  % This is tail recursion, so it's really a jump to the top of playLoop.
   end. % if


processCommand(Line, ServerNode, TurnCount, Score, CurrentLocale, InventoryList) ->
   % Do some elementary parsing of the line in two parts:
   % 1. Remove the trailing newline charater.
   Command = lists:sublist(Line, length(Line)-1),  % (Because Line is a character list ending with a linefeed.)
   % 2. Break the line into two parts: before the space and after the space (if there's even a space)
   Verb = lists:takewhile( fun(Element) -> Element /= 32 end, Command),
   Noun = lists:dropwhile( fun(Element) -> Element /= 32 end, Command),
   %
   case Verb of
      "help"     -> {help,   helpText()};
      "quit"     -> {quit,   "Quitting."};
      "q"        -> {quit,   "Quitting."};
      "nodes"    -> {nodes,  listNodes()};
      "server"   -> {server, server(ServerNode)};
      "go"       -> {go,     go(Noun, ServerNode, TurnCount, Score, CurrentLocale, InventoryList)};
      "look"     -> {CurrentLocale, locationDesc(CurrentLocale)};
      "l"        -> {CurrentLocale, locationDesc(CurrentLocale)};
      "h"        -> {CurrentLocale, helpText()};
      "help"     -> {CurrentLocale, helpText()};
      "map"      -> {CurrentLocale, showMap(CurrentLocale)};
      "show map" -> {CurrentLocale, showMap(CurrentLocale)};
      "inventory"-> {CurrentLocale, showInventory(Inventory)};
      "i"        -> {CurrentLocale, showInventory(Inventory)};
      % -- Otherwise...
      _Else      -> {unknownCommand, "I do  not understand that command."}
   end.

helpText() ->
   io_lib:format("Commands: [help], [quit], [nodes], [server], [go <location>]", []).

listNodes() ->
   io_lib:format("This node: ~w~n", [node()]) ++   % No ?id here because it will be supplied when printed above.
   io_lib:format("~sOther nodes in our cluster: ~w", [?id, nodes()]).

server(ServerNode) ->
   KnownNode = lists:member(ServerNode, nodes()),
   if KnownNode ->
      io_lib:format("Talking to game server on node ~w, which is known to be in our cluster.", [ServerNode]);
   ?else ->
      io_lib:format("Talking to game server on node ~w, which is NOT known to be in our cluster, and that may be a problem.", [ServerNode])
   end. % if

go([_Space | Destination], ServerNode, TurnCount, Score, CurrentLocale, InventoryList) ->
   DestAtom = list_to_atom(Destination),
   io:fwrite("~s[debug] Going [~w].~n", [?id, DestAtom]),
    % -- Compass directions - Get the new location from the server.
   if DestAtom == "north" -> move(ServerPid, {CurrentLocale, north});
   if DestAtom == "n"     -> move(ServerPid, {CurrentLocale, north});
   if DestAtom == "south" -> move(ServerPid, {CurrentLocale, south});
   if DestAtom == "s"     -> move(ServerPid, {CurrentLocale, south});
   if DestAtom == "east"  -> move(ServerPid, {CurrentLocale, east});
   if DestAtom == "e"     -> move(ServerPid, {CurrentLocale, east});
   if DestAtom == "west"  -> move(ServerPid, {CurrentLocale, west});
   if DestAtom == "w"     -> move(ServerPid, {CurrentLocale, west});
   if (CurrentLocale == loc3) ->
      % adds the 20 point bonus when location 3 is reached
      {gameServer, ServerNode} ! {node(), CurrentLocale, TurnCount+1, Score+20, goToLocation, DestAtom, lists:append(InventoryList,locationItems(CurrentLocale))};  % This is tail recursion, so it's really a jump to the top of gameLoop.
      % otherwise keeps decreasing score by 10 each move
   ?else ->
      {gameServer, ServerNode} ! {node(), CurrentLocale, TurnCount+1, Score-10, goToLocation, DestAtom, lists:append(InventoryList,locationItems(CurrentLocale))}
   end,
   ok.
go([], _ServerNode, TurnCount, Score, CurrentLocale, InventoryList) ->
   io_lib:format("Where do you want to go?", []).
