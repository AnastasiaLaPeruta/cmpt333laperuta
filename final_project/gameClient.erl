% gameClient.erl - A Distributed Adventure Game Client

-module(gameClient).
-author('Anastasia M. LaPeruta').
-define(else, true).  % -- This is to make the if statements (somewhat) readable.
-define(id, "-- game client: ").
-define(goToLocation, goToLocation).



-type direction() :: north | south | east | west.


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
   processCommand(string:tokens("go north ", " "), GameClientPid, 1, 120, -1, []),
   % -- Begin the play loop
   playLoop(ServerNode, 2, 110, 0, []).


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







% Mapper. Decides location based on direction
mapper(-1, north) -> loc0; 
mapper( 0, west)  -> loc1;
mapper( 0, east)  -> loc5;
mapper( 0, south) -> loc3;
mapper( 1, south) -> loc2;
mapper( 1, east)  -> loc0;
mapper( 2, east)  -> loc3;
mapper( 2, north) -> loc1;
mapper( 3, east)  -> loc4;
mapper( 3, west)  -> loc2;
mapper( 3, north) -> loc0;
mapper( 4, north) -> loc5;
mapper( 4, west)  -> loc3;
mapper( 5, north) -> loc6;
mapper( 5, south) -> loc4;
mapper( 5, west)  -> loc0;
mapper( 6, south) -> loc5;
mapper(_, _) -> -1.

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
   if (ResultAtom == quit orelse CurrentLocale == 6) ->
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
      "h"        -> {CurrentLocale, helpText()};
      "map"      -> {CurrentLocale, showMap(CurrentLocale)};
      "show map" -> {CurrentLocale, showMap(CurrentLocale)};
      "inventory"-> {CurrentLocale, showInventory(InventoryList)};
      "i"        -> {CurrentLocale, showInventory(InventoryList)};
      % -- Otherwise...
      _Else      -> {unknownCommand, "I do  not understand that command."}
   end.

showInventory([])            -> io_lib:format("You are not carrying anything of use.", []);
showInventory(InventoryList) -> io_lib:format("You are carrying ~w.", [lists:usort(InventoryList)]).


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
  DestString = string:to_lower(lists:flatten(Destination)),
case DestString of
    "north" -> io:fwrite("Moving north.");
    "n"     -> io:fwrite("Moving north.");
    "south" -> io:fwrite("Moving south.");
    "s"     -> io:fwrite("Moving south.");
    "east"  -> io:fwrite("Moving east.");
    "e"     -> io:fwrite("Moving east.");
    "west"  -> io:fwrite("Moving west.");
    "w"     -> io:fwrite("Moving west.");
    _       -> io:fwrite("That is not a direction.")
end,


   if (CurrentLocale == 3) ->
      % adds the 20 point bonus when location 3 is reached
      {gameServer, ServerNode} ! {node(), CurrentLocale, TurnCount+1, Score+20, goToLocation, DestString, InventoryList};
      % otherwise keeps decreasing score by 10 each move
   ?else ->
      {gameServer, ServerNode} ! {node(), CurrentLocale, TurnCount+1, Score-10, goToLocation, DestString, InventoryList}
   end,
   ok;

go([], _ServerNode, _TurnCount, _Score, _CurrentLocale, _InventoryList) ->
   io_lib:format("Where do you want to go?", []).

% Send the move message (a tuple) to itself.
-spec move(pid(), {integer(), direction()}) -> integer(). %  This is not enforced at runtime. It's for Dializer and Typer.
move(GameClientPid, MoveTuple) ->
   GameClientPid ! {self(), MoveTuple},
   receive
      {GameClientPid, Response} -> Response  % This waits for a response from ToPid.
   end.