% gameClient.erl - A Distributed Adventure Game Client
-module(gameClient).
-author('Anastasia M. LaPeruta').
-define(else, true).  % -- This is to make the if statements (somewhat) readable.
-define(id, "-- game client: ").
-define(goToLocation, goToLocation).

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
   playLoop(ServerNode, 0, 120, 0, []).
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
% all cases to make sure player does not go out of bounds
mapper(-1, "north") -> 0; 
mapper( 0, "west")  -> 1;
mapper( 0, "east")  -> 5;
mapper( 0, "south") -> 3;
mapper( 0, "north") -> 0;
mapper( 1, "south") -> 2;
mapper( 1, "east")  -> 0;
mapper( 1, "north")  -> 1;
mapper( 1, "west")  -> 1;
mapper( 2, "east")  -> 3;
mapper( 2, "north") -> 1;
mapper( 2, "south") -> 2;
mapper( 2, "west") -> 2;
mapper( 3, "east")  -> 4;
mapper( 3, "west")  -> 2;
mapper( 3, "north") -> 0;
mapper( 3, "south") -> 3;
mapper( 4, "north") -> 5;
mapper( 4, "west")  -> 3;
mapper( 4, "south")  -> 4;
mapper( 4, "east")  -> 4;
mapper( 5, "north") -> 6;
mapper( 5, "south") -> 4;
mapper( 5, "west")  -> 0;
mapper( 5, "east") -> 5;
mapper( 6, "south") -> 5;
mapper( 6, "north") -> 6;
mapper( 6, "west") -> 6;
mapper( 6, "east") -> 6;
mapper(_, _) -> -1.
playLoop(ServerNode, TurnCount, Score, CurrentLocale, InventoryList) ->
   % -- Get a line of input from the user.
   io:fwrite("~s", [showMap(CurrentLocale)]),
   % doesn't let score fall below 0
   if (Score >= 0) ->
      io:fwrite("~nScore=~w  Turn ~w ] ", [Score-20, TurnCount+1]);
   ?else ->
      io:fwrite("~nScore=~w  Turn ~w ] ", [0, TurnCount+1])
   end,
   Line = io:get_line(io_lib:format("~s[play] Enter action or help -] ", [?id])),  % Line is returned as a string.
   {ResultAtom, ResultText} = processCommand(Line, ServerNode, TurnCount, Score, CurrentLocale, InventoryList),
    %
    % Update the display.
    io:fwrite("~s~s~n", [?id, ResultText]),
    Command = lists:sublist(Line, length(Line)-1),
    Verb = lists:takewhile(fun(Element) -> Element /= 32 end, Command),
    Noun = lists:dropwhile(fun(Element) -> Element /= 32 end, Command),
    NewLoc = mapper(CurrentLocale, string:strip(Noun)),
    %
    % Quit or Recurse/Loop.
    if (ResultAtom == quit orelse ResultAtom == q orelse ResultAtom == help orelse NewLoc == 6 orelse ResultAtom == nodes orelse ResultAtom == server orelse ResultAtom == inventory orelse ResultAtom == map) ->
        NewLoc = mapper(CurrentLocale, string:strip(Noun)),
        if 
         (NewLoc == 6 orelse ResultAtom == quit orelse ResultAtom == q) ->
            io:fwrite("~s Thank you for playing.~n", [?id]);
         ?else ->
            playLoop(ServerNode, TurnCount+1, Score-10, CurrentLocale, InventoryList)
         end;
    ?else ->
        if Verb == "go"->
            playLoop(ServerNode, TurnCount+1, Score-10, NewLoc, InventoryList);
        ?else ->
            playLoop(ServerNode, TurnCount+1, Score-10, CurrentLocale, InventoryList)
        end
    end.
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
      "map"      -> {CurrentLocale, showMap(CurrentLocale)};
      "inventory"-> {CurrentLocale, showInventory(InventoryList)};
      % -- Otherwise...
      _Else      -> {unknownCommand, "I do  not understand that command."}
   end.

showInventory([])            -> io_lib:format("Sorry. Nothing you could be carrying would be of any use against a natural disaster", []);
showInventory(InventoryList) -> io_lib:format("You are carrying ~w.", [lists:usort(InventoryList)]).
helpText() ->
   io_lib:format("Commands: [help], [inventory], [map], [quit], [nodes], [server], [go <location>]", []).
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
go(Direction, ServerNode, TurnCount, Score, CurrentLocale, InventoryList) ->
   NewDir = string:strip(Direction),
    case NewDir of
        "north" -> io:fwrite("");
        "n"     -> io:fwrite("");
        "south" -> io:fwrite("");
        "s"     -> io:fwrite("");
        "east"  -> io:fwrite("");
        "e"     -> io:fwrite("");
        "west"  -> io:fwrite("");
        "w"     -> io:fwrite("");
        _       -> io:fwrite("That is not a direction.~n")
    end,
   NewLocale = translateToLoc(mapper(CurrentLocale,NewDir)),
   if (NewLocale == undefined), (not is_integer(NewLocale)) ->  % if input doesnt result in location on map or is invalid input
      playLoop(ServerNode, TurnCount, Score, CurrentLocale, InventoryList);
   ?else ->
      % otherwise keeps decreasing score by 10 each move
      {gameServer, ServerNode} ! {node(), NewLocale, goToLocation, NewDir, InventoryList}
   end,
   ok.


% Mapper. Decides location based on direction
translateToLoc(0) -> loc0; 
translateToLoc(1)  -> loc1;
translateToLoc(2)  -> loc2;
translateToLoc(3) -> loc3;
translateToLoc(4) -> loc4;
translateToLoc(5)  -> loc5;
translateToLoc(6)  -> loc6;
translateToLoc(_) -> undefined.

