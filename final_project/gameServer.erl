% gameServer.erl - A Distributed Adventure Game Server

-module(gameServer).
-author('Anastasia M. LaPeruta').
-define(else, true).  % -- This is to make the if statements (somewhat) readable.
-define(id, "-- game server: ").


-type direction() :: north | south | east | west.

%--------
% Public
%--------

-export([start/0]).

start() ->
   % -- Spawn the server process.
   io:fwrite("~sStarting Distributed Adventure Game Server (pid ~w) on node ~w.~nYou are on a road trip across the US when natural disasters break loose across all fifty states. Do your best to say safe.~n",[?id, self(), node()]),
    GameServerPid = spawn(fun() -> serverLoop([]) end),  % Provides an initial value for InventoryList
   io:fwrite("~sSpawned game server with pid ~w",[?id, GameServerPid]),
   % We want to publish this process in Erlang's local process registry.
   % Before we do that, we need to un-register it if it's already been registered.
   SomePlace = whereis(gameServer),
   if (SomePlace /= undefined) ->  % "not undefined" = "is defined"  (This is horrible, I know.)
      unregister(gameServer);
   ?else ->
      % The proccess was NOT already published/registered, so we don't really want to do anything here.
      true    % This is dumb, but I couldn't think of a better "no-op".
   end,
   % Publish this process in Erlang's local process registry.
   register(gameServer, GameServerPid),
   io:fwrite(", registered as ~w.~n",[gameServer]),
   % -- Erase the existing local process dictionary.
   erase(),
   % -- Begin the admin loop
   adminLoop().


%---------------------------------
% Private, but accepting messages.
%---------------------------------
serverLoop(InventoryList) ->
   receive

      % Local requests

      {local, listClientLocations} ->
         io:fwrite("~sThis server knows about the following (client) locations: ~w~n", [?id, get()]),
         serverLoop(InventoryList);

      {local, listNodes} ->
         io:fwrite("~sThis node: ~w~n", [?id, node()]),
         io:fwrite("~sOther nodes in our cluster: ~w~n", [?id, nodes()]),
         serverLoop(InventoryList);

      {local, endProcess} ->
         io:fwrite("~sThis server processes is ending. Good bye.~n", [?id]),
         exit(normal);


      % Remote requests

      {FromNode, registerNewLocation, LocId, Inventory}  ->
         io:fwrite("~sReceived registerNewLocation message from node ~w for ~w.~n",[?id, FromNode, LocId]),
         % Record this clientLocation and node in our process dictionary.
         io:fwrite("~sPutting {~w,~w} in the local process dictionary.~n", [?id, LocId, FromNode]),
         put(LocId, FromNode),
         io:fwrite("~sWe are now monitoring ~w.~n", [?id, FromNode]),
         monitor_node(FromNode, true),
         serverLoop(lists:append(InventoryList,Inventory));

      {nodedown, Node} ->
         % This server monitors location nodes.
         % A location node has gone down. Notify the admin console...
         io:fwrite("~sNode ~w has left our cluster and is no longer reachable.~n",[?id, Node]),
         % ...  and remove it from our process dictionary.
         LocIdList = get_keys(Node),      % We know the node but we need the LocId to
         erase(hd(LocIdList)),            % erase it from our process dictionary.
         serverLoop(InventoryList);

      {FromNode, NewLocale, TurnCount, Score, goToLocation, NewDir, NewInventoryList}  ->
         io:fwrite("~sReceived goToLocation message from node ~w for direction [~w].~n",[?id, FromNode, NewDir]),
         %Look up the Direction in our local process dictionary
         io:fwrite("~sGetting node for location [~w] from the local process dictionary.~n", [?id, translateToLoc(NewLocale)]),
         ClientLocNode = translateToLoc(NewLocale),
         if ClientLocNode == undefined ->
            io:fwrite(ClientLocNode),
            io:fwrite("~sNode not found in the local process dictionary.~n", [?id]),
            %Use only FromPid here because we don't know the registered name of the process (because there is none).
            {gameClient, FromNode} ! {node(), "You cannot go that way."};
         ?else ->
            io:fwrite("~sFound node in the local process dictionary: [~w].~n", [?id, ClientLocNode]),
            {gameClient, FromNode} ! {node(), "[debug] You CAN go that way."},
            %Tell the ClientLocId on ClientLocNode that a gameClient on FromNode is entering.
            ClientLocId = translateToLoc(NewLocale),
            {ClientLocId, ClientLocNode} ! {node(), enter, FromNode}
         end, % if
         serverLoop(NewInventoryList);
   

      {FromNode, {Num, NewDir}} ->
         io:fwrite("~sReceived move from node ~w for direction [~w].~n",[?id, FromNode, NewDir]),
         serverLoop(InventoryList);


      {FromNode, _Any}  ->
         io:fwrite("~sReceived unknown request [~p] from node ~w.~n",[?id, _Any, FromNode]),
         serverLoop(InventoryList);

      {FromNode, _Any1, _Any2}  ->
         io:fwrite("~sReceived unknown request [~p, ~p] from node ~w.~n",[?id, _Any1, _Any2, FromNode]),
         serverLoop(InventoryList);
   
      AnyOther -> 
         io:fwrite("~sReceived unexpected message: ~p~n", [?id, AnyOther]),
         serverLoop(InventoryList)
   end.



% Mapper. Decides location based on direction
translateToLoc(0) -> loc0; 
translateToLoc(1)  -> loc1;
translateToLoc(2)  -> loc2;
translateToLoc(3) -> loc3;
translateToLoc(4) -> loc4;
translateToLoc(5)  -> loc5;
translateToLoc(6)  -> loc6;
translateToLoc(_) -> undefined.




%---------
% Private
%---------

adminLoop() ->
   % -- Get input from the user.  We need the formatted prompt inside the fread to
   %    prevent line breaks and keep the text together when receiving messages.
   {ok, Input} = io:fread(io_lib:format("~s[admin] Enter command or help -] ", [?id]), "~s"),  % Input is returned as a list.
   Command = hd(Input),  % (Because Input is a list.)
   %
   % -- Process the input/command
   {ResultAtom, ResultText} = processCommand(Command),
   %
   % -- Update the display.
   io:fwrite("~s~s~n", [?id, ResultText]),
   %
   % -- Quit or Recurse/Loop.
   if (ResultAtom == quit) ->
     % Tell the server to kill itself.
     % TODO: check that our linked processes are also killed. TODO TODO: Link them in the first place.
     gameServer ! {local, endProcess},
     ok;
   ?else ->
     adminLoop()  % This is tail recursion, so it's really a jump to the top of adminLoop.
   end. % if


processCommand(Command) ->
   case Command of
      "help"      -> {help, helpText()};
      "locations" -> {procList, listLocations()};
      "nodes"     -> {procList, listNodes()};
      "quit"      -> {quit, "Sending [quit] request..."};
      "q"         -> {quit, "Sending [quit] request..."};
      % -- Otherwise...
      _Else  -> {unknownCommand, "I'm sorry Dave, I do not understand."}  % Starting _Else with "_" prevents the "unused" warning.
   end.

helpText() ->
   io_lib:format("Commands: [help], [locations], [nodes], [quit]", []).

listLocations() ->
   % Send a messagfe to the local gameServer telling it to list the
   % client locations it knows about. Since the gameServer is running
   % as a different process (becasue we spawned) its process dictionary
   % is seperate from ours and we cannot access it here. That's why
   % we're sending a message to it with this request.
   gameServer ! {local, listClientLocations},
   ok.

listNodes() ->
   % Send a message to the local gameServer telling it to list the
   % nodes it knows about.
   gameServer ! {local, listNodes},
   ok.