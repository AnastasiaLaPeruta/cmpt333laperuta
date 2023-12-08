% loc5.erl - Distributed Adventure Game Location 4

-module(loc5).
-author('Anastasia M. LaPeruta').
-define(else, true).  % -- This is to make the if statements (somewhat) readable.
-define(id, "-- location 5: ").


%--------
% Public
%--------

-export([start/0, start/1, locationLoop/0]).

start() ->
   io:fwrite("You must supply a game sever node.~n", []).

start(ServerNode) ->
   % -- Spawn this location process.
   io:fwrite("~sStarting Location 5 (pid ~w) on node ~w.~n",[?id, self(), node()]),
   LocPid = spawn(loc5, locationLoop, []),
   io:fwrite("~sSpawned location with pid ~w",[?id, LocPid]),
   % We want to publish this process in Erlang's process registry.
   % Before we do that, we need to un-register it if it's already been registered.
   SomePlace = whereis(loc5),
   if (SomePlace /= undefined) ->  % "not undefined" = "is defined"  (This is horrible, I know.)
      unregister(loc5);
   ?else ->
      % The proccess was NOT already published/registered, so we don't really want to do anything here.
      true    % This is dumb, but I couldn't think of a better "no-op".
   end,
   % Publish this process in Erlang's process registry.
   register(loc5, LocPid),
   io:fwrite(", registered as ~w.~n",[loc5]),
   % Send ourselves to the gameServer.
   io:fwrite("~sNotifying server on node ~w.~n",[?id, ServerNode]),
   {gameServer, ServerNode} ! {node(), registerNewLocation, loc5, [itemsScattered()]},
   % Initialize server monitoring.
   loc5 ! {monitor, ServerNode},
   ok.


%---------------------------------
% Private, but accepting messages.
%---------------------------------

locationLoop() ->
   receive
      {monitor, ServerNode} ->
         io:fwrite("~sMonitoring game server on node ~w.~n",[?id, ServerNode]),
         monitor_node(ServerNode, true),
         locationLoop();

      {nodedown, Node} ->
         % This location monitors the server node.
         % The server node has gone down. Notify the admin console...
         io:fwrite("~sServer node ~w has left our cluster and is no longer reachable. Shutting down.~n",[?id, Node]),
         % ...  and shut down.
         exit(normal);

      {_FromNode, enter, GameClientNode}  ->
         io:fwrite("~sA gameClient on ~w is entering loc5.~n",[?id, GameClientNode]),
         {gameClient, GameClientNode} ! {node(), unicode:characters_to_binary(describe())}, %translate so there is no unicode
         locationLoop();

      {FromNode, _Any}  ->
         io:fwrite("~sReceived request [~p] from node ~w.~n",[?id, _Any, FromNode]),
         locationLoop();

      AnyOther ->
         io:fwrite("testing for receive")
   end.


%--------
% Private
%--------
describe() ->
   io_lib:format("(5) North Carolina: You are horseback riding in the Outer Banks while a hurricane is forming over the Atlantic Ocean. You see ~w scattered around.", [itemsScattered()]).

itemsScattered() -> [compass].