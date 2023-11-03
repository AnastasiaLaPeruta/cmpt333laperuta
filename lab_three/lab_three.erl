% lab_three.erl - Adventure Game Server

-module(lab_three).
-author('Anastasia M. LaPeruta').
-define(else, true).  % -- This is to make the if statements (somewhat) readable.


-type direction() :: north | south | east | west.

%--------
% Public
%--------

-export([start/0]).

start() ->
   % -- Spawn the server process.
   io:fwrite("Starting AG server.~nYou are on a road trip across the US when natural disasters break loose across all fifty states. Do your best to say safe.~n",[]),
   ServerPid = spawn(fun serverLoop/0),
   % -- Display the initial location description by moving north from -1.
   {_NewLocale, Description} = processCommand(-1, "north", ServerPid, []),
   io:fwrite("~n~s~n~n", [Description]),

   % -- Kick off the game loop with the ServerPID, location = 0, and turn count = 1, 
   % score at 120 since least possible moves to win would end up at score of 100 if descrease by 10 each time.
   gameLoop(ServerPid, 0, 1, 120, []).


%---------
% Private
%---------

gameLoop(ServerPid, CurrentLocale, TurnCount, Score, InventoryList) ->
   % -- Show the map and get input from the player.
   io:fwrite("~s", [showMap(CurrentLocale)]),
   % doesn't let score fall below 0
   if (Score >= 0) ->
      io:fwrite("~nScore=~w  Turn ~w ] ", [Score, TurnCount]);
   ?else ->
      io:fwrite("~nScore=~w  Turn ~w ] ", [0, TurnCount])
   end,
   {ok, Input} = io:fread("Enter a command (or help) -] ", "~s"),  % Input gets returned as a list from io:fread.
   [Command | _] = Input,   % (Because Input is a list.)
   %
   % -- Process the player's input/command into a NewLocale and Description.
   {NewLocale, Description} = processCommand(CurrentLocale, Command, ServerPid, InventoryList),
   %
   % -- Update the display.
   io:fwrite("~n~s~n~n", [Description]),

   % -- Quit or Recurse/Loop.
   if (NewLocale < 0) ->
     io:fwrite("Goodbye.~n",[]);
   ?else ->
      if (NewLocale == 3) ->
         gameLoop(ServerPid, NewLocale, TurnCount+1, Score+20, lists:append(InventoryList,locationItems(NewLocale)));  % This is tail recursion,
      ?else ->
         gameLoop(ServerPid, NewLocale, TurnCount+1, Score-10, lists:append(InventoryList,locationItems(NewLocale)))
      end
   end.                                                                                                               % so it's really a jump to the top of gameLoop.


processCommand(CurrentLocale, Command, ServerPid, Inventory) ->
   case Command of
      % -- Compass directions - Get the new location from the server.
      "north" -> move(ServerPid, {CurrentLocale, north});
      "n"     -> move(ServerPid, {CurrentLocale, north});
      "south" -> move(ServerPid, {CurrentLocale, south});
      "s"     -> move(ServerPid, {CurrentLocale, south});
      "east"  -> move(ServerPid, {CurrentLocale, east});
      "e"     -> move(ServerPid, {CurrentLocale, east});
      "west"  -> move(ServerPid, {CurrentLocale, west});
      "w"     -> move(ServerPid, {CurrentLocale, west});
      % -- Other commands - Handle non-movement commands.
      "quit"          -> {-1, "Thank you for playing."};
      "q"             -> {-1, "Thank you for playing."};
      "look"          -> {CurrentLocale, locationDesc(CurrentLocale)};
      "l"             -> {CurrentLocale, locationDesc(CurrentLocale)};
      "h"          -> {CurrentLocale, helpText()};
      "help"          -> {CurrentLocale, helpText()};
      "map"           -> {CurrentLocale, showMap(CurrentLocale)};
      "show map"      -> {CurrentLocale, showMap(CurrentLocale)};
      "inventory"     -> {CurrentLocale, showInventory(Inventory)};
      "i"             -> {CurrentLocale, showInventory(Inventory)};
      % -- Otherwise...
      _Else   -> {CurrentLocale, "I do not understand."}  % Starting _Else with "_" prevents the "unused" warning.
   end.


helpText() -> io_lib:format("You can enter compass directions: [n] or [north], [s] or [south], [e] or [east], ", []) ++
              io_lib:format("[w] or [west], as well as [quit], [look], [help], [map], [inventory], and other commands.", []).


% Send the move message (a tuple) to the server.
-spec move(pid(), {integer(), direction()}) -> integer(). %  This is not enforced at runtime. It's for Dializer and Typer.
move(ServerPid, MoveTuple) ->
   ServerPid ! {self(), MoveTuple},
   receive
      {ServerPid, Response} -> Response  % This waits for a response from ToPid.
   end.


% This is the process spawned at the start.
serverLoop() ->
   receive
      {FromPid, {CurrentLocale, Direction}} ->
         NewLocaleNumber = mapper(CurrentLocale, Direction),
         if NewLocaleNumber > -1 ->
            % ends game if reached
            if NewLocaleNumber == 6 ->
               % Valid move.
               NewLocaleDesciption = locationDesc(NewLocaleNumber),
               NewLocaleItems      = locationItems(NewLocaleNumber),
               FromPid ! {self(), {NewLocaleNumber, io:format(" GAME OVER.")}},
               init:stop();
            ?else ->
               % Valid move.
               NewLocaleDesciption = locationDesc(NewLocaleNumber),
               NewLocaleItems      = locationItems(NewLocaleNumber),
               FromPid ! {self(), {NewLocaleNumber, io_lib:format("~s You see ~w scattered around.", [NewLocaleDesciption, NewLocaleItems])}},
               serverLoop()
            end;
         ?else ->
            % Invalid move.
            FromPid ! {self(), {CurrentLocale, "You cannot go that way."}},
            serverLoop()
         end;

      {FromPid, _} ->
         FromPid ! {self(), "Internal error: You are lost. Get back on track before the chaos catches up to you."},
         serverLoop()
   end.


% Mapper. Double-check with showMap().
mapper(-1, north) -> 0; 
mapper( 0, west)  -> 1;
mapper( 0, east)  -> 5;
mapper( 0, south) -> 3;
mapper( 1, south) -> 2;
mapper( 1, east)  -> 0;
mapper( 2, east)  -> 3;
mapper( 2, north) -> 1;
mapper( 3, east)  -> 4;
mapper( 3, west)  -> 2;
mapper( 3, north) -> 0;
mapper( 4, north) -> 5;
mapper( 4, west)  -> 3;
mapper( 5, north) -> 6;
mapper( 5, south) -> 4;
mapper( 5, west)  -> 0;
mapper( 6, south) -> 5;
mapper( _, _)     ->-1.


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


% Location Descriptions
% These location descriptions DO NOT end with ~n newlines. The newline is taken care of in the display code.
locationDesc(0)   -> io_lib:format("0. South Dakota~nHome of Mount Rushmore. An earthquake is breaking loose the boulders of the monument. Better get out quick!", []);
locationDesc(1)   -> io_lib:format("1. Las Vegas, Nevada~nYou decide to take a break at the casino when the building begins to flood.", []);
locationDesc(2)   -> io_lib:format("2. California~nUsually sunny and clear skies, a wildfire creates clouds of smoke that cause low visibility.", []);
locationDesc(3)   -> io_lib:format("3. Texas~nYou have been given 20 bonus points! Unfortunately, massive tornadoes rip through the state, each a mile wide and you must evacuate.", []);
locationDesc(4)   -> io_lib:format("4. Florida~nYou are in Miami where a tsunami threatens the coast.", []);
locationDesc(5)   -> io_lib:format("5. North Carolina~nYou are horseback riding in the Outer Banks while a hurricane is forming over the Atlantic Ocean.", []);
locationDesc(6)   -> io:format("6. Canadian Border~nYou successfully escaped to Canada.");
locationDesc(Loc) -> io_lib:format("Oops! Unknown locale: ~w.", [Loc]).


% Location Items
locationItems(0)    -> [brochure, pocketknife];
locationItems(1)    -> [loose_change, keys];
locationItems(2)    -> [water_bottle];
locationItems(3)    -> [];
locationItems(4)    -> [swimsuit];
locationItems(5)    -> [compass];
locationItems(6)    -> [];
locationItems(_Loc) -> [].

% Other Commands

showInventory([])            -> io_lib:format("You are not carrying anything of use.", []);
showInventory(InventoryList) -> io_lib:format("You are carrying ~w.", [lists:usort(InventoryList)]).


