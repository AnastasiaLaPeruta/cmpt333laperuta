% - -- -  - -- -  - -- -  - -- -  - -- - - -- -  - -- -- -- -  - -- -  - -- -  - -- -  - -- --
%
% grabit.erl - grab URLs from the ever-so-annoying Slideshare via their Amazon S3 CDN storage.
%
% Sample usage:
%    grabit:start("http://cdn.slidesharecdn.com/nosqldatabases-slideshare-110227120448-phpapp01-slide-{#}.swf", 1, 5).
%    grabit:start("https://www.labouseur.com/courses/erlang/lab{#}.pdf", 0, 4).
%    grabit:start("https://www.labouseur.com", 0, 0).
% - -- -  - -- -  - -- -  - -- -  - -- - - -- -  - -- -- -- -  - -- -  - -- -  - -- -  - -- --

-module(grabit).

-author('Alan G. Labouseur').

-define(else, true).  % -- This is to make the if statements (somewhat more) readable.

%%%
%%% ------------- Public -------------
%%%
-export([start/0, start/3]).

%% No command-line arguements.
start() ->
    welcome(),
    usage().

%% There are command-line arguements. Deal with them.
start(UrlPattern, Start, Stop)  -> try
                                      welcome(),
                                      grabInit(UrlPattern, Start, Stop)
                                   catch
                                      _Class:Reason -> io:fwrite("Error: ~s.~n", [Reason])
                                   end.


%%%
%%% ------------- Private -------------
%%%

welcome() ->
    io:fwrite("Welcome to grabit.~n", []).

usage() ->
    io:fwrite("Usage:\tgrabit:start(url_pattern, start_int, stop_int) grabs at [url_pattern] from [start_int] to [stop_int].~n", []).

grabInit(UrlPattern, Start, Stop) ->
    io:fwrite("Generating URLs to grab from ~p to ~p in ~p.~n", [Start, Stop, UrlPattern]),
    UrlList = generateURLs(UrlPattern, Start, Stop),
    io:fwrite("Starting inets and ssl applications.~n", []),
    try
       inets:start(),     % TODO: Don't do these every time;
       ssl:start()        %       just when we need to.
    catch
       _ -> erlang:exit("Failed to start inets and/or ssl.")
    end,
    grab(UrlList, []).


generateURLs(UrlPattern, Current, Stop) when (Current =< Stop) ->
    [ re:replace(UrlPattern, "{#}", integer_to_list(Current), [global, {return, list}]) | generateURLs(UrlPattern, Current+1, Stop) ];
generateURLs(_UrlPattern, Current, Stop) when (Current > Stop) -> [].

grab([ThisUrl | RestUrls], ConvertCommands) ->
    io:fwrite("Grabbing ~p.~n", [ThisUrl]),
    % Send an HTTP request to the Url asking for the content at that address.
    {ok, {{_HttpVer, _Code, _Msg}, _Headers, BodyBin}} = httpc:request(get, {ThisUrl, []}, [], [{body_format, binary}]),
    % Store the result.
    storeBody(BodyBin, ThisUrl),
    % Generate the SWFconvert command to turn this into a PNG later on.
    Cmd = genConvertCommand(ThisUrl),
    % Recurse on the tail of the list while accumulating the convert commands.
    grab( RestUrls, [Cmd | ConvertCommands] );
grab(_, ConvertCommands) ->
    % The recursion on the URLs has stopped.
    % Write the SWFtools SWFrender commands to process the SWFs into PNGs.
    saveCmds(ConvertCommands).

storeBody(BodyBin, ThisUrl) ->
    % Construct the FileName.
    FileName = "grabit-" ++ filename:basename(ThisUrl),
    io:fwrite("Storing body in ~p.~n", [FileName]),
    % Open (or overwrite) our file ...
    {ok, Handle} = file:open(FileName, [write, binary]),
    % ... write the Content ...
    file:write(Handle, BodyBin),
    % ... and close it.
    file:close(Handle).

genConvertCommand(ThisUrl) ->
    % Construct the fileNames.
    SwfFileName = "grabit-" ++ filename:basename(ThisUrl),
    PngFileName = "grabit-" ++ filename:basename(ThisUrl, ".swf") ++ ".png",
    io_lib:format("swfrender ~p -o ~p~n", [SwfFileName, PngFileName]).

saveCmds(ConvertCommands) ->
    FileName = "grabit-convert.sh",
    io:fwrite("Writing convert commands to ~p.~n", [FileName]),
    file:write_file(FileName, ConvertCommands).
