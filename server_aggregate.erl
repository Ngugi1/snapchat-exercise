%%%-------------------------------------------------------------------
%%% @author ngugi
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Mar 2019 07:57
%%%-------------------------------------------------------------------
-module(server_aggregate).
-author("ngugi").

%% API
-export([initialize/3, aggregate_stories/5]).
initialize(ToProcessCount, RespondTo, UserId) ->
  io:format("Aggregator is waiting"),
  io:write(ToProcessCount),
  io:write(RespondTo),
  spawn_link(?MODULE, aggregate_stories, [[], ToProcessCount, 0, RespondTo, UserId]).

aggregate_stories(Timeline, ToProcessCount, Processed, RespondTo, UserId) ->
  receive
    {Sender, stories, _, Stories} ->
      NewProcessed = Processed + 1,
      NewTimeline = Timeline ++ Stories,
      if
        ToProcessCount == NewProcessed ->
          io:write(in_ifbranch),
          SortedTimeLine = lists:reverse(lists:keysort(3,NewTimeline)),
          io:write(RespondTo),
          RespondTo ! {Sender, homepage, UserId, SortedTimeLine},
          io:write(exiting_now),
          exit(self(), normal);
        true -> % works as an 'else' branch
          aggregate_stories(NewTimeline, ToProcessCount , NewProcessed, RespondTo, UserId)
      end;

    {Sender, no_friends, UserId} ->
      RespondTo ! {Sender, homepage, UserId , []}%% Respond with no stories
%%      exit(self(), normal)
  end.
