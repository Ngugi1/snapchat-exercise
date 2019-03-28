%% This is a simple implementation of the project, using one centralized server.
%%
%% It will create one "server" actor that contains all internal state (users,
%% their friends, and their stories).
%%
%% This implementation is provided with unit tests, however, these tests are
%% neither complete nor implementation independent, so be careful when reusing
%% them.
-module(server_centralized).

-include_lib("eunit/include/eunit.hrl").

-define(EXPIRATION_TIME, 30000). % time until a message expires, in milliseconds

%%
%% Exported Functions
%%
-export([initialize/0,
         % internal actors
         data_actor/1]).

%%
%% API Functions
%%

% Start server.
% This returns the pid of the server, but you can also use the name "data_actor"
% to refer to it.
initialize() ->
    %% TODO change 4 - data is now stored in a tuple, no need for a list anymore
    ServerPid = spawn_link(?MODULE, data_actor, [{}]),
    ServerPid.

% The data actor works like a small database and encapsulates all state of this
% simple implementation.
%
% The Data is a list of {user, UserId, Stories, LastStoryId, Friends}
% with Stories a map StoryId -> {story, UserId, Timestamp, Text}.
data_actor(Data) ->
    receive
        {Sender, register_user} ->
            {NewData, NewUserId} = add_new_user(),
%%            io:format("registered\n"),
            Sender ! {self(), registered_user, NewUserId}, %% TODO change 003 - respond to the user directly, don't go through the registration server
            data_actor(NewData);

        {Sender, homepage, UserId} ->
            homepage(Data, UserId, Sender),
            data_actor(Data);

        {Sender, get_stories, UserId} ->
            Sender ! {self(), stories, UserId, stories(Data, UserId)},
%%            io:format("gostories\n"),
            data_actor(Data);

        {Sender, make_story, UserId, Text} ->
            {NewData, Timestamp, StoryId} = make_story(Data, UserId, Text),
            Sender ! {self(), story_published, UserId, Timestamp},
%%            io:format("madestory\n"),
            % send_after sends the destruct message after 30 seconds
            timer:send_after(?EXPIRATION_TIME, self(),
                {self(), destruct_story, UserId, StoryId}),
            data_actor(NewData);

        {_Sender, destruct_story, UserId, StoryId} ->
            NewData = destruct_story(Data, UserId, StoryId),
            data_actor(NewData);

        {Sender, befriend, UserId, NewFriendId} ->
            NewData = befriend(Data, UserId, NewFriendId),
            Sender ! {self(), befriended, UserId, NewFriendId},
%%            io:format("befriended\n"),
            data_actor(NewData);
        _ ->
            io:write(unknown_message)
    end.

%%
%% Internal Functions
%%

add_new_user() ->
    %% TODO - CHANGE 002 here, the newly created server is the user ID
    % We store the stories in a map id => story.
    % See https://erldocs.com/current/stdlib/maps.html
    {{user, self(), #{}, 0, sets:new()}, self()}.

homepage(Data, UserId, Sender) ->
    {user, _ , _, _, Friends} = Data,
    %% This is a short lived process whose work is just to aggregate the results of getting stories
    AggregatorId = server_aggregate:initialize(length(sets:to_list(Friends)), Sender, UserId),
    case sets:is_empty(Friends) of
        true ->
            AggregatorId ! {self(), no_friends, UserId}; %% No friends no timeline
        false ->
            lists:foreach(fun(FriendId) ->
                FriendId ! {AggregatorId, get_stories, FriendId}
                          end,
                sets:to_list(Friends))
    end.

%%    UnsortedStories =
%%        lists:foldl(fun(FriendId, AccStories) ->
%%                        {_, _, FriendStories, _, _} = lists:nth(FriendId + 1, Data),
%%                        AccStories ++ maps:values(FriendStories)
%%                    end,
%%                    [],
%%                    sets:to_list(Friends)),
%%    % Sort stories by third element (timestamp), reversed
%%    SortedStories = lists:reverse(lists:keysort(3, UnsortedStories)),
%%    lists:sublist(SortedStories, 10).

stories(Data, UserId) ->
    {user, UserId, Stories, _, _} = Data,
    % Sort stories by third element (timestamp), reversed
    maps:values(Stories).

make_story(Data, UserId, Text) ->
%%    io:format("\nMaking story\n"),
    {user, UserId, Stories, LastStoryId, Friends} = Data,
    StoryId = LastStoryId + 1,
    Timestamp = os:timestamp(),
    NewStories = maps:put(StoryId, {story, UserId, Timestamp, Text}, Stories),
    NewUser = {user, UserId, NewStories, StoryId, Friends},
    {NewUser ,Timestamp, StoryId}. %% TODO - this is server for one user change 007

destruct_story(Data, UserId, StoryId) ->
    {user, UserId, Stories, LastStoryId, Friends} =  Data,
    NewStories = maps:remove(StoryId, Stories),
    NewUser = {user, UserId, NewStories, LastStoryId, Friends},
    NewUser. %% TODO - this is server for one user change 006

befriend(Data, UserId, NewFriendId) ->

    if
        UserId == NewFriendId ->
            Data;
        true ->
            {user, UserId, Stories, LastStoryId, Friends} = Data,
            NewFriends = sets:add_element(NewFriendId, Friends),
            NewUser = {user, UserId, Stories, LastStoryId, NewFriends},
            NewUser

    end.
%% TODO 5 - Change - we just return the same user but with different set of friends

%%
%% Test Functions
%%
%% These tests are for this specific implementation. They are a partial
%% definition of the semantics of the provided interface but also make certain
%% assumptions of its implementation. You can thus reuse them, but may need to
%% modify them.
%%

initialization_test() ->
    Pid = registration_server:initialize(),
    Pid.

register_user_test() ->
    ServerPid = initialization_test(),
    % We assume here that everything is sequential, and we have simple
    % incremental ids
    ?assertMatch({_, _Pid1}, server:register_user(ServerPid)),
    ?assertMatch({_, _Pid2}, server:register_user(ServerPid)),
    ?assertMatch({_, _Pid3}, server:register_user(ServerPid)),
    ?assertMatch({_, _Pid4}, server:register_user(ServerPid)).

init_for_test() ->
    ServerPid = initialization_test(),
    {_, Pid1} = server:register_user(ServerPid),
    {_, Pid2} = server:register_user(ServerPid),
    {_, Pid3} = server:register_user(ServerPid),
    {_, Pid4} = server:register_user(ServerPid),
   [Pid1, Pid2, Pid3, Pid4].


homepage_test() ->
    Pids = init_for_test(),
    [Pid1, Pid2 | _ ] = Pids,
    ?assertMatch([], server:homepage(Pid1, Pid1)),
    ?assertMatch([], server:homepage(Pid2, Pid2)).

users_stories_test() ->
    Pids = init_for_test(),
    [Pid1, Pid2 | _ ] = Pids,

    ?assertMatch([], server:get_stories(Pid1, Pid1)),
    ?assertMatch([], server:get_stories(Pid2, Pid2)).

story_test() ->
    Pids = init_for_test(),
    [Pid1, Pid2 | _ ] = Pids,

    ?assertMatch([], server:homepage(Pid1, Pid1)),
    ?assertMatch([], server:homepage(Pid2, Pid2)),
    io:format("1. We are here\n"),

    ?assertMatch({_MegaSecs, _Secs, _MicroSecs},
        server:make_story(Pid1, Pid1, "My first story")),
    io:format("2. We are here\n"),
    ?assertMatch([], server:get_stories(Pid2, Pid2)),
    io:format("3. We are here\n"),
    ?assertMatch([{story, _, _,"My first story"}], server:get_stories(Pid1, Pid1)),
    io:format("4. We are here\n"),
    Pids. % no friends

befriend_test() ->
    [Pid1, Pid2 | _ ] = story_test(),
    ?assertMatch(ok, server:befriend(Pid2, Pid2, Pid1)),
    io:format("Befriended\n"),
    % now there is a friend relation, so the first story should be visible
    ?assertMatch([{story, _, _, "My first story"}], server:homepage(Pid2, Pid2)),
        io:format("Aftwer Befriended\n"),
    % publish a second story and check whether it's visible
    ?assertMatch({_MegaSecs, _Secs, _MicroSecs},
      server:make_story(Pid1, Pid1, "My second story")),

    ?assertMatch([{story, _, _, "My second story"},
                 {story, _, _, "My first story"}],
                server:homepage(Pid2, Pid2)),
    io:format("+++++++++++++++ DONE ******************"),
    done.
