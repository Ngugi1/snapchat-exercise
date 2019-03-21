%%%-------------------------------------------------------------------
%%% @author ngugi
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% This server is only used for registering users.
%%% Registering a user is to spawn a new server for that specific user and persist their information here
%%% After registration, this registration server will keep track of users and their process identifiers
%%% @end
%%% Created : 14. Mar 2019 11:06
%%%-------------------------------------------------------------------
-module(registration_server).
-author("ngugi").

%% API
-export([initialize/0 ,registration_actor/0]).
%% Start this server which is responsible for creating new users in the system
initialize() ->
  Pid = spawn(?MODULE, registration_actor, []),
  %% register(registration_server, Pid),
  Pid.

%% TODO - CHANGE 001 - Each user must have their own server
%% Registration server keeps no data at all
registration_actor() ->
  receive
    {Sender, register_user} ->
      UserServer = server_centralized:initialize(),
      UserServer ! {Sender, register_user}, %% TODO here we expect a user id and process id from the server - we might use this later
      registration_actor();
    _ ->
      registration_actor()
  end.





