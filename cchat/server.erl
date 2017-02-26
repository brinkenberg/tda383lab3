-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_st{name = ServerName}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

%% connect to server

handle(St, Request) ->
	% placeholder
	% the generic server sends the evaluation of {request, self(), Ref, Data} which is F(State, Data) to Server
	% patternmatching states that handle(St, Data) will run. Data = {connect, {UserNick, UserPid}}
	requestFromGenServer(St, {connect, {UserNick, UserPid}}).


requestFromGenServer(St, {connect, {UserNick, UserPid}} ) ->
	%% for simplicity sake, at the moment you cannot connect if someone is connected with the default name
	%% user lists will hold both nick and pid
	
	UserNickinUsers = keymember(UserNick, N, St#server_st.users),
	UserPidInUsers = keymember(UserPid, N, St#server_st.users),
	if
		%% user is already connected
		UserNickinUsers xor UderPidInUsers -> 
			{reply, {user_already_connected}, St};
	if 	
		%%user can connect and is added to the list
		not UserNickinUsers xor UderPidInUsers -> 
			St#server_st{users={UserNick, UserPid}},
			{reply, ok, St};
	%% if user cannot connect to server for some reason this will send exit(Reason) to genserver which relays message to client
	

%%disconnect from server

%%connect to channel
%%disconnect from channel
