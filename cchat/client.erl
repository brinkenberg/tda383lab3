-module(client).
-export([handle/2, initial_state/2, tryConnect/1, tryDisconnect/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
%%	sets the nickname, gui and sever to default.
initial_state(Nick, GUIName) ->
    #client_st { gui = GUIName, nick = Nick, server = Default }.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server	
%% St is the variable whose value is a client record
%% the Client either replies with the atom ok or {error, Atom, Text}. 
%% Atom ok indicates that the operation succeeded. Tuple {error, Atom, Text} 
%% denotes that something went wrong while processing the request.

%response from server via module genserver
handle(St, {connect, Server}) ->
	% Checks if the handling function of the genserver fails for any reason (notes: lecture 8)
	% genserver:request(Pid, Data)
	% Pid = the servername, Data = UserNick and UserPid (needed for retreival)

	case catch (genserver:request(Server, {connect, {St#client_st.nick, self()}})) of
		% requester gets exception, no valid server or other fault
		{'EXIT', Reason} -> 
			{reply, {error, server_not_reached, "Server not reached!"}, St} ;
		
		% request to connect granted
		ok ->
		St#client_st.server = list_to_atom(Server), 
		{reply, ok, St};
		
		%user is already connected
		{user_already_connected} -> 
		{reply, {error, user_already_connected, "You are already connected to the server!"}, St} ;
	end;	


%% Disconnect from server
handle(St, disconnect) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;
	
		 
% Join channel
handle(St, {join, Channel}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Leave channel
handle(St, {leave, Channel}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
handle(St, whoami) ->
    % {reply, "nick", St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
