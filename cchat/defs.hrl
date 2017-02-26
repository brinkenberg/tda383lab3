% This record defines the structure of the client process.
% Add whatever other fields you need.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
%	nick: the nickname of the user
%	server: the server the user is connected to (if Default, no server has connected)
-record(client_st, {gui, nick, server}).

% This record defines the structure of the server process.
% Add whatever other fields you need.
%	users: list of users connected to the server
%	channels: lists of channels active on the server
-record(server_st, {name, users = [], channels = []}).

% This record defines the structure of the channel process.
% Add whatever other fields you need.
%	users: list of users in the channel
-record(channel_st, {users = []}).
