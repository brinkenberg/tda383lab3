%% @author Ingrid
%% @doc @todo Adds new client processes
-module(spawner).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/3]).

%% ====================================================================
%% Internal functions
%% ====================================================================
%% Spawn a process and register it with a given atom
%% Function F should have arity 1

start(Atom, State, F) ->
	%% apply(function(), [argument]) will run the called function with arguments
	%% called from different module as fun module:Function/Arity
  Pid = spawn(fun() -> apply(F, [State]) end),
  	catch(unregister(Atom)),
  	register(Atom, Pid),
  	Pid.

%% Send a request to a Pid and wait for a response
request(Pid, Data) ->
  request(Pid, Data, 3000).

%% Send a request to a Pid and wait for a response
%% With a specified timeout
request(Pid, Data, Timeout) ->
  Ref = make_ref(),
  Pid!{request, self(), Ref, Data},
  receive
    {result, Ref, Result} ->
      Result;
    {exit, Ref, Reason} ->
      exit(Reason)
  after Timeout ->
    exit("Timeout")
  end.
