-module(calypso_websocket_handler).
-author("begemot").


-behaviour(cowboy_websocket).
%% API
-export([
  init/2,
  websocket_handle/3,
  websocket_info/3,
  terminate/3
]).

-define(TICK, 30).

-record(req, {
	module :: atom(),
	ref :: reference(),
	ping_time :: calypso_time:time(),
	state :: term()
}).

-type frame() :: close | ping | pong
	| {text | binary | close | ping | pong, iodata()}
	| {close, close_code(), iodata()}.
-export_type([frame/0]).

-type req() :: cowboy_req:req().

-callback init(Req :: req(), Opt :: term()) -> { ok, State :: term() }.
-callback websocket_handle(Msg :: term(), Req, State ) ->
	{ok, Req, State}
	| {ok, Req, State, hibernate}
	| {reply, frame() | [frame()], Req, State}
	| {reply, frame() | [frame()], Req, State, hibernate}
	| {shutdown, Req, State}
	when Req::cowboy_req:req(), State::any().

-callback websocket_info(Msg :: term(), Req, State)
	-> {ok, Req, State}
	| {ok, Req, State, hibernate}
	| {reply, frame() | [frame()], Req, State}
	| {reply, frame() | [frame()], Req, State, hibernate}
	| {shutdown, Req, State}
	when Req::cowboy_req:req(), State::any().

-callback terminate(Reason, Req, State) -> ok.

init(Req, { Module, Opts}) when is_atom(Module) ->
	{ ok, St } = Module:init(Req, Opts),
	State = tick(#req{
		module = Module,
		ref = make_ref(),
		state = St
	}),
	{cowboy_websocket, Req, State#req{
		ping_time = undefined
	}}.

websocket_handle({text, <<"pong">>}, Req, State) ->
	{ ok, Req, State#req{
		ping_time = undefined
	}};

websocket_handle({text, <<"ping">>}, Req, State) ->
	{ reply, { text, <<"pong">> }, Req, State#req{
	  ping_time = calypso_time:now()
	}};

websocket_handle(Msg, Req0, State) ->
	Module = module(State),
	case Module:websocket_handle(Msg, Req0, st(State)) of
		{ok, Req, St} ->
			{ ok, Req, set_st(St, State) };
		{ok, Req, St, hibernate} ->
			{ ok, Req, set_st(St, State), hibernate};
		{reply, Frames, Req, St} ->
			{reply, Frames, Req, set_st(St, State) };
		{reply, Frames, Req, St, hibernate} ->
			{reply, Frames, Req, set_st(St, State), hibernate};
		{shutdown, Req, St} ->
			{shutdown, Req, set_st(St, State)}
	end.

websocket_info({ tick, Ref },  Req, State#req{ ping_time = undefined }) when Ref =:= State#req.ref ->
	{ reply, { text, <<"ping">> }, Req, tick(State#req{
		ping_time = calypso_time:now()
	})};

websocket_info({ tick, Ref },  Req, State#req{ ping_time = Time }) when Ref =:= State#req.ref ->
	case calypso_time:diff(Time + ?TICK, calypso_time:now()) < 0 of
		true ->
			{ shutdown, Req, State };
		false ->
			{ reply, { text, <<"ping">> }, Req, tick(State)}
	end;

websocket_info(Msg, Req0, State) ->
	Module = module(State),
	case Module:websocket_info(Msg, Req0, st(State)) of
		{ok, Req, St} ->
			{ ok, Req, set_st(St, State) };
		{ok, Req, St, hibernate} ->
			{ ok, Req, set_st(St, State), hibernate};
		{reply, Frames, Req, St} ->
			{reply, Frames, Req, set_st(St, State) };
		{reply, Frames, Req, St, hibernate} ->
			{reply, Frames, Req, set_st(St, State), hibernate};
		{shutdown, Req, St} ->
			{shutdown, Req, set_st(St, State)}
	end.

terminate(Reason, Req, State) ->
	Module = module(State),
  Module:terminate(Reason, Req, st(State)).


tick(State) ->
	erlang:send_after(?TICK * 1000, self(), { tick, State#req.ref }),
	State.

module(State) ->
	State#req.module.

st(State) ->
	State#req.state.

set_st(St, State) ->
  State#req{ state = St }.
