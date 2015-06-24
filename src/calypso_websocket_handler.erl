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

-record(state, {
	modules :: [atom()],
	ref :: reference(),
	ping_time :: calypso_time:time(),
	states :: [{ atom(), term()}],
	decode_fun :: fun((term()) -> term()),
	encode_fun :: fun((term()) -> term())
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

init(Req, { Modules, Opts}) ->
	ModulesSt = lists:reverse(lists:foldl(fun(Module, Acc) when is_atom(Module) ->
		{ ok, St } = Module:init(Req, Opts),
		[ {Module, St } | Acc ]
	end, [], Modules)),
	State = tick(#state{
		modules = Modules,
		ref = make_ref(),
		states = ModulesSt
	}),
	{cowboy_websocket, Req, State#state{
		ping_time = undefined
	}}.

websocket_handle({text, <<"pong">>}, Req, State) ->
	{ ok, Req, State#state{
		ping_time = undefined
	}};

websocket_handle({text, <<"ping">>}, Req, State) ->
	{ reply, { text, <<"pong">> }, Req, State#state{
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

websocket_info({ tick, Ref },  Req, State#state{ ping_time = undefined }) when Ref =:= State#state.ref ->
	{ reply, { text, <<"ping">> }, Req, tick(State#state{
		ping_time = calypso_time:now()
	})};

websocket_info({ tick, Ref },  Req, State#state{ ping_time = Time }) when Ref =:= State#state.ref ->
	case calypso_time:diff(Time + ?TICK, calypso_time:now()) < 0 of
		true ->
			{ shutdown, Req, State };
		false ->
			{ reply, { text, <<"ping">> }, Req, tick(State)}
	end;

websocket_info(Msg, Req0, State) ->
	case apply_cmd(fun(Module, Msg, Req, St) ->
		Module:websocket_info(Msg, Req, St)
	end, Msg, Req0, State)of
		{ ok, NewReq, NewState } ->
			{ ok, NewReq, NewState };
		{ ok, NewReq, NewState, hibernate } ->
			{ ok, NewReq, NewState, hibernate };
		{ {reply, Frames }, NewReq, NewState } ->
			{ reply, Frames, NewReq, NewState };
		{ {reply, Frames }, NewReq, NewState, hibernate } ->
			{ reply, Frames, NewReq, NewState, hibernate };
		{ shutdown, NewReq, NewState } ->
			{ shutdown, NewReq, NewState }
	end.


terminate(Reason, Req, State) ->
	lists:foreach(fun(Module) ->
  	Module:terminate(Reason, Req, st(State))
  end, State#state.modules).

tick(State) ->
	erlang:send_after(?TICK * 1000, self(), { tick, State#state.ref }),
	State.

module(State) ->
	State#state.modules.

st(State) ->
	State#state.states.

set_st(St, State) ->
  State#state{ states = St }.

apply_cmd(Fun, Cmd, Req, State) ->
	case apply_cmd(Fun, Cmd, State#state.states, [], Req) of
		{ Answer, NewReq, ModuleSt } ->
			{ Answer, NewReq, State#state{
			  states = ModuleSt
			}};
		{ Answer, NewReq, ModuleSt, hibernate } ->
			{ Answer, NewReq, State#state{
			  states = ModuleSt
			}, hibernate}
	end.

apply_cmd(_Fun, _Cmd, Req, [], Acc) ->
	{ ok, Req, lists:reverse(Acc) };

apply_cmd(Fun, Msg, Req, [{ Module, St} = ModuleItem | ModulesSt ], Acc) ->
  case Fun(Module, Msg, Req, St) of
		{ok, NewReq, NewSt } ->
			apply_cmd(Fun, Msg, Req, ModulesSt, [ ModuleItem | Acc ]);
		{ok, NewReq, NewSt, hibernate} ->
			apply_cmd(Fun, Msg, Req, ModulesSt, [ ModuleItem | Acc ]);
		{reply, Frames, NewReq, NewSt} ->
			{ { reply, Frames }, NewReq, collect(Module, NewSt, ModulesSt, Acc) };
		{reply, Frames, NewReq, NewSt, hibernate} ->
			{ { reply, Frames }, NewReq, collect(Module, NewSt, ModulesSt, Acc), hibernate};
		{shutdown, NewReq, NewSt} ->
			{shutdown, NewReq, collect(Module, NewSt, ModulesSt, Acc)}
	end.

collect(Module, St, ModuleSt, Acc) ->
  lists:reverse([ { Module, St } | Acc]) ++ ModuleSt.
