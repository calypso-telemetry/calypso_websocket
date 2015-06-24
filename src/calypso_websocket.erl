-module(calypso_websocket).
-author("begemot").

%% API
-export([
  cowboy_route/3
]).

-define(APP, calypso_websocket).

cowboy_route(Url, Module, Opt) when is_atom(Module), is_list(Url) ->
  { Url, calypso_websocket_handler, { Module, Opt }}.


