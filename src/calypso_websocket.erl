-module(calypso_websocket).
-author("begemot").

%% API
-export([
  cowboy_route/3,
  add_handler/1, remove_handler/1
]).

-define(APP, calypso_websocket).

cowboy_route(Module, Url, Opt) when is_atom(Module), is_list(Url) ->
  { Url, calypso_websocket_handler, { Module, Opt }}.


