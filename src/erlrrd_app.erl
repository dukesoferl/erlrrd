-module(erlrrd_app).

-export([start/0, stop/0]).
-behavior(application).
-export([start/2, stop/1]).

start() ->
  Apps = [ sasl, erlrrd ],
  [ ensure_started (App) || App <- Apps].

start(_Type, _Args) ->
  erlrrd_sup:start_link().

stop() ->
  application:stop(erlrrd).

stop(_State) ->
  ok.

%-=====================================================================-
%-                               Private                               -
%-=====================================================================-

ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.
