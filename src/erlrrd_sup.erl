-module(erlrrd_sup).

-export([start_link/1, start_link/0]).

-behavior(supervisor).

-export([init/1]).


%% @spec start_link(RRDToolCmd) ->  Result
%%   RRDToolCmd = string()
%%   Result = {ok,Pid} | ignore | {error,Error}
%%     Pid = pid()
%%     Error = {already_started,Pid} | shutdown | term()
start_link(RRDToolCmd) ->
  application:set_env(erlrrd, rrdtoolcmd, RRDToolCmd),
  supervisor:start_link(erlrrd_sup, []).

%% @spec start_link() ->  Result
%%   Result = {ok,Pid} | ignore | {error,Error}
%%     Pid = pid()
%%     Error = {already_started,Pid} | shutdown | term()
start_link() ->
  supervisor:start_link(erlrrd_sup, []).

init(_) ->
  SubProcesses =
    [
      {
        erlrrd,
        { erlrrd, start_link, [] },
        permanent,
        3000,
        worker,
        [ erlrrd ]
      }
    ],

  % optionally start the cache
  FinalProcesess =
    case application:get_env (erlrrd, cache) of
      undefined ->
        SubProcesses;
      {ok, CacheConfig} ->
        [
          {
            erlrrdcached,
            { erlrrdcached, start_link, [CacheConfig] },
            permanent,
            3000,
            worker,
            [ erlrrdcached ]
          }
          | SubProcesses ]
    end,

  {
    ok,
    {
      {one_for_one, 5, 10 },
      FinalProcesess
    }
  }.
