-module (erlrrdcached).

-behaviour(gen_server).

-export ([start_link/1,
          listen_file/0,
          pid_file/0,
          alive/0,
          ping/0]).
-export ([init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3]).

-record(state, {port}).

start_link (Config) ->
  gen_server:start_link ( { local, ?MODULE }, ?MODULE, Config, []).

stringify (V) when is_integer (V) ->
  integer_to_list (V);
stringify (V) ->
  V.

process_config (Config) ->
  process_config (Config, "", "").

process_config ([],Command, Args) ->
  [Command | Args];
process_config ([{listen,V}|Rest], Command, Args) ->
  process_config (Rest, Command, [ " -l ", V | Args ]);
process_config ([{group, V}|Rest], Command, Args) ->
  process_config (Rest, Command, [ " -s ", stringify (V) | Args ]);
process_config ([{mode, V}|Rest], Command, Args) ->
  process_config (Rest, Command, [ " -m ", V | Args ]);
process_config ([{commands, V}|Rest], Command, Args) ->
  process_config (Rest, Command, [ " -P ", V | Args ]);
process_config ([{write_timeout, V}|Rest], Command, Args) ->
  process_config (Rest, Command, [ " -w ", stringify (V) | Args ]);
process_config ([{delay, V}|Rest], Command, Args) ->
  process_config (Rest, Command, [ " -z ", stringify (V) | Args ]);
process_config ([{flush_timeout, V}|Rest], Command, Args) ->
  process_config (Rest, Command, [ " -f ", stringify (V) | Args ]);
process_config ([{pidfile, V}|Rest], Command, Args) ->
  process_config (Rest, Command, [ " -p ", V | Args ]);
process_config ([{write_threads, V}|Rest], Command, Args) ->
  process_config (Rest, Command, [ " -t ", stringify (V) | Args ]);
process_config ([{journal_dir, V}|Rest], Command, Args) ->
  process_config (Rest, Command, [ " -j ", V | Args ]);
process_config ([flush_on_exit|Rest], Command, Args) ->
  process_config (Rest, Command, [ " -F " | Args ]);
process_config ([foreground|Rest], Command, Args) ->
  process_config (Rest, Command, [ " -g " | Args ]);
process_config ([{root_dir,V}|Rest], Command, Args) ->
  process_config (Rest, Command, [ " -b ", V | Args ]);
process_config ([writes_only_in_root|Rest], Command, Args) ->
  process_config (Rest, Command, [ " -B " | Args ]);
process_config ([{rrdcachedcmd,V}|Rest], _, Args) ->
  process_config (Rest, V, Args);
process_config (A,_,_) ->
  erlang:error ({badconfig, A}).

pid_file () ->
  filename:join(["","tmp",lists:concat(["rrdcached-",os:getpid(),".pid"])]).

listen_file () ->
  filename:join(["","tmp",lists:concat(["rrdcached-",os:getpid(),".sock"])]).

alive () ->
  case os:cmd (lists:concat (["kill -0 `cat ",pid_file (),"`"])) of
    [] -> true;
    _ -> false
  end.

ping () ->
  gen_server:cast (?MODULE, ping).

init (Config) ->
  process_flag(trap_exit, true),
  PrivDir = filename:join ([filename:dirname(code:which(?MODULE)),"..","priv"]),
  ExtCmd = filename:join([PrivDir, "killable "]),

  Args =
    process_config ([ foreground,
                      {listen, listen_file ()},
                      {pidfile, pid_file ()}
                      | Config]),
  Cmd = binary_to_list(list_to_binary([ExtCmd, "rrdcached ", Args])),
  Port =
    open_port({spawn, Cmd},
              [stream,{line, 10000},exit_status,stderr_to_stdout]),
  {ok, #state{port = Port}}.

handle_call (_Request, _From, State) ->
  { reply, ok, State }.

handle_cast (ping, State = #state {port = Port}) ->
  port_command (Port, ".\n"),
  { noreply, State };
handle_cast (_Request, State) ->
  { noreply, State }.

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
  { stop, {port_terminated, Reason}, State };
handle_info (_Request, State) ->
  error_logger:info_msg ("handle_info not handled ~p",[_Request]),
  { noreply, State }.

terminate({port_terminated, _Reason}, _State) ->
  ok;
terminate(_Reason, #state{port = Port} = _State) ->
  port_close(Port),
  os:cmd (lists:concat(["kill -9 `cat ",pid_file (),"`"])),
  ok.

code_change (_OldVsn, State, _Extra) ->
  { ok, State }.
