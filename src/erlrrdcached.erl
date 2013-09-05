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

-record(state, {port, pid_file, listen_file}).

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
process_config ([{base_dir,V}|Rest], Command, Args) ->
  process_config (Rest, Command, [ " -b ", V | Args ]);
process_config ([writes_only_in_root|Rest], Command, Args) ->
  process_config (Rest, Command, [ " -B " | Args ]);
process_config ([{rrdcachedcmd,V}|Rest], _, Args) ->
  process_config (Rest, V, Args);
process_config (A,_,_) ->
  erlang:error ({badconfig, A}).

pid_file () ->
  gen_server:call (?MODULE, pid_file).

listen_file () ->
  gen_server:call (?MODULE, listen_file).

alive () ->
  case os:cmd (lists:concat (["kill -0 `cat ",pid_file (),"`"])) of
    [] -> true;
    _ -> false
  end.

ping () ->
  gen_server:cast (?MODULE, ping).

init (Config0) ->
  process_flag(trap_exit, true),
  PrivDir = filename:join ([filename:dirname(code:which(?MODULE)),"..","priv"]),
  ExtCmd = filename:join([PrivDir, "killable "]),

  { RootDir, Config1 } =
    case lists:keytake (root_dir, 1, Config0) of
      {value, {root_dir, R}, C1} -> {R, C1};
      false -> { ["", "var", "run" ], Config0 }
    end,

  { ListenFile, Config2 } =
    case lists:keytake (listen, 1, Config1) of
      {value, {listen, L}, C2} -> {L, C2};
      false ->
        { filename:join (lists:concat ([RootDir,
              [lists:concat(["rrdcached-",os:getpid(),".sock"])]])),
          Config1
        }
    end,

  { PidFile, Config3 } =
    case lists:keytake (pidfile, 1, Config2) of
      {value, {pidfile, P}, C3} -> {P, C3};
      false ->
        { filename:join (lists:concat ([RootDir,
              [lists:concat(["rrdcached-",os:getpid(),".pid"])]])),
          Config2
        }
    end,

  Args =
    process_config ([ foreground,
                      {listen, ListenFile},
                      {pidfile, PidFile}
                      | Config3
                    ]),
  Cmd = binary_to_list(list_to_binary([ExtCmd, "rrdcached ", Args])),
  Port =
    open_port({spawn, Cmd},
              [stream,{line, 10000},exit_status,stderr_to_stdout]),

  {ok, #state{port = Port, pid_file = PidFile, listen_file = ListenFile}}.

handle_call (pid_file, _From, State = #state { pid_file = PidFile }) ->
  { reply, PidFile, State };
handle_call (listen_file, _From, State = #state { listen_file = ListenFile}) ->
  { reply, ListenFile, State };
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
terminate(_Reason, #state{port = Port, pid_file = PidFile} = _State) ->
  port_close(Port),
  os:cmd (lists:concat(["kill -9 `cat ",PidFile,"`"])),
  ok.

code_change (_OldVsn, State, _Extra) ->
  { ok, State }.
