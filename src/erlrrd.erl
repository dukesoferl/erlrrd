-module(erlrrd).
-include_lib ("eunit/include/eunit.hrl").

-export([create/1, update/1, updatev/1, dump/1, restore/1, last/1,
         first/1, info/1, fetch/1, tune/1, resize/1, xport/1,
         graph/1, lastupdate/1, ls/0, cd/1, mkdir/1, pwd/0
         ]).

-export([start_link/1, start_link/0]).
-export([start/0]).
-export([stop/0]).
-export([combine/1, c/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record( state, { port }  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @spec start_link(RRDToolCmd) -> Result
%%   RRDToolCmd = string()
%%   Result = {ok,Pid} | ignore | {error,Error}
%%     Pid = pid()
%%     Error = {already_started,Pid} | shutdown | term()
%% @doc calls gen_server:start_link
%%   RRDToolCmd is the command passed to open_port()
%%   usually "rrdtool -"
start_link(RRDToolCmd) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, RRDToolCmd, []).
%% @equiv start_link("rrdtool -")
start_link() ->
  start_link("rrdtool -").



%% @spec combine(List) -> List
%%   List = [ term() ]
%% @doc "joins" and quotes the given arg list. 
%%   takes a list of arguments, and returns a deeplist with 
%%   each argument surrounded by double quotes
%%   then separated by spaces
%%
%%   combine(["these", "are", "my args"]). ->
%%
%%   [["\"","these","\""]," ",["\"","are","\""]," ",["\"","my args","\""]]
%%
%%   it is intended as a convinence function to the 
%%   rrdtool commands which all take a single iodata() argument
%%   which represents the string to be passed as the arguments 
%%   to the corresponding rrdtool command.  
%%
%%   erlrrd:xport(erlrrd:c(["DEF:foo=/path with/space/foo.rrd:foo:AVERAGE", "XPORT:foo"])).
combine(Args) -> 
    join([ [ "\"", X, "\"" ] || X <- Args ], " ").
%% @spec c(List) -> List
%%   List = [ term() ]
% @equiv combine(Args)
c(Args) -> combine(Args).

c_test_() -> 
  [
    ?_test(
      [ 
        ["\"", "these", "\""], " ",
        ["\"", "are",   "\""], " ",
        ["\"", "my",    "\""], " ",
        ["\"", "args",  "\""]  
      ] = combine(["these", "are", "my", "args"])),
    ?_test([[ "\"", "a", "\""]] = combine(["a"]))
  ].


% rrdtool commands

%% @spec create(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc Set up a new Round Robin Database (RRD). Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdcreate.en.html rrdcreate].
create     (Args) when is_list(Args) -> do(create,     Args).

%% @spec update(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc Store new data values into an RRD. Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdupdate.en.html rrdupdate].
update     (Args) when is_list(Args) -> do(update,     Args).

%% @spec updatev(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc Operationally equivalent to update except for output. Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdupdate.en.html rrdupdate].
updatev    (Args) when is_list(Args) -> do(updatev,    Args).

%% @spec dump(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc    Dump the contents of an RRD in plain ASCII. In connection with
%%         restore you can use this to move an RRD from one computer
%%         architecture to another.  Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrddump.en.html rrddump].
dump       (Args) when is_list(Args) -> do(dump,       Args).

%% @spec restore(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc Restore an RRD in XML format to a binary RRD. Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdrestore.en.html rrdrestore]
restore    (Args) when is_list(Args) -> do(restore,    Args).

%% @spec last(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc    Return the date of the last data sample in an RRD. Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdlast.en.html rrdlast]
last       (Args) when is_list(Args) -> do(last,       Args).

%% @spec lastupdate(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc    Return the most recent update to an RRD. Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdlastupdate.en.html rrdlastupdate]
lastupdate (Args) when is_list(Args) -> do(lastupdate, Args).

%% @spec first(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc Return the date of the first data sample in an RRA within an
%%       RRD. Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdfirst.en.html rrdfirst]
first      (Args) when is_list(Args) -> do(first,      Args).

%% @spec info(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc Get information about an RRD. Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdinfo.en.html rrdinfo].
info       (Args) when is_list(Args) -> do(info,       Args).

%% @spec fetch(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc   Get data for a certain time period from a RRD. The graph func-
%%         tion uses fetch to retrieve its data from an RRD. Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdfetch.en.html rrdfetch].
fetch      (Args) when is_list(Args) -> do(fetch,      Args).

%% @spec tune(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc Alter setup of an RRD. Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdtune.en.html rrdtune].
tune       (Args) when is_list(Args) -> do(tune,       Args).

%% @spec resize(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc    Change the size of individual RRAs. This is dangerous! Check
%%         rrdresize.
resize     (Args) when is_list(Args) -> do(resize,     Args).

%% @spec xport(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc   Export data retrieved from one or several RRDs. Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdxport.en.html rrdxport]
%%
%%  erlrrd:xport("'DEF:foo=/path with/space/foo.rrd:foo:AVERAGE' XPORT:foo").
%%
%%  erlrrd:xport(erlrrd:c(["DEF:foo=/path with/space/foo.rrd:foo:AVERAGE", "XPORT:foo"])).
xport      (Args) when is_list(Args) -> do(xport,      Args).

%% @spec graph(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc   Create a graph from data stored in one or several RRDs. Apart
%%         from generating graphs, data can also be extracted to stdout.
%%         Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdgraph.en.html rrdgraph].
graph      (Args) when is_list(Args) -> 
  % TODO: regexp:match needs a string, will break w/binarys
  %   lists:flatten needs a deeplist, will break with iolists
  %   fix both of these.
  case regexp:match(lists:flatten(Args), " -( |$)") of
    { match, _, _ } -> 
      % graph to stdout will break this Ports parsing of reponses..
      { error, "Graphing to stdout not supported." };
    nomatch -> 
      do(graph, Args)
  end.

% rrd "remote" commands
%% @spec cd(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc   ask the rrdtool unix process to change directories
%% 
%%    erlrrd:cd("/usr/share/rrd/data").
%%
%%    erlrrd:cd(erlrrd:combine(["/Users/foo/Library/Application Support/myapp/rrd"]).
cd         (Arg)  when is_list(Arg) -> do(cd,         Arg).

%% @spec mkdir(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc   ask the rrdtool unix process to create a directory
mkdir      (Arg)  when is_list(Arg) -> do(mkdir,      Arg).

%% @spec ls() -> { ok, Response }  | { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc  lists all *.rrd files in rrdtool unix process'
%%       current working directory
ls         ()     -> do(ls,         []  ).

%% @spec pwd() -> { ok, Response }  |  { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc  return the rrdtool unix process'
%%       current working directory.
pwd        ()     -> do(pwd,        []  ).



%% @hidden
%% @equiv start("rrdtool -")
start() -> start("rrdtool -").
%% @hidden
%% @spec start(Args) -> any()
%% @doc start the rrdtool gen_server
%%    calls gen_server:start
start(RRDToolCmd)      -> gen_server:start     ({local, ?MODULE}, ?MODULE, RRDToolCmd, []).
%% @hidden
%% @spec stop() -> any()
%% @doc stop the rrdtool gen_server
stop()       -> gen_server:call      (?MODULE, stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  eunit,  test starting and stopping.
%% 
test_start_stop_(StartFun, StopFun, Tag) -> 
  { spawn, 
    { inorder,
      [
        check_stopped_(Tag),
        { Tag, setup,
          StartFun,
          StopFun,
          check_started_(Tag) 
        },
        check_stopped_(Tag)
      ]
    }
  }.

check_stopped_(Tag) ->
  wrap_tag_(Tag,
    [
      ?_assertExit( { noproc, _ }, pwd()),
      ?_assertExit( { noproc, _ }, mkdir("erlrrdtestdir")),
      ?_assertExit( { noproc, _ }, ls())
    ]
  ).

stop_helper_(Pid) ->
  true = exit(Pid, normal),
  receive 
    {'EXIT', Pid, Reason} -> Reason
  after 3000 -> 
    throw({ timeout, Pid, "Pid not responding to EXIT?"})
  end.

start_link_test_() -> 
  test_start_stop_(
    fun() -> {ok,Pid} = start_link(), Pid end,
    fun stop_helper_/1,
    "start_link test" 
  ).


start_stop_test_() ->
  test_start_stop_(
    fun()  -> {ok, _Pid} = start() end,
    fun(_) -> stopped    = stop()  end,
    "start/0 stop/0"
  ).

start_sup_test_() ->
  test_start_stop_(
    fun() -> 
      {ok,Pid} = erlrrd_sup:start_link(),
      Pid 
    end,
    fun stop_helper_/1,
    "sup:start_link/0 exit/2" 
  ).

start_sup2_test_() ->
  test_start_stop_(
    fun() -> 
      check_cwd_helper_(),
      {ok,Pid} = erlrrd_sup:start_link("./dummyrrdtool"),
      Pid 
    end,
    fun stop_helper_/1,
    "sup:start_link/1 exit/2" 
  ).

start_app_test_() -> 
  test_start_stop_(
    fun()  -> ok = erlrrd_app:start() end,
    fun(_) -> ok = erlrrd_app:stop()  end,
    "app:start/0 app:stop/0"
  ).

wrap_tag_(T,L) when is_list(L) -> 
  [ { T, X } || X <- L ].

check_started_(Tag) ->
  wrap_tag_(Tag,
    [
      ?_test(ls()),
      ?_test(pwd()),
      ?_test(ok),
      ?_test(ok)
    ] 
  ).

% check if the dir we're in end's in /tests
check_cwd_helper_() ->
   { ok, L } = file:get_cwd(),  
   "stset/" ++ _  = lists:reverse(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  eunit,  test interfaces

datain_dataout_test_() ->
  RRDFile = "foo.rrd",
  PNGFile = "out.png",
  Now = time_since_epoch(),
  Then = Now - 86400,
  StepSize = 300,
  Steps = round((Now - Then) / StepSize),
  { setup,
    fun()  -> 
      check_cwd_helper_(),
      ok = file:delete(RRDFile),
      { error, enoent } = file:read_file_info(RRDFile),
      {ok, _Pid} = start() 
    end,
    fun(_) -> 
      stopped = stop(),
      ok %= file:delete(RRDFile)
    end,
    { inorder,
      [
        % create an rrd
        fun() ->
          {ok, _ } = erlrrd:create([
            io_lib:fwrite("~s --start ~B", [RRDFile, Then]),
            io_lib:fwrite(" --step ~B DS:thedata:GAUGE:~B:U:U",
              [ StepSize, StepSize ]),
            io_lib:fwrite(" RRA:AVERAGE:0.5:1:~B",[Steps])
          ])
        end,
        % write sin wave to rrd
        fun() -> 
          lists:foreach(
            fun(X) ->
              Pi = math:pi(),
              P = 5 * ( 
                math:sin( 3*Pi/2 + X/(Steps / (8*Pi) )  )
                + 1
              ),

               %io:format(user, "~s ~B:~f~n", [ RRDFile, Then + X * StepSize, P ]),
              {ok, _ } = erlrrd:update(
                io_lib:format("~s ~B:~f", [ RRDFile, Then + X * StepSize, P ])
              )
            end,
            lists:seq(1, Steps)
          )
        end,
        % make a graph!! :)
        fun() -> 
          { ok, _ } = erlrrd:graph([
            "-l 0 -r", " ",
            "-w 700 -h 200 -a PNG ", PNGFile,
            " DEF:thedata=foo.rrd:thedata:AVERAGE AREA:thedata#CC9945" 
          ])
          % ok, now how can we check the graph???  hmm.
        end,
        % export rrd
        % compare
        fun() -> commas_are_cool end
      ]
    }
  }.

time_since_epoch() ->
  calendar:datetime_to_gregorian_seconds(erlang:universaltime()) - ( 719528 * 86400 ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen server interface poo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @hidden
init(RRDToolCmd) -> 
  process_flag(trap_exit, true),
  Port = erlang:open_port({spawn, RRDToolCmd}, [ {line, 10000}, eof, exit_status, stream ] ),
  {ok, #state{port = Port}}.

%%
%% handle_call
%% @hidden
handle_call({do, Action, Args }, _From, #state{port = Port} = State) ->
    Line = [ erlang:atom_to_list(Action), " ", Args , "\n"],
    port_command(Port, Line),
    case collect_response(Port) of
        {response, Response} -> 
            {reply, { ok, Response }, State};
        { error, timeout } ->
            {stop, port_timeout, State};
        { error, Error } -> 
            {reply, { error, Error  }, State}
    end;

handle_call(stop, _From, State) -> 
  {stop, normal, stopped, State}.

%% handle_cast
%% @hidden
handle_cast(_Msg, State) -> {noreply, State}.

%% handle_info
%% @hidden
handle_info(_Info, State) -> {noreply, State}.

%% terminate
%% @hidden
terminate(_Reason, _State) -> ok.

%% code_change
%% @hidden
code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private poo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do(Command, Args) -> 
  case has_newline(Args) of
    true  -> { error, "No newlines" };
    false -> gen_server:call (?MODULE, { do, Command, Args } ) 
  end.

join([Head | [] ], _Sep) ->
  [Head];
join([Head | Tail], Sep) ->
  [ Head, Sep | join(Tail, Sep) ].

join_test() -> 
  [ "a", " ", "b", " ", "c"] = join(["a", "b", "c"], " ").
join_test_() ->
  [ 
    ?_test([ "a", " ", "b", " ", "c"] = join(["a", "b", "c"], " ")),
    ?_assertNot([ "a", "b", " ", "c"]  =:= join(["a", "b", "c"], " "))
  ].

has_newline([]) -> false;
has_newline(<<>>) -> false;
has_newline([ H |  T]) 
  when is_list(H); is_binary(H) ->
    case has_newline(H) of
      true -> true;
      false -> has_newline(T)
    end;
has_newline([ H | T]) when is_integer(H) ->
  if 
    H =:= $\n -> true;
    true -> has_newline(T)
  end;
has_newline(<<H:8,T/binary>>) ->
  if 
    H =:= $\n -> true;
    true -> has_newline(T)
  end.

has_newline_test_() ->
  [ 
    ?_test( true  = has_newline("\n")),
    ?_test( true  = has_newline(["\n"])),
    ?_test( true  = has_newline(["these", ["are", [ "my args" ] | <<"newline\n">> ], "so", "there"])),
    ?_test( false = has_newline(["these", ["are", [ "my args" ] | <<"newline">> ], "so", "there"])),
    ?_test( true  = has_newline(["these\n", ["are", [ "my args" ] | <<"newline">> ], "so", "there"])),
    ?_test( true  = has_newline(
      ["these", ["are", [ "my args" | [[[[[[[[ "blah\n"]]]]]]]] ] | <<"newline">> ], "so", "there"])),
    ?_test( false = has_newline("")),
    ?_test( false = has_newline([])),
    ?_test( false = has_newline(<<>>))
  ].


collect_response(Port) ->
    collect_response(Port, [], []).

collect_response( Port, RespAcc, LineAcc) ->
    receive
        {Port, {data, {eol, "OK u:" ++ _T }}} ->
            {response, lists:reverse(RespAcc)};
        {Port, {data, {eol, "ERROR: " ++ Error }}} ->
            {error, [ Error, lists:reverse(RespAcc)]};
        {Port, {data, {eol, Result}}} ->
            Line = lists:reverse([Result | LineAcc]),
            collect_response(Port, [Line | RespAcc], []);
        {Port, {data, {noeol, Result}}} ->
            collect_response(Port, RespAcc, [Result | LineAcc])

    %% Prevent the gen_server from hanging indefinitely in case the
    %% spawned process is taking too long processing the request.
    after 3000 ->  % TODO user configurable timeout.
            { error, timeout }
    end.
