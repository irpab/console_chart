%% @doc <p>Server that allows to create simple text (ASCII) line chart of some value over time.</p>
%% <p>It receives new value via API call (rate of new values == chart refresh rate
%% - depends on user) and draws corresponding level in cell of "text matrix" (number of lines).
%% Current chart will be moved left with new value.</p>
%% <p>For printing chart external module should be used which will receive text lines to print.</p>

-module(console_chart).

-author("Pavel Baturko (pabftk@gmail.com)").

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%% API
-export([
  start_link/0
, start_link/1
, new_value/1
, print/0
, print_ret/0
, stop/0
]).

%% For tests only
-export([
  update_rows/1
]).

-define(SERVER, ?MODULE).

-record(state, {
  rows = []
, level_min
, level_max
, level_dt
, num_levels
, last_level = 1
, width
, print_mod
, scale_column
, scale_row
}).

-type option() ::
  {num_levels, pos_integer()}
| {level_min, integer()}
| {level_max, integer()}
| {width, pos_integer()}
| {print_mod, atom()}
| {num_markers, non_neg_integer()}
.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start server with default parameters
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Start server
-spec start_link(list(option())) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Add new value to chart
-spec new_value(number()) -> ok.
new_value(V) ->
  gen_server:call(?SERVER, {new_value, V}).

%% @doc Return current chart via list of lines
-spec print_ret() -> list(string()).
print_ret() ->
  {ok, Rows} = gen_server:call(?SERVER, print),
  Rows.

%% @doc Print current chart via io:format
-spec print() -> ok.
print() ->
  Rows = print_ret(),
  [io:format(Row ++ "~n") || Row <- Rows],
  io:format("~n"),
  ok.

%% @doc Stop server
-spec stop() -> ok.
stop() ->
  gen_server:call(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden gen_server callback
init(Opts) ->
  Num_levels  = proplists:get_value(num_levels    , Opts, 50),
  Level_min   = proplists:get_value(level_min     , Opts, 0),
  Level_max   = proplists:get_value(level_max     , Opts, 100),
  Width       = proplists:get_value(width         , Opts, 100),
  Print_mod   = proplists:get_value(print_mod     , Opts, mline_display),
  Num_markers = proplists:get_value(num_markers   , Opts, 1),

  Scale_column = calc_scale_column(Num_levels, Level_min, Level_max, Num_markers),

  Init_state = #state{
    rows         = lists:duplicate(Num_levels, lists:flatten(lists:duplicate(Width, " ")))
  , level_min    = Level_min
  , level_max    = Level_max
  , level_dt     = (Level_max - Level_min) div Num_levels
  , num_levels   = Num_levels
  , width        = Width
  , print_mod    = Print_mod
  , scale_column = Scale_column
  , scale_row    = lists:flatten(lists:duplicate(Width, " "))
  },
  {ok, Init_state}.

%% @hidden gen_server callback
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call({new_value, New_value}, _From, State = #state{
        rows         = Rows0
      , level_min    = Level_min
      , level_dt     = Level_dt
      , num_levels   = Num_levels
      , last_level   = Last_level
      , print_mod    = Print_mod
      , scale_column = Scale_column
      , scale_row    = Scale_row
    }) ->
  New_level = level_for_value(New_value, Level_min, Num_levels, Level_dt),
  Diff_with_last_level = Last_level - New_level,
  Symbol_to_draw = line_sym_for_dt_lev(Diff_with_last_level),
  Level_to_draw =
  if Diff_with_last_level < 0 ->
    New_level-1;
  true ->
    New_level
  end,

  {Vert_added, Rows1} =
  if abs(Diff_with_last_level) > 1 ->
    {true, add_vertical_line(Rows0, Level_to_draw, Last_level)};
  true ->
    {false, Rows0}
  end,

  Rows2 = add_sym_to_rows(Rows1, Symbol_to_draw, Level_to_draw),
  Rows3 = case Vert_added of
    true  -> [Rest_row || [_, _ | Rest_row] <- Rows2];
    false -> [Rest_row || [   _ | Rest_row] <- Rows2]
  end,
  Rows4 = Rows3 ++ [Scale_row],
  Rows5 = add_scale_column(Scale_column, Rows4),

  New_state = State#state{
    rows = Rows3
  , last_level = New_level
  },

  Print_mod:update_rows(lists:reverse(Rows5)),

  {reply, ok, New_state};

handle_call(print, _From, State) ->
  {reply, {ok, lists:reverse(State#state.rows)}, State}.

%% @hidden gen_server callback
handle_cast(_Request, State) ->
  {noreply, State}.

%% @hidden gen_server callback
handle_info(_, State = #state{}) ->
  {noreply, State}.

%% @hidden gen_server callback
terminate(_Reason, _State) ->
  ok.

%% @hidden gen_server callback
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%===================================================================
%%% Internal funs
%%%===================================================================

%% @hidden internal
level_for_value(_V, _Level_min, 1, _Level_dt) ->
  1;
level_for_value(V, Level_min, Level, Level_dt) when V >= (Level_min+(Level-1)*Level_dt) ->
  Level;
level_for_value(V, Level_min, Level, Level_dt) ->
  level_for_value(V, Level_min, Level-1, Level_dt).

%% @hidden internal
line_sym_for_dt_lev( 0) -> "_";
line_sym_for_dt_lev(N) when N < 0 -> "/";
line_sym_for_dt_lev(N) when N > 0 -> "\\".

%% @hidden internal
add_sym_to_rows(Rows, Char, LevV) ->
  add_sym_to_rows2([], 1, Rows, Char, LevV).

%% @hidden internal
add_sym_to_rows2(New_rows, _, [], _, _) ->
  lists:reverse(New_rows);
add_sym_to_rows2(New_rows, CLev, [Row | Rows], Char, LevV) ->
  Char_add =
  if CLev == LevV ->
    Char;
  true ->
    " "
  end,
  New_row = Row ++ Char_add,
  add_sym_to_rows2([New_row | New_rows], CLev+1, Rows, Char, LevV).

%% @hidden Draw vertical line if delta between new and last levels > 1
add_vertical_line(Row0, Lev1, Lev0) when Lev1 > Lev0 ->
  add_vertical_line2([], 1, Row0, Lev1, Lev0, up);
add_vertical_line(Row0, Lev1, Lev0) when Lev1 < Lev0 ->
  add_vertical_line2([], 1, Row0, Lev0, Lev1, down).

%% @hidden internal
add_vertical_line2(New_rows, _, [], _, _, _) ->
  lists:reverse(New_rows);
add_vertical_line2(New_rows, CLev, [Row | Rows], Lev1, Lev0, Dir) ->
  Cond_min =
  case Dir of
    up   -> CLev >= Lev0;
    down -> CLev >  Lev0
  end,
  Cond_max = CLev < Lev1,
  Char_add =
  if Cond_min and Cond_max ->
    "|";
  true ->
    " "
  end,
  New_row = Row ++ Char_add,
  add_vertical_line2([New_row | New_rows], CLev+1, Rows, Lev1, Lev0, Dir).

%% @hidden Create left column with scale markers (min + max [+ intermediate markers])
calc_scale_column(Num_rows, Level_min, Level_max, Num_markers) ->
  Level_values_range = Level_max - Level_min,
  Scale_markers = prepare_scale_marker(Num_rows, Level_min, Level_values_range, Num_markers),
  calc_scale_column2([], 1, Num_rows+1, Scale_markers).

%% @hidden internal
calc_scale_column2(Scale_rows, Cur_level, Num_rows, _Level_markers) when Cur_level-1 == Num_rows ->
  lists:reverse(Scale_rows);
calc_scale_column2(Scale_rows, Cur_level, Num_rows, Level_markers) ->
  Marker_width = 5,
  Marker =
  case marker_for_level(Cur_level, Level_markers) of
    none -> lists:flatten(lists:duplicate(Marker_width, " "));
    Val  -> lists:flatten(io_lib:format("\~" ++ integer_to_list(Marker_width) ++ "w", [Val]))
  end,
  calc_scale_column2([Marker | Scale_rows], Cur_level+1, Num_rows, Level_markers).

%% @hidden Calculate for which scale point print scale markers - [0..1]. 0 - min, 1 - max.
prepare_relative_marker(Num_markers) ->
  [(M-1)/(Num_markers-1) || M <- lists:seq(1, Num_markers)].

%% @hidden Calculate level and scale marker value which will be printed in scale column
prepare_scale_marker(Num_rows, Level_min, Level_values_range, Num_intermediate_markers0) ->
  Num_mandatory_markers = 2, % min and max markers
  Num_markers = Num_intermediate_markers0 + Num_mandatory_markers,
  Relative_markers = prepare_relative_marker(Num_markers),
  [{round(Num_rows * N)+1, round(Level_min + Level_values_range * N)} || N <- Relative_markers].

%% @hidden internal
marker_for_level(_Cur_level, []) ->
  none;
marker_for_level(Cur_level, [{Cur_level, Marker} | _Level_markers]) ->
  Marker;
marker_for_level(Cur_level, [_ | Level_markers]) ->
  marker_for_level(Cur_level, Level_markers).

%% @hidden internal
add_scale_column(L1, L2) ->
  add_scale_column([], L1, L2).
add_scale_column(L, [], []) ->
  lists:reverse(L);
add_scale_column(L, [H1 | L1], [H2 | L2]) ->
  add_scale_column([(H1++H2) | L], L1, L2).


%%%===================================================================
%%% EUnit
%%%===================================================================

prepare_relative_marker_test() ->
  ?assertEqual([0.0, 0.25, 0.5, 0.75, 1.0]       , prepare_relative_marker(5)),
  ?assertEqual([0.0, 1.0]                        , prepare_relative_marker(2)),
  ?assertEqual([0.0, 0.5, 1.0]                   , prepare_relative_marker(3)),
  ?assertEqual([0.0, 1/3, 2/3, 1.0]              , prepare_relative_marker(4)),
  ok.

prepare_scale_marker_test() ->
  ?assertEqual([{1,0}, {14,25}, {26,50}, {39,75}, {51,100}] , prepare_scale_marker(50, 0, 100, 3)),
  ?assertEqual([{1,0}, {4,3}, {6,5}, {9,8}, {11,10}]        , prepare_scale_marker(10, 0, 10, 3)),
  ?assertEqual([{1,0}, {4,5}, {6,10}]                       , prepare_scale_marker(5, 0, 10, 1)),
  ok.

marker_for_level_test() ->
  ?assertEqual(none , marker_for_level(1, [{2, 10}, {4, 20}, {5, 30}])),
  ?assertEqual(10   , marker_for_level(2, [{2, 10}, {4, 20}, {5, 30}])),
  ?assertEqual(none , marker_for_level(3, [{2, 10}, {4, 20}, {5, 30}])),
  ?assertEqual(20   , marker_for_level(4, [{2, 10}, {4, 20}, {5, 30}])),
  ?assertEqual(30   , marker_for_level(5, [{2, 10}, {4, 20}, {5, 30}])),
  ?assertEqual(none , marker_for_level(6, [{2, 10}, {4, 20}, {5, 30}])),
  ok.

line_sym_for_dt_lev_test() ->
  ?assertEqual("_"  , line_sym_for_dt_lev(   0)),
  ?assertEqual("/"  , line_sym_for_dt_lev(  -1)),
  ?assertEqual("\\" , line_sym_for_dt_lev(   1)),
  ?assertEqual("/"  , line_sym_for_dt_lev(  -6)),
  ?assertEqual("\\" , line_sym_for_dt_lev(1215)),
  ok.

level_for_value_test() ->
  ?assertEqual(1 , level_for_value(  0, 0, 10, 2)),
  ?assertEqual(1 , level_for_value(  1, 0, 10, 2)),
  ?assertEqual(1 , level_for_value(1.9, 0, 10, 2)),

  ?assertEqual(2 , level_for_value(2.0, 0, 10, 2)),
  ?assertEqual(2 , level_for_value(2.1, 0, 10, 2)),
  ?assertEqual(2 , level_for_value(3.9, 0, 10, 2)),

  ?assertEqual(9 , level_for_value(17.0, 0, 10, 2)),
  ?assertEqual(9 , level_for_value(17.1, 0, 10, 2)),
  ?assertEqual(9 , level_for_value(17.9, 0, 10, 2)),

  ?assertEqual(10 , level_for_value(19.0, 0, 10, 2)),
  ?assertEqual(10 , level_for_value(19.1, 0, 10, 2)),
  ?assertEqual(10 , level_for_value(19.9, 0, 10, 2)),

  % below min level
  ?assertEqual(1 , level_for_value( -0.1, 0, 10, 2)),
  ?assertEqual(1 , level_for_value(   -1, 0, 10, 2)),
  ?assertEqual(1 , level_for_value(-10.5, 0, 10, 2)),

  % above max level
  ?assertEqual(10 , level_for_value( 20 , 0, 10, 2)),
  ?assertEqual(10 , level_for_value(20.1, 0, 10, 2)),
  ?assertEqual(10 , level_for_value(100 , 0, 10, 2)),
  ok.

calc_scale_column_test() ->
  % calc_scale_column(Num_levels, Level_min, Level_max, Num_markers),
  ?assertEqual(["    0", "     ", "     ", "    5", "     ", "   10"], calc_scale_column(5, 0, 10, 1)),
  ?assertEqual(["    0"
    , "     ", "     ", "    3"
    , "     ", "    5", "     "
    , "     ", "    8", "     "
    , "   10"], calc_scale_column(10, 0, 10, 3)),
  ?assertEqual(["    0", "    1", "    2", "    3", "    4", "    5"], calc_scale_column(5, 0, 5, 4)),
  ok.

add_scale_column_test() ->
  ?assertEqual(["1ab", "2cd", "3ef"], add_scale_column(["1", "2", "3"], ["ab", "cd", "ef"])),
  ?assertEqual(["1xa", "2yc", "3ze"], add_scale_column(["1x", "2y", "3z"], ["a", "c", "e"])),
  ok.

add_vertical_line_test() ->
  ?assertEqual(["|", "|", "|", " ", " "], add_vertical_line(["", "", "", "", ""], 4, 1)),
  ?assertEqual([" ", "|", " ", " ", " "], add_vertical_line(["", "", "", "", ""], 3, 2)),
  ?assertEqual([" ", " ", "|", "|", " "], add_vertical_line(["", "", "", "", ""], 2, 5)),
  ?assertEqual(["1a ", "2b|", "3c|", "4d|", "5e "], add_vertical_line(["1a", "2b", "3c", "4d", "5e"], 1, 5)),
  ok.

add_sym_to_rows_test() ->
  ?assertEqual(["1ax", "2b ", "3c ", "4d ", "5e "], add_sym_to_rows(["1a", "2b", "3c", "4d", "5e"], "x", 1)),
  ?assertEqual(["1a ", "2b ", "3cx", "4d ", "5e "], add_sym_to_rows(["1a", "2b", "3c", "4d", "5e"], "x", 3)),
  ?assertEqual(["1a ", "2b ", "3c ", "4d ", "5ex"], add_sym_to_rows(["1a", "2b", "3c", "4d", "5e"], "x", 5)),
  ?assertEqual(["1ax"], add_sym_to_rows(["1a"], "x", 1)),
  ok.

update_rows_test() ->
  ?assertEqual(ok, update_rows([])),
  ok.

gen_server_test_() ->
  {foreach, fun setup/0, fun cleanup/1, [
      fun(Pid) -> fun() -> server_is_alive(Pid) end end
    , fun(Pid) -> fun() -> check_print(Pid) end end
    , fun(Pid) -> fun() -> stop_server(Pid) end end
  ]}.

gen_server_defaults_test_() ->
  {foreach, fun setup_def/0, fun cleanup/1, [
      fun(Pid) -> fun() -> server_is_alive(Pid) end end
    , fun(Pid) -> fun() -> stop_server(Pid) end end
  ]}.

setup() ->
  process_flag(trap_exit, true),
  Opt = [{num_levels, 5}, {level_min, 0}, {level_max, 10}, {width, 5}, {print_mod, ?MODULE}],
  {ok, Pid} = ?MODULE:start_link(Opt),
  Pid.

setup_def() ->
  process_flag(trap_exit, true),
  {ok, Pid} = ?MODULE:start_link(),
  Pid.

server_is_alive(Pid) ->
  ?assertEqual(true, is_process_alive(Pid)),
  ?assertEqual(true, is_process_alive(whereis(?SERVER))).

stop_server(Pid) ->
  ?assertEqual(ok, ?MODULE:stop()),
  ?assertEqual(false, is_process_alive(Pid)).

cleanup(Pid) ->
  exit(Pid, kill),
  ?assertEqual(false, is_process_alive(Pid)).

update_rows(_) -> ok.

check_print(_Pid) ->
  ?assertEqual(ok, ?MODULE:print()),
  ?assertEqual(["     "
              , "     "
              , "     "
              , "     "
              , "     "], ?MODULE:print_ret()),

  ?assertEqual(ok, ?MODULE:new_value(0)),
  ?assertEqual(["     "
              , "     "
              , "     "
              , "     "
              , "    _"], ?MODULE:print_ret()),

  ?assertEqual(ok, ?MODULE:new_value(0)),
  ?assertEqual(["     "
              , "     "
              , "     "
              , "     "
              , "   __"], ?MODULE:print_ret()),

  ?assertEqual(ok, ?MODULE:new_value(2)),
  ?assertEqual(["     "
              , "     "
              , "     "
              , "     "
              , "  __/"], ?MODULE:print_ret()),

  ?assertEqual(ok, ?MODULE:new_value(3)),
  ?assertEqual(["     "
              , "     "
              , "     "
              , "    _"
              , " __/ "], ?MODULE:print_ret()),

  ?assertEqual(ok, ?MODULE:new_value(5.5)),
  ?assertEqual(["     "
              , "     "
              , "     "
              , "   _/"
              , "__/  "], ?MODULE:print_ret()),

  ?assertEqual(ok, ?MODULE:new_value(9.5)),
  ?assertEqual(["     "
              , "    /"
              , "   | "
              , " _/  "
              , "/    "], ?MODULE:print_ret()),

  ?assertEqual(ok, ?MODULE:new_value(11)),
  ?assertEqual(["    _"
              , "   / "
              , "  |  "
              , "_/   "
              , "     "], ?MODULE:print_ret()),

  ?assertEqual(ok, ?MODULE:new_value(-21)),
  ?assertEqual(["  _  "
              , " / | "
              , "|  | "
              , "   | "
              , "    \\"], ?MODULE:print_ret()),

  ok.
