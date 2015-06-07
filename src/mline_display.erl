%% @doc <p>Server that constantly displays several rows with redraw</p>
%% <p>Server receives number of text rows and every timeout prints
%% them with special escape symbols that moves cursor to other lines.</p>

-module(mline_display).

-author("Pavel Baturko (pabftk@gmail.com)").

-behaviour(gen_server).

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
, start_display/0
, stop_display/0
, update_rows/1
, stop/0
]).

-define(SERVER, ?MODULE).

-define(ESC, <<"\e[">>).
-define(END, <<"m">>).

-record(state, {
  num_rows
, update_tmo = 500
, update_tref
, move_row_esc
, rows = []
, state = idle
, display_fun = fun() -> ok end
, do_clear_display = false
}).

-type option() ::
  do_clear_display
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

%% @doc Start display rows
-spec start_display() -> ok.
start_display() ->
  gen_server:call(?SERVER, start_display).

%% @doc Stop display rows
-spec stop_display() -> ok.
stop_display() ->
  gen_server:call(?SERVER, stop_display).

%% @doc Update rows to be displayed
-spec update_rows(list(string())) -> ok.
update_rows(Rows) ->
  gen_server:call(?SERVER, {update_rows, Rows}).

%% @doc Stop server
-spec stop() -> ok.
stop() ->
  gen_server:call(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden gen_server callback
init(Opts) ->
  Do_clear_display = proplists:get_bool(do_clear_display, Opts),
  Update_tmo = proplists:get_value(update_tmo, Opts, 500),

  Init_state = #state{
    do_clear_display = Do_clear_display
  , update_tmo = Update_tmo
  },

  {ok, Init_state}.

%% @hidden gen_server callback
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(start_display, _From, State) ->
  Tref = erlang:send_after(State#state.update_tmo, self(), display_timeout),
  New_state = State#state{
    rows = ["NO INFO"]
  , move_row_esc = num_rown_to_esc_bin(1)
  , num_rows = 1
  , state = display
  , update_tref = Tref
  },
  Display_fun = create_display_fun_from_state(New_state),
  {reply, ok, New_state#state{ display_fun = Display_fun }};

handle_call(stop_display, _From, State) ->
  cancel_timer(State#state.update_tref),
  New_state = State#state{
    state = idle
  },
  {reply, ok, New_state};

handle_call({update_rows, Rows}, _From, State) ->
  Num_rows = length(Rows),
  Move_row_esc = num_rown_to_esc_bin(Num_rows),
  New_state = State#state{
    rows = Rows
  , num_rows = Num_rows
  , move_row_esc = Move_row_esc
  },
  Display_fun = create_display_fun_from_state(New_state),
  {reply, ok, New_state#state{ display_fun = Display_fun }}.

%% @hidden gen_server callback
handle_cast(_Request, State) ->
  {noreply, State}.

%% @hidden gen_server callback
handle_info(display_timeout, State = #state{ display_fun = Display_fun }) ->
  Tref =
  case State#state.state of
  	idle -> undefined;
  	_    -> erlang:send_after(State#state.update_tmo, self(), display_timeout)
  end,
  Display_fun(),
  New_state = State#state{
    update_tref = Tref
  },
  {noreply, New_state}.

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
create_str_from_rows(Rows) ->
  lists:foldl(fun(Row, Str) ->
  	Str ++ Row ++ "  ~n" end,
  	"", Rows).

%% @hidden internal
cancel_timer(undefined) ->
  ok;
cancel_timer(Ttef) ->
  erlang:cancel_timer(Ttef).

%% @hidden internal
num_rown_to_esc_bin(Num_rows) ->
  Move_row_esc = list_to_binary(integer_to_list(Num_rows+1) ++ "A"),
  <<?ESC/binary, Move_row_esc/binary>>.

%% @hidden internal
create_display_fun_from_state(State) ->
  Str = create_str_from_rows(State#state.rows),
  Erase_str = [State#state.move_row_esc] ++ lists:flatten(lists:duplicate(State#state.num_rows+1, lists:duplicate(80, " ") ++ "||" ++ "~n")),
  Final_str = [State#state.move_row_esc] ++ lists:flatten(lists:duplicate(80, "=")) ++ "~n" ++ Str,
  case State#state.do_clear_display of
    true ->
      fun() ->
        io:format(Erase_str),
        io:format(Final_str)
      end;
    false ->
      fun() ->
        io:format(Final_str)
      end
  end.
