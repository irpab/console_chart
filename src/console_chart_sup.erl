-module(console_chart_sup).

-behaviour(supervisor).

%% API
-export([
  start_link/0
, start_display/0
, start_display/1
, stop_display/0
]).

-define(SERVER, ?MODULE).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, A), {I, {I, start_link, A}, permanent, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_display() ->
  start_display([]).

start_display(Opts) ->
  Mline_display_spec = ?CHILD(mline_display, [Opts]),
  Console_chart_spec = ?CHILD(console_chart, [Opts]),

  {ok, _} = supervisor:start_child(?SERVER, Mline_display_spec),
  {ok, _} = supervisor:start_child(?SERVER, Console_chart_spec),

  ok = mline_display:start_display(),

  ok.

stop_display() ->
  ok = supervisor:terminate_child(?SERVER, mline_display),
  ok = supervisor:terminate_child(?SERVER, console_chart),

  ok = supervisor:delete_child(?SERVER, mline_display),
  ok = supervisor:delete_child(?SERVER, console_chart),

  ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @hidden supervisor callback
init(_) ->
  {ok, { {one_for_one, 5, 10}, []} }.
