#!/usr/bin/env escript

main(Cmdl_opt) ->
  {Type, T} =
  case Cmdl_opt of
    [X] -> {X, 1000};
    [X, Y] -> {X, list_to_integer(Y)};
    _ -> {"cpu", 1000}
  end,

  ok = application:start(sasl),
  ok = application:start(os_mon),

  {Max_level, F} =
  case Type of
    "cpu" ->
      {100, fun() -> get_cpu() end};
    "mem" ->
      {get_total_mem(), fun() -> get_free_mem() end}
  end,

  % num_levels - use 25 rows in console
  % width - 100 chars width
  % level_min - min value that can be displayed
  % level_max - max value that can be displayed
  % num_markers - how many intermediate scale markers to show.
  %   in case of min/max values 0/100 and num_markers=3 left scale
  % column will show levels 0, 25, 50, 75, 100.
  Opts = [{num_levels, 25}, {level_min, 0}, {level_max, Max_level}, {width, 100}, {num_markers, 3}],
  ok = application:start(console_chart),
  ok = console_chart_sup:start_display(Opts),
  loop(T, F, Opts).

get_total_mem() ->
  Mem_data = memsup:get_system_memory_data(),
  Total_memory = proplists:get_value(total_memory, Mem_data),
  Total_memory div (1024 * 1024).

get_free_mem() ->
  Mem_data = memsup:get_system_memory_data(),
  Free_memory = proplists:get_value(free_memory, Mem_data),
  Free_memory div (1024 * 1024).

get_cpu() ->
  round(cpu_sup:util()) * 100 div 100.

loop(T, F, Opts) ->
  New_value = F(),
  console_chart:new_value(New_value),
  timer:sleep(T),
  loop(T, F, Opts).
