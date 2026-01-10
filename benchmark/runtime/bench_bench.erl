-module(bench_bench).
-export([run_table/1, run_pages/1]).

run_table(N) ->
    io:format("Running table lookup ~p iterations~n", [N]),
    Start = erlang:monotonic_time(millisecond),
    run_table_loop(N),
    End = erlang:monotonic_time(millisecond),
    io:format("~p~n", [End-Start]).

run_pages(N) ->
    io:format("Running pages lookup ~p iterations~n", [N]),
    Start = erlang:monotonic_time(millisecond),
    run_pages_loop(N),
    End = erlang:monotonic_time(millisecond),
    io:format("~p~n", [End-Start]).

run_table_loop(0) -> ok;
run_table_loop(N) ->
    % Pick a small set of codepoints common in the mapping
    Cps = [16#00C0,16#00E9,16#00F6,16#00DF,16#0130,16#00D6,16#00C4,16#00E4],
    I = (N rem length(Cps)) + 1,
    Cp = lists:nth(I, Cps),
    _ = 'str@internal@generated_translit_table_bench':translit_table_bench_lookup(Cp),
    run_table_loop(N-1).

run_pages_loop(0) -> ok;
run_pages_loop(N) ->
    Cps = [16#00C0,16#00E9,16#00F6,16#00DF,16#0130,16#00D6,16#00C4,16#00E4],
    I = (N rem length(Cps)) + 1,
    Cp = lists:nth(I, Cps),
    _ = 'str@internal@generated_translit_pages_bench':translit_pages_bench_lookup_by_codepoint(Cp),
    run_pages_loop(N-1).
