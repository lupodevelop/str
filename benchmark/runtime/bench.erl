-module(bench).
-export([run/1]).

run(N) ->
    io:format("Running ~p iterations~n", [N]),
    Start = erlang:monotonic_time(millisecond),
    run_loop(N),
    End = erlang:monotonic_time(millisecond),
    io:format("~p~n", [End-Start]).

run_loop(0) -> ok;
run_loop(N) ->
    _ = str:slugify("Crème Brûlée 123 Öö æ"),
    run_loop(N-1).
