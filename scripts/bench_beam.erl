%% Lightweight BEAM-native benchmark harness for `str` functions.
%%
%% Usage (from repo root):
%%   erl -noshell \
%%     -pa build/dev/erlang/gleam_stdlib/ebin \
%%     -pa build/dev/erlang/str/ebin \
%%     -eval "bench_beam:run(), halt()."
%%
%% The script writes CSV output to `scripts/bench_results/bench_beam_<ts>.csv`.

-module(bench_beam).
-export([run/0]).

%% Simple helpers
ensure_dir(Path) ->
  case filelib:is_dir(Path) of
    true -> ok;
    false -> file:make_dir(Path)
  end.

timestamp() ->
  {{Y,Mo,D},{H,Mi,S}} = calendar:universal_time(),
  lists:flatten(io_lib:format("~4..0B~2..0B~2..0B_~2..0B~2..0B~2..0B", [Y,Mo,D,H,Mi,S])).

do_warmup(_M,_F,_A,0) -> ok;
do_warmup(M,F,A,N) when N > 0 ->
  _ = apply(M,F,A),
  do_warmup(M,F,A,N-1).

time_fun(M,F,A,Iter) ->
  %% Warm-up
  do_warmup(M,F,A,5),
  {MicroSecs, _} = timer:tc(fun() -> lists:foreach(fun(_) -> _ = apply(M,F,A) end, lists:seq(1,Iter)) end),
  MicroSecs div Iter.

gen_repetitive(Bin, N) when is_binary(Bin) ->
  iolist_to_binary(lists:duplicate(N, Bin)).

gen_random(Alphabet, N) when is_list(Alphabet) ->
  %% Alphabet is a list of integers (string). Build list of N random elements and convert to binary.
  Len = length(Alphabet),
  Fun = fun(_) -> lists:nth(rand:uniform(Len), Alphabet) end,
  Chars = [Fun(Arg) || Arg <- lists:seq(1,N)],
  list_to_binary(Chars).

write_csv_header(File) ->
  io:format(File, "case,scenario_type,text_len,pat_len,matches,index_of_us,index_of_auto_us,kmp_us,sliding_us,count_us,count_auto_us,iter~n", []).

measure_case(File, Name, Type, Text, Pat, Iter) ->
  %% Compute matches using sliding_search_all for consistency
  MatchesList = catch 'str@core':sliding_search_all(Text, Pat),
  Matches = case MatchesList of
    {'EXIT', _} -> -1;
    L -> length(L)
  end,
  Iof = time_fun('str@core', index_of, [Text, Pat], Iter),
  Iaof = time_fun('str@core', index_of_auto, [Text, Pat], Iter),
  Kmp = time_fun('str@core', kmp_search_all, [Text, Pat], Iter),
  Slide = time_fun('str@core', sliding_search_all, [Text, Pat], Iter),
  Cnt = time_fun('str@core', count, [Text, Pat, true], Iter),
  Ca = time_fun('str@core', count_auto, [Text, Pat, true], Iter),
  io:format(File, "~s,~s,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p~n",
    [Name, Type, byte_size(Text), byte_size(Pat), Matches, Iof, Iaof, Kmp, Slide, Cnt, Ca, Iter]).

run() ->
  rand:seed(exsplus, {erlang:monotonic_time(), erlang:unique_integer([positive]), erlang:phash2(node())}),
  ensure_dir("scripts/bench_results"),
  Ts = timestamp(),
  Path = filename:join("scripts/bench_results", "bench_beam_" ++ Ts ++ ".csv"),
  {ok, File} = file:open(Path, [write, {encoding, utf8}]),
  write_csv_header(File),
  io:format("Starting BEAM benchmarks...~n"),
  Iter = 200,

  %% Scenarios
  %% 1) repetitive no match
  Text1 = gen_repetitive(<<$a>>, 20000),
  Bin1 = gen_repetitive(<<$a>>, 1000),
  Pat1 = <<Bin1/binary, $b>>,
  io:format("Running repetitive_nomatch (~p bytes text, ~p bytes pat)...~n", [byte_size(Text1), byte_size(Pat1)]),
  measure_case(File, "repetitive_nomatch", "repetitive_nomatch", Text1, Pat1, Iter),

  %% 2) repetitive many matches
  Text2 = gen_repetitive(<<$a>>, 20000),
  Pat2 = gen_repetitive(<<$a>>, 50),
  io:format("Running repetitive_many (~p bytes text, ~p bytes pat)...~n", [byte_size(Text2), byte_size(Pat2)]),
  measure_case(File, "repetitive_many", "repetitive_many", Text2, Pat2, Iter),

  %% 3) random small pat
  Text3 = gen_random("abcd", 20000),
  Pat3 = gen_random("abcd", 20),
  io:format("Running random_small_pat (~p bytes text, ~p bytes pat)...~n", [byte_size(Text3), byte_size(Pat3)]),
  measure_case(File, "random_small_pat", "random", Text3, Pat3, Iter),

  %% 4) large text small pat
  Text4 = gen_random("abcd", 200000),
  Pat4 = <<"abcdab">>,
  io:format("Running large_text_small_pat (~p bytes text, ~p bytes pat)...~n", [byte_size(Text4), byte_size(Pat4)]),
  measure_case(File, "large_text_small_pat", "random", Text4, Pat4, Iter div 4),

  %% (emoji case omitted in this BEAM harness to avoid encoding edge-cases)

  file:close(File),
  io:format("Wrote results to ~s~n", [Path]),
  ok.
