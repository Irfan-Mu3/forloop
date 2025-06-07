%%---------------------------------------------------------------------
%% @doc
%%   EUnit tests for iter:for/2
%%---------------------------------------------------------------------
-module(iter_tests).
-include_lib("eunit/include/eunit.hrl").

%% iter:for/2 should work for lists
list_map_test() ->
    List = [1,2,3],
    Fun = fun(X) -> X * 2 end,
    Expected = [2,4,6],
    ?assertEqual(Expected, iter:for(List, Fun)).

%% iter:for/2 should work for maps
map_map_test() ->
    Map = #{a => 1, b => 2},
    Fun = fun(_K, V) -> V + 10 end,
    Expected = #{a => 11, b => 12},
    ?assertEqual(Expected, iter:for(Map, Fun)).

%% iter:for/2 should work for sets
set_map_test() ->
    Set = sets:from_list([1,2,3]),
    Fun = fun(X) -> X - 1 end,
    Result = iter:for(Set, Fun),
    %% convert result set to sorted list for comparison
    Sorted = lists:sort(sets:to_list(Result)),
    ExpectedList = [0,1,2],
    ?assertEqual(ExpectedList, Sorted).

%% iter:for/2 should work for arrays
array_map_test() ->
    Arr = array:from_list([10,20,30]),
    Fun = fun(_I, X) -> X div 10 end,
    MappedArr = iter:for(Arr, Fun),
    ?assertEqual([1,2,3], array:to_list(MappedArr)).

%% iter:for/2 should work for dicts
dict_map_test() ->
    D = dict:from_list([{x,5},{y,6}]),
    Fun = fun(_K, V) -> V * V end,
    M = iter:for(D, Fun),
    %% extract values sorted by key
    List = lists:keysort(1, dict:to_list(M)),
    Values = [V || {_K,V} <- List],
    ?assertEqual([25,36], Values).

%% iter:for/2 should work for queues
queue_map_test() ->
    Q = queue:from_list([2,4,6]),
    Fun = fun(X) -> X + 1 end,
    MappedQ = iter:for(Q, Fun),
    List = queue:to_list(MappedQ),
    ?assertEqual([3,5,7], List).

%% iter:for/2 should error on unsupported types
error_map_test() ->
    %% use a tuple that isn't a supported container
    F = fun(_)-> ok end,
    ?assertException(error, {unsupported_iterable, {foo, bar}}, iter:for({foo,bar}, F) ).
