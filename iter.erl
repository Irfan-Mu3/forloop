-module(iter).
-export([for/3]).

% testing
-export([main/0]).


for(Iterable, Acc, Fun) when is_list(Iterable) ->
    lists:foldl(Fun, Acc, Iterable);

for(Iterable, Acc, Fun) when is_map(Iterable) ->
    maps:fold(Fun, Acc, Iterable);

% catches Set type too
for(Iterable, Acc, Fun) when is_tuple(Iterable) ->
    IsSet = sets:is_set(Iterable),
    case IsSet of
        true -> sets:fold(Fun, Acc, Iterable);
        false -> lists:foldl(Fun, Acc, tuple_to_list(Iterable))
    end;

for(_Iterable, _, _) ->
    error({unsupported_iterable, _Iterable}).


main() ->
    List = [1, 2, 3],
    iter:for(List, 0, fun(Elem, Acc) -> Elem + Acc end),

    Map = #{a => 1, b => 2},
    iter:for(Map, 0, fun(_Key, Value, Acc) -> Value + Acc end),

    Tuple = {1, 2, 3},
    iter:for(Tuple, 0, fun(Elem, Acc) -> Elem + Acc end),

    Set = sets:from_list([1, 2, 3]),
    iter:for(Set, 0, fun(Elem, Acc) -> Elem + Acc end),

    OuterList = [1, 2],
    InnerList = [10, 20],
    iter:for(OuterList, [], fun(OuterElem, OuterAcc) ->
        InnerResult = iter:for(InnerList, [], fun(InnerElem, InnerAcc) ->
            [OuterElem + InnerElem | InnerAcc]
        end),
        [InnerResult | OuterAcc]
    end).



