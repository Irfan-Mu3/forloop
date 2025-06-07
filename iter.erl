-module(iter).
-export([for/3, for/2]).

% testing
-export([main/0]).

-type iterable(T) ::
          [T]
       | sets:set(T)
       | array:array(T)
       | queue:queue(T).
       
-type iterable_map(Key, T) ::
        #{Key := T}
       | dict:dict(Key, T).

-spec for(Iterable :: iterable(T) | iterable_map(Key, T),
          Fun      :: fun((T) -> U) | fun((Key, T) -> U) | fun((ArrayIndex, T) -> U))
      -> iterable(U)
       when T :: term(),
            U :: term(),
            ArrayIndex :: integer(),
            Key :: term().
        
for(Iterable, Fun) when is_list(Iterable) ->
    lists:map(Fun, Iterable);

for(Iterable, Fun) when is_map(Iterable) ->
    maps:map(Fun, Iterable);

%% tuples: further dispatch on first element tag
for(Iterable, Fun) when is_tuple(Iterable) ->
    case element(1, Iterable) of
        set -> sets:map(Fun,Iterable);
        array -> array:map(Fun, Iterable);
        dict -> dict:map(Fun, Iterable);
        _ -> case queue:is_queue(Iterable) of
                true ->
                    queue:filtermap(fun(Elem) -> {true, Fun(Elem)} end, Iterable);
                false ->
                    error({unsupported_iterable, Iterable})
            end
    end;

%% fallback
for(Iterable, _) ->
    error({unsupported_iterable, Iterable}).

-spec for( Iterable :: iterable(T) | iterable_map(Key, T),
          Acc      :: A,
          Fun      :: fun((T, A) -> A)
                   | fun((Key, T, A) -> A)
                   | fun((ArrayIndex, T, A) -> A))
      -> A
      when ArrayIndex :: integer(),
           Key :: term(),
           T :: term(),
           A :: term().

for(Iterable, Acc, Fun) when is_list(Iterable) ->
    lists:foldl(Fun, Acc, Iterable);

for(Iterable, Acc, Fun) when is_map(Iterable) ->
    maps:fold(Fun, Acc, Iterable);

%% Convert other structures like gb_trees, gb_sets and basic tuples to lists first. 
for(Iterable, Acc, Fun) when is_tuple(Iterable) ->
    case element(1, Iterable) of
        set -> sets:fold(Fun, Acc, Iterable);
        array -> array:foldl(Fun, Acc, Iterable);
        dict -> dict:fold(Fun, Acc, Iterable);
        _ -> case queue:is_queue(Iterable) of 
            true -> queue:fold(Fun, Acc, Iterable);
            false -> error({unsupported_iterable, Iterable})
            end
    end;

for(Iterable, _, _) ->
    error({unsupported_iterable, Iterable}).


main() ->
    List = [1, 2, 3],
    iter:for(List, 0, fun(Elem, Acc) -> Elem + Acc end),

    Map = #{a => 1, b => 2},
    iter:for(Map, 0, fun(_Key, Value, Acc) -> Value + Acc end),

    Set = sets:from_list([1, 2, 3]),
    iter:for(Set, 0, fun(Elem, Acc) -> Elem + Acc end),

    Array = array:from_list([1,2,3]),
    iter:for(Array, 0, fun(_Index, Value, Acc) -> Value + Acc end),

    Queue = queue:from_list([1,2,3,4,5,6,7,6,5,4,32,1,34]),
    iter:for(Queue, 0, fun(Elem, Acc) -> Elem + Acc end),

    Tuple = {1, 2, 3},
    iter:for(tuple_to_list(Tuple), 0, fun(Elem, Acc) -> Elem + Acc end),

    Dict = dict:from_list([{c,1},{a,2}]),
    iter:for(dict:to_list(Dict), 0, fun({_Key, Value}, Acc) -> Value + Acc end),

    OrdDict = orddict:from_list([{a,1},{b,2}]),
    iter:for(orddict:to_list(OrdDict), 0, fun({_Key, Value}, Acc) -> Value + Acc end),

    GBTrees = gb_trees:from_orddict(OrdDict),
    iter:for(gb_trees:to_list(GBTrees), 0, fun({_Key, Value}, Acc) -> Value + Acc end),

    OuterList = [1, 2],
    InnerList = [10, 20],
    iter:for(OuterList, [], fun(OE, OA) ->
        Sums = iter:for(InnerList, [], fun(IE, IA) -> [OE + IE | IA] end),
        [Sums | OA]
    end).


