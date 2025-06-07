# Iter: Polymorphic For-Loops in Erlang

`iter` is a lightweight Erlang library that provides a generic, polymorphic `for/2` and `for/3` function interface for iterating and folding over common data structures, including lists, maps, sets, arrays, queues, and dictionaries. It was also an attempt to re-order the map/fold parameter ordering into one which would improve readability when using nested calls. 

## Features

- Unified API for different iterable types
- Works with:
  - Lists
  - Maps
  - `sets:set()`
  - `array:array()`
  - `queue:queue()`
  - `dict:dict()`
- Supports both mapping and folding styles.

## API

```erlang
-spec for(Iterable :: iterable(T), Fun :: fun((T) -> U)) -> iterable(U).
```
Maps a function over the elements of a supported iterable structure, returning a transformed iterable.

```erlang
-spec for(Iterable :: iterable(T), Acc :: A, Fun :: fun((T, A) -> A)) -> A.
```
Folds a function over the elements of a supported iterable, starting from an initial accumulator.

## Supported iterable(T) Types

```erlang
-type iterable(T) ::
          [T]                  %% List
        | #{_Key := T}         %% Map
        | array:array(T)       %% Array
        | sets:set(T)          %% Set
        | queue:queue(T)       %% Queue
        | dict:dict(T).        %% Dictionary
        
```

## Examples

```erlang
% Sum a list
SumList = iter:for([1, 2, 3], 0, fun(Elem, Acc) -> Elem + Acc end).

% Map over a set
NewSet = iter:for(sets:from_list([1, 2, 3]), fun(X) -> X * 2 end).

% Fold a map
SumMap = iter:for(#{a => 1, b => 2}, 0, fun(_Key, Value, Acc) -> Value + Acc end).

% Map an array
NewArray = iter:for(array:from_list([1, 2, 3]), fun(X) -> X * 10 end).

% Nested loops: build sums of two lists
Outer = [1, 2], Inner = [10, 20],
Res = iter:for(Outer, [],
        fun(OE, OA) ->
            Sums = iter:for(Inner, [],
                fun(IE, IA) ->
                    [OE + IE | IA]
                end),
            [Sums | OA]
        end).
```

## Error Handling

If an unsupported iterable is passed to for/2 or for/3, the function will return:

```erlang
error({unsupported_iterable, Iterable})
```

## Testing

To compile and run the `main/0` examples:

```erlang
1> c(iter).
{ok,iter}
2> iter:main().
```

Or, to run the `iter_tests.erl`
```erlang
1> c(iter).
{ok,iter}
2> c(iter_tests).
{ok,tests}
3 > eunit:test(iter_tests).
  All 7 tests passed.
```