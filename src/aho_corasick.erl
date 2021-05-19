-module(aho_corasick).

-export([matches/1, is_pattern/1]).

-spec matches(Subject :: binary()) -> Found :: [binary:part()].
matches(Subject) ->
    RootState = 0,
    Index = 1,
    matches_1(Subject, RootState, Index).

matches_1(<<Word/utf8, T/binary>> = Subject, State, Index) ->
    case ac_tree:success(State) of
        #{Word := NextState} ->
            Pattern = matches_1(T, NextState, Index + 1),
            get_output(NextState, Index) ++ Pattern;
        _ when State =:= 0 ->
            matches_1(T, State, Index + 1);
        _ ->
            {NewState, _} = ac_tree:failure(State),
            matches_1(Subject, NewState, Index)
    end;
matches_1(<<>>, _State, _Index) ->
    [].

get_output(State, Index) when State > 0 ->
    case ac_tree:failure(State) of
        {NewState, undefined} ->
            get_output(NewState, Index);
        {NewState, Len} ->
            [{Index - Len, Len} | get_output(NewState, Index)]
    end;
get_output(_State, _Index) ->
    [].

-spec is_pattern(Subject :: binary()) -> boolean().
is_pattern(Subject) ->
    RootState = 0,
    is_pattern_1(Subject, RootState).

is_pattern_1(<<Word/utf8, T/binary>>, State) ->
    case ac_tree:success(State) of
        #{Word := NextState} ->
            case is_pattern_2(NextState) of
                true ->
                    true;
                false ->
                    is_pattern_1(T, NextState)
            end;
        _ when State =:= 0 ->
            is_pattern_1(T, State);
        _ ->
            {NewState, _} = ac_tree:failure(State),
            is_pattern_1(<<Word/utf8, T/binary>>, NewState)
    end;
is_pattern_1(<<>>, _State) ->
    false.

is_pattern_2(State) when State > 0 ->
    case ac_tree:failure(State) of
        {NewState, undefined} ->
            is_pattern_2(NewState);
        _ ->
            true
    end;
is_pattern_2(_State) ->
    false.
