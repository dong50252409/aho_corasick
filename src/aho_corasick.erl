-module(aho_corasick).

-export([matches/1, output/2]).

matches(Subject) ->
    RootState = 0,
    Index = 1,
    matches_1(Subject, RootState, Index).

matches_1(<<Word/utf8, T/binary>>, State, Index) ->
    {NewState, LenList} = match_word(Word, State),
    Pattern = matches_1(T, NewState, Index + 1),
    [{Index - Len, Len} || Len <- LenList] ++ Pattern;
matches_1(<<>>, _State, _Index) ->
    [].

match_word(Word, State) ->
    case ac_tree:success(Word, State) of
        false ->
            case State of
                0 ->
                    {State, []};
                _ ->
                    {NewState, _} = ac_tree:failure(State),
                    match_word(Word, NewState)
            end;
        NextState ->
            {NextState, get_output(NextState)}
    end.

get_output(State) when State > 0 ->
    {NewState, Len} = ac_tree:failure(State),
    LenList = get_output(NewState),
    case Len of
        undefined ->
            LenList;
        Len ->
            [Len | LenList]
    end;
get_output(_State) ->
    [].

output(Content, MatchesList) ->
    io:format("~ts", [output_1(unicode:characters_to_list(Content), MatchesList)]).

output_1(Content, [{Pos, Len} | T]) ->
    [lists:sublist(Content, Pos + 1, Len), "," | output_1(Content, T)];
output_1(_Content, []) ->
    [].