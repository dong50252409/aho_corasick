-module(aho_corasick).

%% API
-export([gen_acs_by_filename/2, gen_acs_by_list/2]).
-export([matches/2, is_pattern/2, replace/2, replace/3]).

gen_acs_by_filename(Filename, ModName) when is_atom(ModName) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            StrList = binary:split(unicode:characters_to_binary(Content), [<<"\r\n">>, <<"\n">>], [trim_all, global]),
            do_build(StrList, ModName);
        Err ->
            Err
    end.

gen_acs_by_list(StrList, ModName) when is_atom(ModName) ->
    do_build(StrList, ModName).

%% 构建AC算法所需的必要结构
do_build(StrList, ModName) ->
    RootState = 0,                                  % 从根节点开始遍历
    SuccessMap = #{},                               % 前缀树结构
    OutputMap = #{},                                % 匹配上的字符串输出结构
    FailureMap = #{},                               % 创建失配结构
    {NewSuccessMap, NewOutputMap} = do_build_success(StrList, RootState, SuccessMap, OutputMap),
    NewFailureMap = do_build_failure([RootState], NewSuccessMap, FailureMap),
    gen_ac_file(NewSuccessMap, NewFailureMap, NewOutputMap, ModName).

%% 构建前缀树结构
do_build_success([Str | T], MaxState, SuccessMap, OutputMap) ->
    RootState = 0,
    {CurState, NewMaxState, NewSuccessMap} = do_build_success_1(Str, RootState, MaxState, SuccessMap),
    NewOutputMap = OutputMap#{CurState => length(unicode:characters_to_list(Str))},
    do_build_success(T, NewMaxState, NewSuccessMap, NewOutputMap);
do_build_success([], _NewMaxState, SuccessMap, Output) ->
    {SuccessMap, Output}.

do_build_success_1(<<Word/utf8, T/binary>>, CurState, MaxState, SuccessMap) ->
    case SuccessMap of
        #{CurState := #{Word := NextState}} ->
            do_build_success_1(T, NextState, MaxState, SuccessMap);
        #{CurState := ChildSuccessMap} ->
            MaxState1 = MaxState + 1,
            SuccessMap1 = SuccessMap#{CurState := ChildSuccessMap#{Word => MaxState1}},
            do_build_success_1(T, MaxState1, MaxState1, SuccessMap1);
        #{} ->
            MaxState1 = MaxState + 1,
            SuccessMap1 = SuccessMap#{CurState => #{Word => MaxState1}},
            do_build_success_1(T, MaxState1, MaxState1, SuccessMap1)
    end;
do_build_success_1(<<>>, CurState, MaxState, SuccessMap) ->
    {CurState, MaxState, SuccessMap}.

%% 基于宽度优先构建失配结构
do_build_failure([ParentState | T], SuccessMap, FailureMap) ->
    case SuccessMap of
        #{ParentState := ChildSuccessMap} ->
            Iter = maps:iterator(ChildSuccessMap),
            {NextStateList, NewFailureMap} = do_build_failure_1(maps:next(Iter), ParentState, SuccessMap, FailureMap),
            do_build_failure(T ++ NextStateList, SuccessMap, NewFailureMap);
        #{} ->
            do_build_failure(T, SuccessMap, FailureMap)
    end;
do_build_failure([], _SuccessMap, FailureMap) ->
    FailureMap.

do_build_failure_1({Char, State, NextIter}, ParentState, SuccessMap, FailureMap) when ParentState > 0 ->
    FailureState = maps:get(ParentState, FailureMap, 0),
    ChildSuccessMap = maps:get(FailureState, SuccessMap),
    Iter = maps:iterator(ChildSuccessMap),
    NewFailureMap = do_find_failure_node(maps:next(Iter), Char, State, SuccessMap, FailureMap),
    {NextStateList, NewFailureMap1} = do_build_failure_1(maps:next(NextIter), ParentState, SuccessMap, NewFailureMap),
    {[State | NextStateList], NewFailureMap1};
do_build_failure_1({_Char, State, NextIter}, ParentState, SuccessMap, FailureMap) ->
    {NextStateList, NewFailureMap} = do_build_failure_1(maps:next(NextIter), ParentState, SuccessMap, FailureMap),
    {[State | NextStateList], NewFailureMap};
do_build_failure_1(none, _ParentState, _SuccessMap, FailureMap) ->
    {[], FailureMap}.

do_find_failure_node({Char, ParentState, _NextIter}, Char, State, SuccessMap, FailureMap) ->
    case maps:is_key(ParentState, SuccessMap) of
        true ->
            FailureMap#{State => ParentState};
        false ->
            FailureMap
    end;
do_find_failure_node({_ParentChar, _ParentState, NextIter}, Char, State, SuccessMap, FailureMap) ->
    do_find_failure_node(maps:next(NextIter), Char, State, SuccessMap, FailureMap);
do_find_failure_node(none, _Char, _State, _SuccessMap, FailureMap) ->
    FailureMap.

gen_ac_file(SuccessMap, FailureMap, OutputMap, ModName) ->
    {ok, Content} = file:read_file(code:priv_dir(aho_corasick) ++ "/acs.template"),
    Success = gen_success(SuccessMap),
    Failure = gen_failure(FailureMap, OutputMap),
    Str = unicode:characters_to_list(<<(iolist_to_binary(io_lib:format(Content, [ModName])))/binary, Success/binary, Failure/binary>>),
    Forms = scan_and_parse(Str, 1),
    {ok, ModName, Binary} = compile:forms(Forms, [deterministic, no_line_info]),
    {module, ModName} = code:load_binary(ModName, ModName, Binary),
    ok = file:write_file(code:lib_dir(aho_corasick) ++ "/ebin/" ++ atom_to_list(ModName) ++ ".beam", Binary).

gen_success(SuccessMap) ->
    Fun =
        fun(State, ChildSuccessMap, Acc) ->
            <<Acc/binary, "success(", (integer_to_binary(State))/binary, ") -> ", (iolist_to_binary(io_lib:format(<<"~w">>, [ChildSuccessMap])))/binary, ";\n">>
        end,
    Content = maps:fold(Fun, <<>>, SuccessMap),
    <<Content/binary, "success(_) -> false.\n\n">>.

gen_failure(FailureMap, OutputMap) ->
    Fun1 =
        fun(State, NextState, Acc) ->
            case OutputMap of
                #{State := Len} ->
                    <<Acc/binary, "failure(", (integer_to_binary(State))/binary, ") -> {", (integer_to_binary(NextState))/binary, ", ", (integer_to_binary(Len))/binary, "};\n">>;
                #{} ->
                    <<Acc/binary, "failure(", (integer_to_binary(State))/binary, ") -> {", (integer_to_binary(NextState))/binary, ", undefined};\n">>
            end
        end,
    Content1 = maps:fold(Fun1, <<>>, FailureMap),
    Fun2 =
        fun(State, Len, Acc) ->
            case maps:is_key(State, FailureMap) of
                true ->
                    Acc;
                false ->
                    <<Acc/binary, "failure(", (integer_to_binary(State))/binary, ") -> {0", ", ", (integer_to_binary(Len))/binary, "};\n">>
            end
        end,
    Content2 = maps:fold(Fun2, <<>>, OutputMap),
    <<Content1/binary, Content2/binary, "failure(_) -> {0, undefined}.\n\n">>.

scan_and_parse(Text, Line) ->
    case erl_scan:tokens([], Text, Line) of
        {done, {ok, Tokens, NLine}, T} ->
            {ok, Forms} = erl_parse:parse_form(Tokens),
            [Forms | scan_and_parse(T, NLine)];
        {more, _Continuation} ->
            []
    end.

-spec matches(AcsMod :: module(), Subject :: binary()) -> Found :: [binary:part()].
matches(AcsMod, Subject) ->
    AcsMod:matches(Subject).

-spec is_pattern(AcsMod :: module(), Subject :: binary()) -> boolean().
is_pattern(AcsMod, Subject) ->
    AcsMod:is_pattern(Subject).

-spec replace(AcsMod :: module(), Subject :: binary()) -> Return :: binary().
replace(AcsMod, Subject) ->
    AcsMod:replace(Subject, <<"*"/utf8>>).

-spec replace(AcsMod :: module(), Subject :: binary(), Replacement :: binary()) -> Return :: binary().
replace(AcsMod, Subject, Replacement) ->
    AcsMod:replace(Subject, Replacement).