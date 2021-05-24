aho_corasick
=====

基于Aho Corasick 算法实现的Erlang版本敏感词匹配库，通过定义不同的ModName可以同时存在多个版本的敏感词库。

如何使用 How to use
-----

    $ rebar3 shell
    Erlang/OTP 23 [erts-11.1.4] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1]

    Eshell V11.1.4  (abort with ^G)
    1> ModName = acs.
    acs
    2> aho_corasick:gen_acs_by_list([<<"a">>, <<"ab">>, <<"bab">>, <<"bc">>, <<"bca">>, <<"c">>, <<"caa">>], ModName).
    ok
    3> Content = <<"abccab">>.
    <<"abccab">>
    4> aho_corasick:matches(ModName, Content).
    [{4,2},{4,1},{3,1},{2,1},{1,2},{0,2},{0,1}]

版本要求 OTP Version
-----
    >= OTP 17.0.0
