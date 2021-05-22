aho_corasick
=====

基于Aho Corasick 算法实现的Erlang版本敏感词匹配库，可以同时存在多个版本的敏感词库。

如何使用
-----

    $ rebar3 shell

    ModName = acs.                                          % 定义敏感词库模块名

    aho_corasick:gen_acs_by_filename("mask.txt", ModName).  % 通过文件加载敏感词

    {ok, Content}  = file:read_file("text.txt").

    aho_corasick:matches(Content, ModName).                 % 敏感词匹配

版本要求
-----
    >= OTP 17.0.0
