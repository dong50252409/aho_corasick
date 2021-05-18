aho_corasick
=====

An OTP library

Build
-----

    $ rebar3 shell
    gen_ac_tree:gen_by_filename(code:priv_dir(aho_corasick) ++ "/mask.txt").
    
    c(ac_tree).

    {ok, Content}  = file:read_file(code:priv_dir(aho_corasick) ++ "/text.txt")).

    aho_corasick:matches(Content).
