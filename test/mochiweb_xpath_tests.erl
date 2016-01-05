%% coding: utf-8
%% 
%% @author Pablo Polvorin 
%% @author Hunter Kelly.
%% created on 2008-04-30
%% Converted to eunit on 2011-07-04
%% 
%% Some simple functional test cases, test the xpath implementation
-module(mochiweb_xpath_tests).

-include_lib("eunit/include/eunit.hrl").

-define(HTMLDIR, "../test/html-docs").
-define(HTML1, "test1.html").
-define(HTML2, "test2.html").

%% "@" is shortcut of 'attribute::'
%% "//" is like "descendant-or-self::" on root node
%% empty axis is "child::"
%% "../" is "parent::"
%% "./" is "self::"

%% [{HtmlFileName,Cases}]
%% Cases = {XPath,ExpectedResult}
test_definitions() ->
    [ 
      {?HTML1,[
               {"/html/head/title/text()",[<<"Title">>]},
               {"/html/head/meta[2]/@content",[<<"text/html; charset=utf-8">>]},
               {"/html/body/div[@id='first']/@class",[<<"normal">>]},
               % A bug in xmerl_xpath_scan means concat(' ', @class, ' ') fails to tokenise.
               {"/html/body/div[contains(concat(' ', @class, ' '), ' cd ')]/text()", [<<"oh no">>]},
               {"/html/body/form/input[@name='id3']/@value",[<<"Val3">>]},
               {"/html/body/*/input[@name='id3']/@value",[<<"Val3">>]},
               {"/html/head/title/text() = 'Title'",true},
               {"/html/head[non_existent/no_existent]/title/text()",[]},
               {"/html/body/*/input[position() = 3]/@value",[<<"Val3">>]},
               {"/html/body/*/input[position() > 3]/@value",[<<"Val4">>,<<"Val5">>,<<"Val6">>]},
               {"/html/body/*/input[@type='hidden']/@value",[<<"Val1">>,<<"Val2">>,<<"Val3">>,<<"Val4">>,<<"Val5">>,<<"Val6">>]},
               {"/html/body/*/input[@type='hidden'][position() = last()]/@value",[<<"Val6">>]},
               {"/html/body/*/input[@type='hidden'][position()>1]/@value",[<<"Val2">>,<<"Val3">>,<<"Val4">>,<<"Val5">>,<<"Val6">>]},

               % testing the abbreviated syntax to access indexed
               {"count(/html/body/form[position()]/*)", 8},
               {"/html/body/*/input[3 + 0]/@value",[<<"Val3">>]},

               {"string(//div[@class='normal']/cite)",[<<"one-two-three-(nested-[deeply-four-done]-done)-five">>, <<"other stuff">>]},
               {"name(/html/body/*/input[@type='hidden'][@name=\"id1\"]/..)",<<"form">>},
               {"name(/html/body/*/input[@type='hidden'][@name=\"id1\"]/../..)",<<"body">>},
               {"name(/html/body/*/input[@type='hidden'][@name=\"id1\"]/../../..)",<<"html">>},
               {"/html/body/*/input[position() = 3]/@value",[<<"Val3">>]},
               %% test union "|"
               {"/html/head/title/text() | /html/body/h1/text()", [<<"Some Title!!">>, <<"Title">>]}, % not necessary in document order according to spec
               %% test "contains()"
               {"/html/body/div/img[contains(@src, 'broken')]/@src",[<<"some_broken_img_tag">>]},
               %% test "concat()"
               {"concat(//div[@id='last']/@class, 'txt1', 2)",<<"normaltxt12">>},
               %% rest "not()"
               {"not(1=2)", true},
               {"not(1=1)", false},
               {"not(/html/head/title)", false},
               {"not(/html/blablabla)", true},
               {"not('123')", false},
               %% test negative
               {"-1", [-1]},
               {"-count(/html/*)", [-2]},
               %% == node tests ==
               %% wildcard
               {"count(/html/body/form/*)", 8},
               {"/html/body/form/@*", [<<"Action">>, <<"POST">>]},
               %% name
               {"/html/head/title", [{<<"title">>, [], [<<"Title">>]} ]},
               {"/html/head/meta/@name", [<<"GENERATOR">>]},
               %% -- node type --
               {"count(/html/body/form/node())", 9}, % node()
               {"/html/body/ul[1]/li/text()", [<<"List item">>, <<"list item2">>]}, % text()
               %% {"/html/body/form/attribute()", []},  % attribute() - not exists on standard!
               {"/html/body/comment()", [{comment,<<" some comment ">>}]}, % comment()
               %% {"/html/body/processing-instruction()", [{pi,<<"php my_processing_instr() ">>}]},  % processing-instriction() not work on my R14B04, but fixed in bffc6b40 Xmerl 1.3
               {"/html/body/processing-instruction('php')", [{pi,<<"php my_processing_instr() ">>}]},  % processing-instriction()
               {"/html/body/processing-instruction('erl')", []},  % processing-instriction()
               %% == axes ==
               %% -- descendant --
               {"count(/html/body/ul/descendant::*)", 4},
               {"/html/body/div[@id='desc_or_self']/descendant::*",
                fun({Name, _Arttr, _Chld}) -> Name end,
                [<<"i">>, <<"p">>, <<"span">>, <<"b">>]},
               %% --descendant-or-self --
               {"count(/html/body/ul/descendant-or-self::*)", 6},
               {"/html/body/div[@id='desc_or_self']/descendant-or-self::*",
                fun({Name, _Arttr, _Chld}) -> Name end,
                [<<"div">>, <<"i">>, <<"p">>, <<"span">>, <<"b">>]},
%% FIXME: Broken
%%               {"/html/body/div[@id='desc_or_self']/descendant-or-self::*/text()",
 %%               fun(Name) -> re:replace(Name, "(^\\s+)|(\\s+$)", "", [global,{return,list}]) end,
   %%             ["txt1", "txt2", "txt3", "txt4", "txt5", "txt6", "txt7"]},
%%
%% preorder_text(El) ->
%%     iolist_to_binary(preorder_text1(El)).
%% preorder_text1({_El, _At, Childs}) ->
%%     [preorder_text(Child) || Child <- Childs];
%% preorder_text1(Binary) ->
%%     Binary.
%%
%% /FIXME
               %% -- parent --
               {"count(/html/body/ul/li/parent::*)", 2},
               %% {"count(/html/body/ul/li/text()/parent::*)", 4},  %%TODO: parent for non-elements
               {"/html/body/form/input/parent::form/@method", [<<"POST">>]},
               %% -- following-sibling --
               {"count(/html/body/*/input[position() = 3]/following-sibling::*)",5},
               {"/html/body/*/input[position() = 3]/following-sibling::input/@value",
                [<<"Val4">>,<<"Val5">>,<<"Val6">>]},
               {"/html/body/*/input[position() = 3]/following-sibling::*/@value",
                [<<"Val4">>,<<"Val5">>,<<"Val6">>]},
               %% -- preceding-sibling --
               {"count(/html/body/*/input[position() = 3]/preceding-sibling::*)",2},
               {"/html/body/*/input[position() = 3]/preceding-sibling::input/@value",
                [<<"Val1">>, <<"Val2">>]},
               %% -- attribute --
               {"/html/body/form/input[1]/attribute::*", [<<"hidden">>, <<"id1">>, <<"Val1">>]},
               {"/html/body/form/input[1]/attribute::node()", [<<"hidden">>, <<"id1">>, <<"Val1">>]},
               {"/html/body/form/input[1]/attribute::value", [<<"Val1">>]}
              ]},
      {?HTML2,[
               {"/html/body/div[1]/a[3]/text()",[<<"ssddd">>]},
               {"/html/body/form[1]/input[@type='hidden']/@value",
                [<<"Val1">>,<<"Val2">>,<<"Val3">>]},
               {"/html/body/form[input[@name='id1_2']/@value='Val1_2']/@action",
                [<<"Action2">>]},
               {"//input[@name='id1_2']/@value",[<<"Val1_2">>]},
               {"//input[@name='id2' and @type='hidden']/@value", [<<"Val2">>, <<"Val2_2">>]},
               {"//input[@name='id2' or @name='id3']/@value",
                [<<"Val2">>, <<"Val3">>, <<"Val2_2">>, <<"Val3_2">>]},
               {"//form[.//input[@name='id1_2']]/@action",[<<"Action2">>]},
               {"//form[.//input/@name = 'id1']/@action",[<<"Action1">>]},
               {"//form[//input/@name ='id1']/@action",
                [<<"Action1">>,<<"Action2">>]},
               {"count(/html/body/form[count(input[@type='hidden']) = 4])",
                1},
               {"name(/html/*)",<<"head">>},
               {"/html/body/form[starts-with(@action,'Act')]/@action",
                [<<"Action1">>,<<"Action2">>]},
               {"/html/body/div[ends-with(@id,'t')]/@id",
                [<<"first">>,<<"last">>]},
               {"//input[substring(@name,1,4) = 'id1_']/@value",
                [<<"Val1_2">>]},
               {"//div[sum(number)=23]/@id",[<<"sum">>]},
               {"//div[sum(number)>20]/@id",[<<"sum">>]},
               {"string-length(name(/html)) = 4",true},
               {"//a[my_fun(@href) > 0]/text()", 
                [<<"ssddd">>,<<"myURLValue">>]},
               {"/html/body/div[1]/a[1]",
                [{<<"a">>,[{<<"href">>,<<"sss">>}],[<<"ssddd">>]}]},
               {"/html/body/div[1]/a[position() < 3]", 
                [{<<"a">>,[{<<"href">>,<<"sss">>}],[<<"ssddd">>]}, 
                 {<<"a">>, [{<<"href">>,<<"sssd">>}], [<<"sfgfe">>]}]},
               %% XPath expressions in arithmetic operations
               {"sum(//div[@id='second']/div/number)", 23},
               {"(//div[@id='second']/div/number[1]) + (//div[@id='second']/div/number[3])", 13},
               %% XPath expressions in boolean operations
               {"(/html/head/title) and (/html/body/h1)", true},
               {"/html[head/title and body/h1]/body/h1/text()", [<<"Some Title!!">>]},
               %% XPath expressions in string operations
               {"/html/body/div[@id='first']/@class = /html/body/div[@id='last']/@class", true}
              ]}
    ].



%% @doc Functional test
all_test_() ->
    Mapping = [{<<"link">>,1},{<<"myUrl">>,2}],
    %% F = simple user-defined function.
    %%     First argument is the current XPath context,
    %%     Second argument is a list containing the real parameters
    F = fun(_Ctx,[String]) ->
                proplists:get_value(String,Mapping,0)
        end,
    %% Function "specification" {name,fun(),parameters signature}
    %%           (the engine will take care of converting the parameters
    %%            to the appropiate type (node_set|string|number|boolean)
    MyFuns = [{my_fun,F,[string]}],

    lists:map(fun(Def) -> do_test(Def,MyFuns) end, test_definitions()).


unicode_test() ->
    {ok,DocBin} = file:read_file(filename:join(?HTMLDIR, ?HTML1)),
    Doc = mochiweb_html:parse(DocBin),
    ?assertEqual([<<"unicode-class">>],
                 mochiweb_xpath:execute("/html/body/div[@class='юникод']/text()", Doc)),
    mochiweb_xpath:compile_xpath("/html/body/processing-instruction('юникод')").
    %% Unfortunately, unicode in processing instructions doesn't work in mochiweb_html, so
    %% following test doesn't work.
    %% ?assertEqual([<<"unicode-pi">>],
    %%              mochiweb_xpath:execute("/html/body/processing-instruction('юникод')", Doc)).

do_test({File,Cases},UserFunctions) ->
    {ok,DocBin} = file:read_file(filename:join(?HTMLDIR, File)),
    Doc = mochiweb_html:parse(DocBin),
    lists:map(fun({Expr,Expected}) ->
                      R = mochiweb_xpath:execute(Expr,Doc,UserFunctions),
                      { File ++ " " ++ Expr, ?_assertEqual(Expected, R) };
                 ({Expr, MapResFun, Expected}) ->
                      R = [MapResFun(E) || E <- mochiweb_xpath:execute(Expr,Doc,UserFunctions)],
                      { File ++ " " ++ Expr, ?_assertEqual(Expected, R) }
              end, Cases).

%% assert(Expr,Result,Expected) ->
%%    case Result == Expected of
%%         true -> 
%%             io:format("pass: ~s~n",[Expr]);
%%         false -> 
%%             io:format("*fail*: ~s : ~n* Result:~p Expected:~p~n",
%%                         [Expr,Result,Expected])
%%     end.
