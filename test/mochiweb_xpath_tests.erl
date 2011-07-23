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


%% [{HtmlFileName,Cases}]
%% Cases = {XPath,ExpectedResult}
test_definitions() ->
    [ 
        {?HTML1,[
                {"/html/head/title/text()",[<<"Title">>]},
                {"/html/head/meta[2]/@content",[<<"text/html; charset=utf-8">>]},
                {"/html/body/div[@id='first']/@class",[<<"normal">>]},
                {"/html/body/form/input[@name='id3']/@value",[<<"Val3">>]},
                {"/html/body/*/input[@name='id3']/@value",[<<"Val3">>]},
                {"/html/head/title/text() = 'Title'",true},
                {"/html/head[non_existent/no_existent]/title/text()",[]},
                {"/html/body/*/input[position() = 3]/@value",[<<"Val3">>]},
                {"/html/body/*/input[position() > 3]/@value",[<<"Val4">>,<<"Val5">>,<<"Val6">>, "", ""]},
                {"/html/body/*/input[@type='hidden']/@value",[<<"Val1">>,<<"Val2">>,<<"Val3">>,<<"Val4">>,<<"Val5">>,<<"Val6">>]},
                {"/html/body/*/input[@type='hidden'][last()]/@value",[<<"Val6">>]},
                {"/html/body/*/input[@type='hidden'][position()>1]/@value",[<<"Val2">>,<<"Val3">>,<<"Val4">>,<<"Val5">>,<<"Val6">>]},
                {"name(/html/body/*/input[@type='hidden'][@name=\"id1\"]/..)",<<"form">>},
                {"name(/html/body/*/input[@type='hidden'][@name=\"id1\"]/../..)",<<"body">>},
                {"name(/html/body/*/input[@type='hidden'][@name=\"id1\"]/../../..)",<<"html">>},
                {"/html/body/*/input[position() = 3]/@value",[<<"Val3">>]},
                {"count(/html/body/*/input[position() = 3]/preceding-sibling::*)",2},
                {"count(/html/body/*/input[position() = 3]/following-sibling::*)",6},
                {"/html/body/*/input[position() = 3]/following-sibling::*/@value",[<<"Val4">>,<<"Val5">>,<<"Val6">>, "", "", ""]},
                {"/html/body/*/input[position() = 3]/following-sibling::input/@value",[<<"Val4">>,<<"Val5">>,<<"Val6">>, "", ""]}
                ]},
        {?HTML2,[
                {"/html/body/div[1]/a[3]/text()",[<<"ssddd">>]},
                {"/html/body/form[1]/input[@type='hidden']/@value",
                        [<<"Val1">>,<<"Val2">>,<<"Val3">>]},
                {"/html/body/form[input[@name='id1_2']/@value='Val1_2']/@action",
                    [<<"Action2">>]},
                {"//input[@name='id1_2']/@value",[<<"Val1_2">>]},
                {"//form[.//input[@name='id1_2']]/@action",[<<"Action2">>]},
                {"//form[.//input/@name = 'id1']/@action",[<<"Action1">>]},
                {"//form[//input/@name ='id1']/@action",
                                            [<<"Action1">>,<<"Action2">>]},
                {"count(/html/body/form[count(input[@type='hidden']) = 4])",
                                         1},
                {"name(/html/*)",<<"head">>},
                {"/html/body/form[starts-with(@action,'Act')]/@action",
                        [<<"Action1">>,<<"Action2">>]},
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
                     {<<"a">>, [{<<"href">>,<<"sssd">>}], [<<"sfgfe">>]}]}
                ]}
    ].



%% @doc Functional test
all_test_() ->
    Mapping = [{<<"link">>,1},{<<"myUrl">>,2}],
    % F = simple user-defined function.
    %     First argument is the current XPath context,
    %     Second argument is a list containing the real parameters
    F = fun(_Ctx,[String]) ->
            proplists:get_value(String,Mapping,0)
        end,
    % Function "specification" {name,fun(),parameters signature}
    %           (the engine will take care of converting the parameters
    %            to the appropiate type (node_set|string|number|boolean)
    MyFuns = [{my_fun,F,[string]}],

    lists:map(fun(Def) -> do_test(Def,MyFuns) end, test_definitions()).


do_test({File,Cases},UserFunctions) ->
    {ok,DocBin} = file:read_file(filename:join(?HTMLDIR, File)),
    Doc = mochiweb_html:parse(DocBin),
    lists:map(fun({Expr,Expected}) ->
                      R = mochiweb_xpath:execute(Expr,Doc,UserFunctions),
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


