%% mochiweb_html_xpath.erl
%% @author Pablo Polvorin
%% created on <2008-04-29>
%%
%% XPath interpreter, navigate mochiweb's html structs
%% Only a subset of xpath is implemented, see what is supported in test.erl
-module(mochiweb_xpath).

-export([execute/2,execute/3,compile_xpath/1]).

%internal data
-record(ctx, {
        root,
        ctx,
        functions,
        position,
        size
    }).

%%
%% API
%%

%% @spec( string() ) -> compiled_xpath()
compile_xpath(Expr) ->
    mochiweb_xpath_parser:compile_xpath(Expr).
    
%% @doc Execute the given XPath expression against the given document, using
%% the default set of functions. 
%% @spec execute(XPath,Doc) -> Results
%% @type XPath =  compiled_xpath() | string()
%% @type Doc = node()
%% @type Results = [node()] | binary() | boolean() | number()
execute(XPath,Root) ->
    execute(XPath,Root,[]).

%% @doc Execute the given XPath expression against the given document, 
%%      using the default set of functions plus the user-supplied ones. 
%%
%% @see mochiweb_xpath_functions.erl to see how to write functions
%%
%% @spec execute(XPath,Doc,Functions) -> Results
%% @type XPath =  compiled_xpath() | string()
%% @type Doc = node()
%% @type Functions = [FunctionDefinition]
%% @type FunctionDefinition = {FunName,Fun,Signature}
%% @type FunName = atom()
%% @type Fun = fun/2
%% @type Signature = [ArgType]
%% @type ArgType = node_set | string | number | boolean
%% @type Results = [node()] | binary() | boolean() | number()
%% TODO: should pass the user-defined functions when compiling
%%       the xpath expression (compile_xpath/1). Then the 
%%       compiled expression would have all its functions 
%%       resolved, and no function lookup would occur when
%%       the expression is executed
execute(XPathString,Doc,Functions) when is_list(XPathString) ->
    XPath = mochiweb_xpath_parser:compile_xpath(XPathString),
    execute(XPath,Doc,Functions);

execute(XPath,Doc,Functions) ->    
    R0 = {root,none,[Doc]},
    %% TODO: set parent instead of positions list, or some lazy-positioning?
    R1 = add_positions(R0),
    Result = execute_expr(XPath,#ctx{ctx=[R1],
                                     root=R1,
                                     functions=Functions,
                                     position=0}),
    remove_positions(Result).

%%
%% XPath tree traversing, top-level XPath interpreter
%%

%% xmerl_xpath:match_expr/2
execute_expr({path, Type, Arg}, S) ->
    eval_path(Type, Arg, S);
execute_expr(PrimExpr, S) ->
    eval_primary_expr(PrimExpr, S).


eval_path(union, {PathExpr1, PathExpr2}, C) ->
    %% in XPath 1.0 union doesn't necessary must return nodes in document
    %% order (but must in XPath 2.0)
    S1 = execute_expr(PathExpr1, C),
    S2 = execute_expr(PathExpr2, C),
    ordsets:to_list(ordsets:union(ordsets:from_list(S1),
                                  ordsets:from_list(S2)));
eval_path(abs, Path ,Ctx = #ctx{root=Root}) ->
    do_path_expr(Path, Ctx#ctx{ctx=[Root]});
eval_path(rel, Path, Ctx) ->
    do_path_expr(Path, Ctx);
eval_path(filter, {_PathExpr, {pred, _Pred}}, _C) ->
    error({not_implemented, "filter"}).      % Who needs them?


eval_primary_expr({comp,Comp,A,B},Ctx) ->
    %% for predicates
    CompFun = comp_fun(Comp),
    L = execute_expr(A,Ctx),
    R = execute_expr(B,Ctx),
    comp(CompFun,L,R);
eval_primary_expr({arith, Op, Arg1, Arg2}, Ctx) ->
    %% for predicates
    L = execute_expr(Arg1,Ctx),
    R = execute_expr(Arg2,Ctx),
    arith(Op, L, R);
eval_primary_expr({bool,Comp,A,B},Ctx) ->
    CompFun = bool_fun(Comp),
    L = execute_expr(A,Ctx),
    R = execute_expr(B,Ctx),
    comp(CompFun,L,R);
eval_primary_expr({literal,L},_Ctx) ->
    [L];
eval_primary_expr({number,N},_Ctx) ->
    [N];
eval_primary_expr({function_call, Fun, Args}, Ctx=#ctx{functions=Funs}) ->
    %% TODO: refactor double-case
    case mochiweb_xpath_functions:lookup_function(Fun) of
        {Fun, F, FormalSignature} ->
            call_xpath_function(F, Args, FormalSignature, Ctx);
        false ->
            case lists:keysearch(Fun,1,Funs) of
                {value, {Fun, F, FormalSignature}} ->
                    call_xpath_function(F, Args, FormalSignature, Ctx);
                false ->
                    throw({efun_not_found, Fun})
            end
    end.


call_xpath_function(F, Args, FormalSignature, Ctx) ->
    TypedArgs = prepare_xpath_function_args(Args, FormalSignature, Ctx),
    F(Ctx, TypedArgs).

%% execute function args expressions and convert them using formal
%% signatures
prepare_xpath_function_args(Args, Specs, Ctx) ->
    RealArgs = [execute_expr(Arg, Ctx) || Arg <- Args],
    convert_xpath_function_args(RealArgs, Specs, []).

convert_xpath_function_args([], [], Acc) ->
    lists:reverse(Acc);
convert_xpath_function_args(Args, [{'*', Spec}], Acc) ->
    NewArgs = [mochiweb_xpath_utils:convert(Arg,Spec) || Arg <- Args],
    lists:reverse(Acc) ++ NewArgs;
convert_xpath_function_args([Arg | Args], [Spec | Specs], Acc) ->
    NewAcc = [mochiweb_xpath_utils:convert(Arg,Spec) | Acc],
    convert_xpath_function_args(Args, Specs, NewAcc).



do_path_expr({step,{Axis,NodeTest,Predicates}}=_S,Ctx=#ctx{}) ->
    NewNodeList = axis(Axis, NodeTest, Ctx),
    apply_predicates(Predicates,NewNodeList,Ctx);
do_path_expr({refine,Step1,Step2},Ctx) ->
    S1 = do_path_expr(Step1,Ctx),
    do_path_expr(Step2,Ctx#ctx{ctx=S1}).


%%
%% Axes
%%
%% TODO: port all axes to use test_node/3

axis('self', NodeTest, #ctx{ctx=Context}) ->
    [N || N <- Context, test_node(NodeTest, N, Context)];
axis('descendant', NodeTest, #ctx{ctx=Context}) ->
    [N || {_,_,Children,_} <- Context,
          N <- descendant_or_self(Children, NodeTest, [], Context)];
axis('descendant_or_self', NodeTest, #ctx{ctx=Context}) ->
    descendant_or_self(Context, NodeTest, [], Context);
axis('child', NodeTest, #ctx{ctx=Context}) ->
    %% Flat list of all child nodes of Context that pass NodeTest
    [N || {_,_,Children,_} <- Context,
          N <- Children,
          test_node(NodeTest, N, Context)];
axis('parent', NodeTest, #ctx{root=Root, ctx=Context}) ->
    L = lists:foldl(
          fun({_,_,_,Position}, Acc) ->
                  ParentPosition = get_parent_position(Position),
                  ParentNode = get_node_at(Root, ParentPosition),
                  maybe_add_node(ParentNode, NodeTest, Acc, Context);
             (Smth, _Acc) ->
                  error({not_implemented, "parent for non-nodes", Smth})
        end, [], Context),
    ordsets:to_list(ordsets:from_list(lists:reverse(L)));
axis('ancestor', _Test, _Ctx) ->
    error({not_implemented, "ancestor axis"});

axis('following_sibling', {wildcard,wildcard}, #ctx{root=Root, ctx=Context}) ->
    lists:foldl(fun({_,_,_,Position}, Acc) ->
                ParentPosition = get_parent_position(Position),
                MyPosition = get_position_in_parent(Position),
                {_,_,Children,_} = get_node_at(Root, ParentPosition),
                Acc ++ lists:sublist(Children, MyPosition+1, length(Children) - MyPosition)
        end, [], Context);
axis('following_sibling', {name,{Tag,_,_}}, Ctx=#ctx{ctx=_Context}) ->
    F = fun ({Tag2,_,_,_}) when Tag2 == Tag -> true;
             (_) -> false
        end,
    Following0 = axis('following_sibling', {wildcard,wildcard}, Ctx),
    Following1 = lists:filter(F, Following0),
    Following1;

axis('preceding_sibling', {wildcard,wildcard}, #ctx{root=Root, ctx=Context}) ->
    lists:foldl(fun({_,_,_,Position}, Acc) ->
                ParentPosition = get_parent_position(Position),
                MyPosition = get_position_in_parent(Position),
                {_,_,Children,_} = get_node_at(Root, ParentPosition),
                Acc ++ lists:sublist(Children, MyPosition-1)
        end, [], Context);
axis('preceding_sibling', {name,{Tag,_,_}}, Ctx=#ctx{ctx=_Context}) ->
    F = fun ({Tag2,_,_,_}) when Tag2 == Tag -> true;
             (_) -> false
        end,
    Preceding0 = axis('preceding_sibling', {wildcard,wildcard}, Ctx),
    Preceding1 = lists:filter(F, Preceding0),
    Preceding1;

axis('following', _Test, _Ctx) ->
    error({not_implemented, "following axis"});
axis('preceeding', _Test, _Ctx) ->
    error({not_implemented, "preceeding axis"});

axis('attribute', NodeTest, #ctx{ctx=Context}) ->
    %% Flat list of *attribute values* of Context, that pass NodeTest
    %% TODO: maybe return attribute {Name, Value} will be better then
    %% value only?
    [Value || {_,Attributes,_,_} <- Context,
          {_Name, Value} = A <- Attributes,
          test_node(NodeTest, A, Context)];
axis('namespace', _Test, _Ctx) ->
    error({not_implemented, "namespace axis"});
axis('ancestor_or_self', _Test, _Ctx) ->
    error({not_implemented, "ancestor-or-self axis"}).


descendant_or_self(Nodes, NodeTest, Acc, Ctx) ->
    lists:reverse(do_descendant_or_self(Nodes, NodeTest, Acc, Ctx)).

do_descendant_or_self([], _, Acc, _) ->
    Acc;
do_descendant_or_self([Node = {_, _, Children, _} | Rest], NodeTest, Acc, Ctx) ->
    %% depth-first (document order)
    NewAcc1 = maybe_add_node(Node, NodeTest, Acc, Ctx),
    NewAcc2 = do_descendant_or_self(Children, NodeTest, NewAcc1, Ctx),
    do_descendant_or_self(Rest, NodeTest, NewAcc2, Ctx);
do_descendant_or_self([_Smth | Rest], NodeTest, Acc, Ctx) ->
    %% NewAcc = maybe_add_node(Smth, NodeTest, Acc, Ctx), - no attribs or texts
    do_descendant_or_self(Rest, NodeTest, Acc, Ctx).


%% Except text nodes
test_node({wildcard, wildcard}, Element, _Ctx) when not is_binary(Element) ->
    true;
test_node({prefix_test, Prefix}, {Tag, _, _, _}, _Ctx) ->
    test_ns_prefix(Tag, Prefix);
test_node({prefix_test, Prefix}, {AttrName, _}, _Ctx) ->
    test_ns_prefix(AttrName, Prefix);
test_node({name, {Tag, _, _}}, {Tag, _, _, _}, _Ctx) ->
    true;
test_node({name, {AttrName, _, _}}, {AttrName, _}, _Ctx) ->
    true;
test_node({node_type, text}, Text, _Ctx) when is_binary(Text) ->
    true;
test_node({node_type, node}, {_, _, _, _}, _Ctx) ->
    true;
test_node({node_type, node}, Text, _Ctx) when is_binary(Text) ->
    true;
test_node({node_type, node}, {_, _}, _Ctx) ->
    true;
%% test_node({node_type, attribute}, {_, _}, _Ctx) ->
%%     true; [38] - attribute() not exists!
test_node({node_type, comment}, {comment, _}, _Ctx) ->
    true;
test_node({node_type, processing_instruction}, {pi, _}, _Ctx) ->
    true;
test_node({processing_instruction, Name}, {pi, Node}, _Ctx) ->
    NSize = size(Name),
    case Node of
        <<Name:NSize/binary, " ", _/binary>> ->
            true;
        _ ->
            false
    end;
test_node(_Other, _N, _Ctx) ->
    false.

test_ns_prefix(Name, Prefix) ->
    PSize = size(Prefix),
    case Name of
        <<Prefix:PSize/binary, ":", _/binary>> ->
            true;
        _ ->
            false
    end.

%% Append Node to Acc only when NodeTest passed
maybe_add_node(Node, NodeTest, Acc, Ctx) ->
    case test_node(NodeTest, Node, Ctx) of
        true ->
            [Node | Acc];
        false ->
            Acc
    end.

%% used for predicate indexing
%% is_reverse_axis(ancestor) ->
%%     true;
%% is_reverse_axis(ancestor_or_self) ->
%%     true;
%% is_reverse_axis(preceding) ->
%%     true;
%% is_reverse_axis(preceding_sibling) ->
%%     true;
%% is_reverse_axis(_) ->
%%     flase.


%%
%% Predicates
%%
apply_predicates(Predicates,NodeList,Ctx) ->
    lists:foldl(fun({pred, Pred} ,Nodes) ->
                 apply_predicate(Pred,Nodes,Ctx) 
                end, NodeList,Predicates).

% special case: indexing
apply_predicate({number,N}, NodeList, _Ctx) when length(NodeList) >= N ->
    [lists:nth(N,NodeList)];

apply_predicate(Pred, NodeList,Ctx) ->
    Size = length(NodeList),
    Filter = fun(Node, {AccPosition, AccNodes0}) ->
            Predicate = mochiweb_xpath_utils:boolean_value(
                execute_expr(Pred,Ctx#ctx{ctx=[Node], position=AccPosition, size = Size})),
            AccNodes1 = if Predicate -> [Node|AccNodes0];
                true -> AccNodes0
            end,
            {AccPosition+1, AccNodes1}
    end,
    {_, L} = lists:foldl(Filter,{1,[]},NodeList),
    lists:reverse(L).


%%
%% Compare functions
%%

%% @see http://www.w3.org/TR/1999/REC-xpath-19991116 , section 3.4 
comp(CompFun,L,R) when is_list(L), is_list(R) ->
    lists:any(fun(LeftValue) ->
                     lists:any(fun(RightValue)->
                                 CompFun(LeftValue,RightValue) 
                               end, R)
              end, L);
comp(CompFun,L,R) when is_list(L) ->
    lists:any(fun(LeftValue) -> CompFun(LeftValue,R) end,L);
comp(CompFun,L,R) when is_list(R) ->
    lists:any(fun(RightValue) -> CompFun(L,RightValue) end,R);
comp(CompFun,L,R) ->
    CompFun(L,R).

comp_fun('=') -> 
    fun 
        (A,B) when is_number(A) -> A == mochiweb_xpath_utils:number_value(B);
        (A,B) when is_number(B) -> mochiweb_xpath_utils:number_value(A) == B;
        (A,B) when is_boolean(A) -> A == mochiweb_xpath_utils:boolean_value(B);
        (A,B) when is_boolean(B) -> mochiweb_xpath_utils:boolean_value(A) == B;
        (A,B) -> mochiweb_xpath_utils:string_value(A) == mochiweb_xpath_utils:string_value(B)
    end;

comp_fun('!=') ->
    fun(A,B) -> F = comp_fun('='),
                not F(A,B) 
    end;

comp_fun('>') ->
  fun(A,B) -> 
    mochiweb_xpath_utils:number_value(A) > mochiweb_xpath_utils:number_value(B) 
  end;
comp_fun('<') ->
  fun(A,B) -> 
    mochiweb_xpath_utils:number_value(A) < mochiweb_xpath_utils:number_value(B)
   end;
comp_fun('<=') ->
  fun(A,B) -> 
    mochiweb_xpath_utils:number_value(A) =< mochiweb_xpath_utils:number_value(B) 
  end;
comp_fun('>=') ->
  fun(A,B) -> 
    mochiweb_xpath_utils:number_value(A) >= mochiweb_xpath_utils:number_value(B) 
  end.

%%
%% Boolean functions
%%

bool_fun('and') ->
    fun(A, B) ->
            mochiweb_xpath_utils:boolean_value(A)
                andalso mochiweb_xpath_utils:boolean_value(B)
    end;
bool_fun('or') ->
    fun(A, B) ->
            mochiweb_xpath_utils:boolean_value(A)
                orelse mochiweb_xpath_utils:boolean_value(B)
    end.
%% TODO more boolean operators

%%
%% Arithmetic functions
%%

arith('+', Arg1, Arg2) ->
    mochiweb_xpath_utils:number_value(Arg1)
		+ mochiweb_xpath_utils:number_value(Arg2);
arith('-', Arg1, Arg2) ->
	mochiweb_xpath_utils:number_value(Arg1)
		- mochiweb_xpath_utils:number_value(Arg2);
arith('*', Arg1, Arg2) ->
	mochiweb_xpath_utils:number_value(Arg1)
		* mochiweb_xpath_utils:number_value(Arg2);
arith('div', Arg1, Arg2) ->
	mochiweb_xpath_utils:number_value(Arg1)
		/ mochiweb_xpath_utils:number_value(Arg2);
arith('mod', Arg1, Arg2) ->
	mochiweb_xpath_utils:number_value(Arg1)
		rem mochiweb_xpath_utils:number_value(Arg2).

%%
%% Helpers
%%

%% @doc Add a position to each node
%% @spec add_positions(Doc) -> ExtendedDoc
%% @type ExtendedDoc = {atom(), [{binary(), any()}], [extended_node()], [non_neg_integer()]}
add_positions(Node) ->
    R = add_positions_aux(Node, []),
    R.

add_positions_aux({Tag,Attrs,Children}, Position) ->
    {_, NewChildren} = lists:foldl(fun(Child, {Count, AccChildren}) ->
                NewChild = add_positions_aux(Child, [Count | Position]),
                {Count+1, [NewChild|AccChildren]}
        end, {1, []}, Children),
    {Tag, Attrs, lists:reverse(NewChildren), Position};
add_positions_aux(Data, _) ->
    Data.

%% @doc Remove position from each node
%% @spec remove_positions(ExtendedDoc) -> Doc
%% @type ExtendedDoc = {atom(), [{binary(), any()}], [extended_node()], [non_neg_integer()]}
remove_positions(Nodes) when is_list(Nodes) ->
    [ remove_positions(SubNode) || SubNode <- Nodes ];
remove_positions({Tag, Attrs, Children, _}) ->
    {Tag, Attrs, remove_positions(Children)};
remove_positions(Data) ->
    Data.

%% @doc Get node according to a position relative to root node
%% @spec get_node_at(ExtendedDoc, Position) -> ExtendedDoc
%% @type Position = [non_neg_integer()]
%% @type ExtendedDoc = {atom(), [{binary(), any()}], [extended_node()], [non_neg_integer()]}
get_node_at(Node, Position) ->
    get_node_at_aux(Node, lists:reverse(Position)).

get_node_at_aux(Node, []) ->
    Node;
get_node_at_aux({_,_,Children,_}, [Pos|Next]) ->
    get_node_at_aux(lists:nth(Pos, Children), Next).

%% @doc Get parent position
%% @spec get_parent_position(Position) -> Position
%% @type Position = [non_neg_integer()]
get_parent_position([_|ParentPosition]) ->
    ParentPosition.

%% @doc Get position relative to my parent
%% @spec get_self_position(Position) -> non_neg_integer()
%% @type Position = [non_neg_integer()]
get_position_in_parent([MyPosition|_]) ->
    MyPosition.
