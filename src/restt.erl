-module(restt).

-include_lib("ibrowse/include/ibrowse.hrl").
-include_lib("proper/include/proper.hrl").

-define(TYPEMARKER, xyz_type).


-export([
	test_stat_req/0,
	prop_test/1
	]).

% Record for static request.
-record(statreq, {host, path, params, method, header, body}).

%
% @doc 
%
test_stat_req() ->
	A = #statreq{host="http://www.web.de", path="/doc/en", params=[{"q", "name"}, {"Limit", "10"}], method=get, header="", body=""},
	stat_req(A).


%
%
% @ todo Add state to check if ibrowse was initialized before.
start_link() ->
	ibrowse:start_link().

%
% @todo implement all attr.
%
stat_req(P) ->
	Host = P#statreq.host,
	Path = P#statreq.path,
	Method = P#statreq.method,

	ibrowse:send_req(string:concat(Host, Path), [], Method).



prop_test(Repetitions) ->
	proper:quicktest_stat_req(test1(), Repetitions).

test1() ->
	{Min, Max} = {1.0, 100.0},
	Doc = [
		{"factor1", {xyz_type, float, [Min, Max]}}
		, {"q", "lala"}
		, {"factor2", {xyz_type, float, [Min, Max]}}
		, {"q", "hua"}
	],
	Types = extract_types(Doc),
	Evaluator = evaluator(),
	?FORALL(T, Types, Evaluator(T, Doc)).

evaluator() ->
	fun(ArgsTupel, Doc) ->
		Args = tuple_to_list(ArgsTupel),
		DocReplaced = insert_values_for_types(Args, Doc),
		io:format("DocReplaced: ~p\n", [DocReplaced]),
		% do the actual server call here
		true
	end.

extract_types(Doc) ->
	list_to_tuple(
		[make_type(Fun, Args)|| E = {_Key, {?TYPEMARKER, Fun, Args}} <- Doc, is_value_with_type(E)]
	).

insert_values_for_types([], DocElements) ->
	DocElements;

insert_values_for_types([V|ValuesRest], [D|DocElementRest]) ->
	case is_value_with_type(D) of
		true ->
			% we replace the doc element with the value
			[insert_value(V, D)|insert_values_for_types(ValuesRest, DocElementRest)];
		false ->
			% the doc element is static, so we pass
			[D|insert_values_for_types([V|ValuesRest], DocElementRest)]
	end.

% we can't dynamically make proper types like:
% erlang:apply(?MODULE, Fun, Args)
% so we have to spell them all out.
% find all types in http://proper.softlab.ntua.gr/papers/proper_types.pdf
make_type(float, [Min, Max]) ->
	float(Min, Max).

is_value_with_type({_, {?TYPEMARKER, _, _}}) ->
	true;
is_value_with_type(_) ->
	false.

insert_value(Value, {Key, {?TYPEMARKER, _, _}}) ->
	{Key, Value}.
	

