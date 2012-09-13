% RESTt - REST Test Library
%
% @author Robert Döring

-module(restt).

-include_lib("ibrowse/include/ibrowse.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TYPEMARKER, xyz_type).


-export([
	start_link/0,
	prop_test/1,
	test1/0
	]).

% Record for static request.
% @todo export into a header file
-record(statreq, {host, path, params, method, header, body}).

%
%
%
run_test() ->
	A = 12,
	B = 32,
	?assert(A < B).

%
% @todo write a doc
% http://maps.googleapis.com/maps/api/geocode/json?address=Berlin,Germany&sensor=false
stat_req_test() ->
	A = #statreq{host="http://maps.googleapis.com", 
		path="/maps/api/geocode/json", 
		params=[{"address", "Berlin,Germany"}, 
				{"sensor", "false"}], 
		method=get, 
		header="", 
		body=""},

	io:format("Request: ~n~p~n", [A]),
	Res = stat_req(A),
	io:format("Result: ~n~p~n", [Res]),
	{Rc, _State, _Header, _Body} = Res,
	?assert( Rc == ok).

%
%
% @ todo Add state to check if ibrowse was initialized before.
start_link() ->
	ibrowse:start_link().

%
% @todo implement all attr. and dont use a record.
%
stat_req(P) ->
	Host = P#statreq.host,
	Path = P#statreq.path,
	Method = P#statreq.method,

	ibrowse:start_link(),
	ibrowse:send_req(string:concat(Host, Path), [], Method).


...hier gehts weiter ....
%
% 
%
param_to_str([{Key, Value}} | []]], String, NumOfParam) ->
	case NumOfParam of
		0 ->
			StringWithDelimiter = "?";
		_ ->
			StringWithDelimiter = string:concat(String, ",")
	end
	string:concat(StringWithDelimiter, string:concat("=", Value)).

prop_test(Repetitions) ->
	proper:quickcheck(test1(), Repetitions).

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
	

