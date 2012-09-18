% RESTt - REST Test Library
%
% @author Robert Döring

-module(restt).

-include_lib("ibrowse/include/ibrowse.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TYPEMARKER, xyz_type).


-export([
	quickcheck/1,
	param_to_str/3,
	param_to_str/1,
	start_link/0,
	prop_test/1,
	test1/0
	]).

% Record for static request.
% @todo export into a header file
% @todo don't use an record, usr 
-record(statreq, {host, path, params, method, header, body}).

-record(request, {name, host, path, params, method, header, body}).
-record(reply, {name, match_list}).
-record(test, {name, request_name, reply_name, iter}).

% Storages for lists of variables, requests, replies and tests
-record(storage, {vars, requests, replies, tests}).


%
% Run REST test
%
quickcheck(Config) ->
	ListOfTests = cfg_get_list_of_tests(Config),
	run_tests(Config, ListOfTests).

quickcheck_test() ->
	Config = [
			{var, "vHours", {integer, 0, 24}},
			{var, "vMinutes", {integer, 0, 60}},
			{var, "vFloatPercent", {float, 0.0, 1.0}},

			{request, "req1", "http://maps.googleapis.com", "/maps/api/geocode/json", 
				[{"address", "Berlin,Germany"}, {"sensor", "false"}], get, "", ""},

			{reply, "rep1",  [{status, 200}] },

			{test, "test1", "req1", "rep1", 100}
		],
	quickcheck(Config).


%
% Go step by step through all tests
%
% @spec run_tests(Config, Tests) -> ok | ???
% where
%	Config = list()
%	Tests = list()
%	ok = atom()
%
run_tests(Config, [Test | OtherTests]) ->
	io:format("Run Test ~p ~n", [Test#test.name]),

	try cfg_get_request_entry(Config, Test#test.request_name) of
		N -> ReqEntry = N
	catch
		_:_ -> 
			Trace = erlang:get_stacktrace(),
			Reason = {badarg, string:format("processing ~p: missing request entry ~p", [Test#test.name, Test#test.request_name])},
			erlang:raise(error, Reason, Trace)
	end,
	io:format("    Send Request ~p ~n", [Test#test.request_name]),
	io:format("    Request Content: ~n~p~n", [ReqEntry]),
	ServerReply = stat_req(ReqEntry#request.host, ReqEntry#request.path, ReqEntry#request.method, ReqEntry#request.params),
	
	try cfg_get_reply_entry(Config, Test#test.reply_name) of
		N -> RepEntry = N
	catch
		_:_ -> 
			Trace = erlang:get_stacktrace(),
			Reason = {badarg, string:format("processing ~p: missing reply entry ~p", [Test#test.name, Test#test.reply_name])},
			erlang:raise(error, Reason, Trace)
	end,
	io:format("    Reply Rule ~p ~n", [Test#test.reply_name]).


%
%
%
run_test() ->
	A = 12,
	B = 32,
	?assert(A < B).

%
% Request for http://maps.googleapis.com/maps/api/geocode/json?address=Berlin,Germany&sensor=false
%
stat_req_test() ->
	A = #request{host="http://maps.googleapis.com", 
		path="/maps/api/geocode/json", 
		params=[{"address", "Berlin,Germany"}, 
				{"sensor", "false"}], 
		method=get, 
		header="", 
		body=""},

	io:format("Request: ~n~p~n", [A]),
	Res = stat_req(A#request.host, A#request.path, A#request.method, A#request.params),
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
stat_req(Host, Path, Method, Params) ->
	ParamStr = param_to_str(Params),
	PathAndParams = string:concat(Path, ParamStr),

	ibrowse:start_link(),
	ibrowse:send_req(string:concat(Host, PathAndParams), [], Method).


%
% Combine parameters to one string
%
param_to_str(ListOfParams) ->
	param_to_str(ListOfParams, "", 0).

param_to_str([{Key, Value} | Rest], IncompleteResultString, NumOfParam) ->
	case NumOfParam of
		0 ->
			StringWithDelimiter = "?";
		_ ->
			StringWithDelimiter = string:concat(IncompleteResultString, "&")
	end,
	
	param_to_str(Rest, string:concat(StringWithDelimiter, string:concat(Key, string:concat("=", Value))), NumOfParam + 1);

param_to_str([], ResultString, _) ->
	ResultString.



%
% Configuration
%

%
% @spec cfg_get_list_of_tests(Config) -> Tests
% where
%	Config = list()
%	Tests = list()
%
cfg_get_list_of_tests(ConfigList) ->
	[Tests || Tests <- ConfigList, is_record(Tests, test)].

%
% @spec cfg_get_request_entry(Config, EntryName) -> Entry
% where
%	Config = list()
%	EntryName = string()
%	Entry = request_record()
%
% @todo Maybe it would be better to look if the entry was found.
%
cfg_get_request_entry(ConfigList, EntryName) ->
	[Entry] = [E || E <- ConfigList, is_record(E, request), E#request.name == EntryName],
	Entry.

%
% @spec cfg_get_reply_entry(Config, EntryName) -> Entry
% where
%	Config = list()
%	EntryName = string()
%	Entry = reply_record()
%
% @todo Maybe it would be better to look if the entry was found.
%
cfg_get_reply_entry(ConfigList, EntryName) ->
	[Entry] = [E || E <- ConfigList, is_record(E, reply), E#reply.name == EntryName],
	Entry.


%
% @toso remove
%
config_to_map_test() ->
	Config = [
			{var, "vHours", {integer, 0, 24}},
			{var, "vMinutes", {integer, 0, 60}},
			{var, "vFloatPercent", {float, 0.0, 1.0}},

			{request, "req1", "http://maps.googleapis.com", "/maps/api/geocode/json", 
				[{"address", "Berlin,Germany"}, {"sensor", "false"}], "", "", get},

			{reply, "rep1",  [{status, 200}] },

			{test, "test1", "req1", "rep1", []}
		],

	ListOfVars = [V || V <- Config, is_record(V, test)].
	%ListOfSearchResult = [R || R = {var, "vMinutes", _Type} <- Config].


varlist_to_map_test() ->
	L = [
			{var, "vHours", {integer, 0, 24}},
			{var, "vMinutes", {integer, 0, 60}},
			{var, "vFloatPercent", {float, 0.0, 1.0}}
		],
	Ets = varlist_to_map(L),
	?assert(length(ets:lookup(Ets, "vHours")) == 1),
	?assert(length(ets:lookup(Ets, "vHours2")) == 0),
	?assert(length(ets:lookup(Ets, "vMinutes")) == 1),
	?assert(length(ets:lookup(Ets, "vFloatPercent")) == 1),
	?assert(length(ets:lookup(Ets, "nix")) == 0),
	Ets.


%
% Reads inall variables into a ETS and return this.
%
varlist_to_map(VarList) ->
	Storage = ets:new(restt_vars, []),
	varlist_to_map(Storage, VarList).

varlist_to_map(Storage, [{var, Name, Content} | Rest]) ->
	ets:insert(Storage, {Name, Content}),
	varlist_to_map(Storage, Rest);	
varlist_to_map(Storage, []) ->
	Storage.

%
% Proper testing
%

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
	

%
%
%
m_test() ->

	
	proper:quickcheck(measure("Der Titel", [2, 30, 60], mprop_test())).

mprop_test() ->
	F = fun (Input) -> 
		%io:format("Input: ~p~n", [Input]),
		true==true
		end,
	io:format("hi~n"),

    proper:with_title("hello"),
	
	?FORALL(V, {integer(10, 20), bool()}, F(V)).