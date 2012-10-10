% RESTt - REST Test Library
%
% @author Robert Doering

-module(restt).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TYPEMARKER, xyz_type).


-export([
	quickcheck/1,
	param_to_str/3,
	param_to_str/1,
	prop_test/1,
	test1/0
	]).


%
-record(resttcfg, {var_list = [], req_list = [], rep_list = [], test_list = []}).

% Record for static request.
% @todo export into a header file
% @todo don't use an record, usr 
%-record(statreq, {host, path, params, method, header, body}).

-record(var, {name, type=undefined, def=undefined}).
-record(request, {name, host, path, params, method, header, body}).
-record(reply, {name, match_list}).
-record(test, {name, request_name, reply_name, iter}).


%
% Run REST test
%
% @todo: Write a spec of config
%
quickcheck(Config) ->
	application:start(sasl),
	application:start(ibrowse),
	ListOfTests = Config#resttcfg.test_list,
	run_tests(Config, ListOfTests).

quickcheck_test() ->
	Vars = [{var, "vHours", {integer, 0, 24}},
			{var, "vMinutes", {integer, 0, 60}},
			{var, "vFloatPercent", {float, 0.0, 1.0}}],

	Requests = [{request, "req1", "http://maps.googleapis.com", "/maps/api/geocode/json", 
				[{"address", "Berlin,Germany"}, {"sensor", "false"}], get, undefined, undefined}],

	Replies = [{reply, 
				"rep1",  
				[	{status, 200},
					{header_part, 
						[	{"Content-Type","application/json; charset=UTF-8"},
							%{"Date","Sun, 30 Sep 2012 14:24:30 GMT"},
							%{"Expires","Mon, 01 Oct 2012 14:24:30 GMT"},
							{"Cache-Control","public, max-age=86400"},
							{"Vary","Accept-Language"},
							{"Server","mafe"},
							{"X-XSS-Protection","1; mode=block"},
							{"X-Frame-Options","SAMEORIGIN"},
							{"Transfer-Encoding","chunked"}]},
					{json_body, 
					{	obj,[{"results",
		   				 [{obj,[{"address_components",
				   				[{obj,[{"long_name",<<"Berlin">>},
						  				{"short_name",<<"Berlin">>},
						  				{"types",[<<"locality">>,<<"political">>]}]},
									{obj,[{"long_name",<<"Berlin">>},
						  				  {"short_name",<<"Berlin">>},
						  				  {"types", [<<"administrative_area_level_1">>, <<"political">>]}]},
									{obj,[{"long_name",<<"Germany">>},
						  				{"short_name",<<"DE">>},
						  				{"types",[<<"country">>,<<"political">>]}]}]},
				  				{"formatted_address",<<"Berlin, Germany">>},
				  				{"geometry",
				   				{obj,[{"bounds",
						  				{obj,[{"northeast",
								 				{obj,[{"lat",52.6754542},
									   				{"lng",13.7611176}]}},
												{"southwest",
								 				{obj,[{"lat",52.33962959999999},
									   				{"lng",13.0911663}]}}]}},
						 				{"location",
						  				{obj,[{"lat",52.519171},{"lng",13.4060912}]}},
						 				{"location_type",<<"APPROXIMATE">>},
						 				{"viewport",
						  				{obj,[{"northeast",
								 				{obj,[{"lat",52.6754542},
									   				{"lng",13.7611176}]}},
												{"southwest",
								 				{obj,[{"lat",52.33962959999999},
									   				{"lng",13.0911663}]}}]}}]}},
				  				{"types",[<<"locality">>,<<"political">>]}]}]},
		  				{"status",<<"OK">>}]}
					}
				] 
			}],
			
	Tests = [{test, "test1", "req1", "rep1", 100},
			{test, "test2", "req2", "rep1", 100},
			{test, "test3", "req1", "rep2", 100},
			{test, "test4", "req2", "rep2", 100},
			{test, "test5", "req1", "rep1", 100}],

	Config = #resttcfg{var_list=Vars, req_list=Requests, rep_list=Replies, test_list=Tests},
	quickcheck(Config).


%
% Go step by step through all tests
%
% @spec run_tests(Config, Tests) -> ok
% where
%	Config = list()
%	Tests = list()
% @end
%
run_tests(_Config, []) ->
	ok;
run_tests(Config, [Test | OtherTests]) ->
	io:format("Run Test ~p ~n", [Test#test.name]),
	io:format("    Send Request ~p ~n", [Test#test.request_name]),
	io:format("    Reply Rule ~p ~n", [Test#test.reply_name]),

	%
	% @todo
	% Is that the best way?
	% if func1() failed
	%	return error
	% if func2() failed
	% 	return error
	% return ok
	%
	case cfg_get_request_entry(Config, Test#test.request_name) of
		{error} -> 
			io:format("    Warning: missing request entry ~p~n", [Test#test.request_name]);
		{ok, ReqEntry} ->
			%io:format("    Request Content: ~n~p~n", [ReqEntry]),
			ServerReply = stat_req(ReqEntry#request.host, ReqEntry#request.path, ReqEntry#request.method, ReqEntry#request.params),
			
			case cfg_get_reply_entry(Config, Test#test.reply_name) of
				{error} ->
					io:format("    Warning: missing reply entry ~p~n", [Test#test.reply_name]);
				{ok, RepEntry} ->
					%
					% @todo here comes the proper loop
					%
					case evaluate_server_reply(Config, RepEntry#reply.match_list, ServerReply) of
						{ok} ->
							io:format("    Result: Passed~n");
						{failed, FailedReason} ->
							io:format("    Result: Failed (~p)~n", [FailedReason])
					end
			end

	end,
	run_tests(Config, OtherTests).


%%
%% @doc Evaluate response from server.
%%
%% @spec evaluate_server_reply(Config, ConditionList, ServerReply) -> Result
%% where
%%  Config = config()
%% 	ConditionList = {header, Header::list_of_key_value_tupels()} | {jbody, Body::rfc4627_oj()} | {status, Num::integer()}
%%	ServerReply =  {ok:atom(), Status::string(), ResponseHeaders::list_of_key_value_tupels(), ResponseBody::json_string()}
%%	Result = ok | {failed, What::kind_as_atom(), expected(), realResponse()}
%% @end
%% 
evaluate_server_reply(_Config, [], _ServerReply) ->
	ok;
evaluate_server_reply(Config, [Condition | ConditionList], ServerReply) ->
	case evaluate_server_reply(Config, Condition, ServerReply) of
		ok ->
			evaluate_server_reply(Config, ConditionList, ServerReply);
		FailedReason ->
			FailedReason
	end;
evaluate_server_reply(_Config, {header_exact, Headers}, {ok, _Status, ResponseHeaders, _ResponseBody}) ->
	HeadersSorted=lists:sort(Headers),
	ResponseHeadersSorted=lists:sort(ResponseHeaders),
	case HeadersSorted of
		ResponseHeadersSorted ->
			{ok};
		_Else ->
			{failed, [{missmatch, header_exact}, {expected, HeadersSorted}, {response, ResponseHeadersSorted}]}
	end;
evaluate_server_reply(_Config, {header_part, Headers}, {ok, _Status, ResponseHeaders, _ResponseBody}) ->
	Members = [X||X<-Headers, lists:member(X,ResponseHeaders)],
	MembersCount = length(Members),
	HeadersCount = length(Headers),
	case MembersCount of
		HeadersCount ->
			{ok};
		_Else ->
			{failed, [{missmatch, header}, {expected, Headers}, {response, ResponseHeaders}]}
	end;
evaluate_server_reply(Config, {json_body, ExpectedReplyBody}, {ok, _Status, _ResponseHeaders, ResponseBody}) ->
	{ok, ResponseBodyJson, _} = rfc4627:decode(ResponseBody),
	evaluate_json(Config, ExpectedReplyBody, ResponseBodyJson);
evaluate_server_reply(_Config, {status, Num}, {ok, Status, _ResponseHeaders, _ResponseBody}) ->
	{StatusInteger, _} = string:to_integer(Status),
	case Num of
		StatusInteger ->
			{ok};
		_Else ->
			{failed, [{missmatch, status}, {expected, Num}, {response, StatusInteger}]}
	end.


evaluate_json(_Config, [],[]) ->
    {ok};
evaluate_json(Config, {obj, ExpectedReply}, {obj, Reply}) ->
    %io:format("Object:    ~p~n",[ExpectedReply]),
    evaluate_json(Config, ExpectedReply, Reply);
% Here we look for vars at the key.
evaluate_json(Config, {#var{name=VarName}, ExpectedReplyValue}, {ReplyKey, ReplyValue}) ->
    %io:format("Var-Name for a Key:    ~p == ~p~n",[VarName, ReplyKey]),
    evaluate_var(Config, VarName, ReplyKey),
    evaluate_json(Config, ExpectedReplyValue, ReplyValue);
evaluate_json(Config, {Key, ExpectedReplyValue}, {Key, ReplyValue}) when is_list(Key)->
    %io:format("Key:    ~p ~n",[Key]),
    evaluate_json(Config, ExpectedReplyValue, ReplyValue);
evaluate_json(Config, [ExpectedReplyKeyValue | ExpectedReplyRest], [ReplyKeyValue | ReplyRest]) ->
    %io:format("Listelem:    ~p~n",[ExpectedReplyKeyValue]),
    case evaluate_json(Config, ExpectedReplyKeyValue, ReplyKeyValue) of
        ok -> evaluate_json(Config, ExpectedReplyRest, ReplyRest);
        FailedReason -> FailedReason
    end;
% Here we look for vars at the value.
evaluate_json(Config, #var{name=VarName}, ReplyValue) ->
    %io:format("Var-Name for a Value:    ~p == ~p~n",[VarName, ReplyValue]),
    evaluate_var(Config, VarName, ReplyValue);
evaluate_json(_Config, Value, Value) ->
    %io:format("Single Value:    ~p~n",[Value]),
	ok;
evaluate_json(_Config, ExpectedReplyValue, ReplyValue) ->
    {failed, {jbody, ExpectedReplyValue, ReplyValue}}.


evaluate_json_test() ->
	Vars = [#var{name="Var1", type=string, def=[]},
			#var{name="Var2", type=string, def=[]},
			#var{name="vHours", type=integer, def=[{min, 0},{max, 24}]},
			#var{name="vMinutes", type=integer, def=[{min, 0},{max, 60}]},
			#var{name="vFloatPercent", type=float, def=[{min, 0.0},{max, 1.0}]} ],

	Config = #resttcfg{var_list=Vars, req_list=undefined, rep_list=undefined, test_list=undefined},

	ExpectedReply0 = { obj,[{"result", #var{name="Var0"}}]},
	Reply0 = { obj,[{"result","super"}]},
	?assertMatch({failed, _}, evaluate_json(Config, ExpectedReply0, Reply0)),
	
	ExpectedReply1 = { obj,[{"result", #var{name="Var1"}}]},
	Reply1 = { obj,[{"result","super"}]},
	?assertMatch({ok}, evaluate_json(Config, ExpectedReply1, Reply1)),

	ExpectedReply2 = { obj,[{#var{name="Var2"}, "super"}]},
	Reply2 = { obj,[{"result","super"}]},
	?assertMatch({ok}, evaluate_json(Config, ExpectedReply2, Reply2)),

	ExpectedReply3 = { obj,[{"time", #var{name="vHours"}}]},
	Reply3 = { obj,[{"time",10}]},
	?assertMatch({ok}, evaluate_json(Config, ExpectedReply3, Reply3)),

	ExpectedReply4 = { obj,[{"time", #var{name="vHours"}}]},
	Reply4 = { obj,[{"time",61}]},
	?assertMatch({failed, _}, evaluate_json(Config, ExpectedReply4, Reply4)),

	ExpectedReply5 = { obj,[{"percent", #var{name="vFloatPercent"}}]},
	Reply5 = { obj,[{"percent",0.121211234}]},
	?assertMatch({ok}, evaluate_json(Config, ExpectedReply5, Reply5)),

	ExpectedReply6 = { obj,[{"percent", #var{name="vFloatPercent"}}]},
	Reply6 = { obj,[{"percent",1.021211234}]},
	?assertMatch({failed, _}, evaluate_json(Config, ExpectedReply6, Reply6)),

	ExpectedReply={	obj,[{"results",
		   				 [{obj,[{"address_components",
				   				[{obj,[{"long_name",<<"Berlin">>},
						  				{"short_name",<<"Berlin">>},
						  				{"types",[<<"locality">>,<<"political">>]}]},
									{obj,[{"long_name",<<"Berlin">>},
						  				  {"short_name",<<"Berlin">>},
						  				  {"types", [<<"administrative_area_level_1">>, <<"political">>]}]},
									{obj,[{"long_name",<<"Germany">>},
						  				{"short_name",<<"DE">>},
						  				{"types",[<<"country">>,<<"political">>]}]}]},
				  				{"formatted_address",<<"Berlin, Germany">>},
				  				{"geometry",
				   				{obj,[{"bounds",
						  				{obj,[{"northeast",
								 				{obj,[{"lat",52.6754542},
									   				{"lng",13.7611176}]}},
												{"southwest",
								 				{obj,[{"lat",52.33962959999999},
									   				{"lng",13.0911663}]}}]}},
						 				{"location",
						  				{obj,[{"lat",52.519171},{"lng",13.4060912}]}},
						 				{"location_type",<<"APPROXIMATE">>},
						 				{"viewport",
						  				{obj,[{"northeast",
								 				{obj,[{"lat",52.6754542},
									   				{"lng",13.7611176}]}},
												{"southwest",
								 				{obj,[{"lat",52.33962959999999},
									   				{"lng",13.0911663}]}}]}}]}},
				  				{"types",[<<"locality">>,<<"political">>]}]}]},
		  				{"status",<<"OK">>}]},

	Reply = {	obj,[{"results",
		   				 [{obj,[{"address_components",
				   				[{obj,[{"long_name",<<"Berlin">>},
						  				{"short_name",<<"Berlin">>},
						  				{"types",[<<"locality">>,<<"political">>]}]},
									{obj,[{"long_name",<<"Berlin">>},
						  				  {"short_name",<<"Berlin">>},
						  				  {"types", [<<"administrative_area_level_1">>, <<"political">>]}]},
									{obj,[{"long_name",<<"Germany">>},
						  				{"short_name",<<"DE">>},
						  				{"types",[<<"country">>,<<"political">>]}]}]},
				  				{"formatted_address",<<"Berlin, Germany">>},
				  				{"geometry",
				   				{obj,[{"bounds",
						  				{obj,[{"northeast",
								 				{obj,[{"lat",52.6754542},
									   				{"lng",13.7611176}]}},
												{"southwest",
								 				{obj,[{"lat",52.33962959999999},
									   				{"lng",13.0911663}]}}]}},
						 				{"location",
						  				{obj,[{"lat",52.519171},{"lng",13.4060912}]}},
						 				{"location_type",<<"APPROXIMATE">>},
						 				{"viewport",
						  				{obj,[{"northeast",
								 				{obj,[{"lat",52.6754542},
									   				{"lng",13.7611176}]}},
												{"southwest",
								 				{obj,[{"lat",52.33962959999999},
									   				{"lng",13.0911663}]}}]}}]}},
				  				{"types",[<<"locality">>,<<"political">>]}]}]},
		  				{"status",<<"OK">>}]},


	?assertMatch({ok}, evaluate_json(Config, ExpectedReply, Reply)).


evaluate_var(Config, VarName, Term) ->
	case cfg_get_var_entry(Config, VarName) of
		{error} -> 
			Msg = lists:flatten(io_lib:format("Variable ~p not declared.", [VarName])),
			{failed, [{missmatch, var}, {msg, Msg}]};
		
		{ok, VarEntry} ->
			io:format("Try to match ~p against ~p~n", [VarEntry, Term]),
			evaluate_var_type(VarEntry, Term)
	end.

evaluate_var_type(Var=#var{type=Type, name=VarName}, Term) ->
	case debutten:validate(Term, {Type}) of
		true ->
			evaluate_var_def(Var, Term);
		_Else ->
			Msg = lists:flatten(io_lib:format("Variable ~p does not match ~p.", [VarName, Term])),
			{failed, [{missmatch, var}, {msg, Msg}]}
	end.

evaluate_var_def(#var{def=[]}, _Term) ->
	{ok};
evaluate_var_def(Var=#var{name=VarName, def=[{min, Min} | Rest]}, Term) ->
	case Term >= Min  of
		true ->
			evaluate_var_def(Var#var{def=Rest}, Term);
		_Else ->
			Msg = lists:flatten(io_lib:format("Variable ~p(value:~p) is less than ~p.", [VarName, Term, Min])),
			{failed, [{missmatch, var},{mag, Msg}]}
	end;
evaluate_var_def(Var=#var{name=VarName, def=[{max, Max} | Rest]}, Term) ->
	case Term < Max  of
		true ->
			evaluate_var_def(Var#var{def=Rest}, Term);
		_Else ->
			Msg = lists:flatten(io_lib:format("Variable ~p(value:~p) is greater than ~p.", [VarName, Term, Max])),
			{failed, [{missmatch, var},{mag, Msg}]}
	end.



%
% Request for http://maps.googleapis.com/maps/api/geocode/json?address=Berlin,Germany&sensor=false
%
stat_req_test() ->
	A = #request{host="http://maps.googleapis.com", 
		path="/maps/api/geocode/json", 
		params=[{"address", "Berlin,Germany"}, 
				{"sensor", "false"}], 
		method=get, 
		header=undefined, 
		body=undefined},

	io:format("Request: ~n~p~n", [A]),
	Res = stat_req(A#request.host, A#request.path, A#request.method, A#request.params),
	io:format("Result: ~n~p~n", [Res]),
	{Rc, _State, _Header, _Body} = Res,
	?assert( Rc == ok).


%
% @todo implement all attr.
%
stat_req(Host, Path, Method, Params) ->
	ParamStr = param_to_str(Params),
	PathAndParams = string:concat(Path, ParamStr),
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
% @spec cfg_get_request_entry(Config, EntryName) -> {ok, Entry} | {error}
% where
%	Config = resttcfg()
%	EntryName = string()
%	Entry = request_record()
%
cfg_get_request_entry(Config, EntryName) ->
	
	case
		[E || E <- Config#resttcfg.req_list, is_record(E, request), E#request.name == EntryName] 
	of
		[Entry] -> 
			{ok, Entry}; 
		[] -> 
			{error}
	end.

%
% @spec cfg_get_reply_entry(Config, EntryName) -> {ok, Entry} | {error}
% where
%	Config = resttcfg()
%	EntryName = string()
%	Entry = request_record()
%
cfg_get_reply_entry(Config, EntryName) ->
	case
		[E || E <- Config#resttcfg.rep_list, is_record(E, reply), E#reply.name == EntryName]
	of
		[Entry] -> 
			{ok, Entry} ;
		[] ->
			{error}
	end.

%
% @spec cfg_get_var_entry(Config, EntryName) -> {ok, Entry} | {error}
% where
%	Config = resttcfg()
%	EntryName = string()
%	Entry = request_record()
%
cfg_get_var_entry(Config, EntryName) ->
	case
		[E || E <- Config#resttcfg.var_list, is_record(E, var), E#var.name == EntryName]
	of
		[Entry] -> 
			{ok, Entry} ;
		[] ->
			{error}
	end.


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
	F = fun (_Input) -> 
		%io:format("Input: ~p~n", [Input]),
		true==true
		end,
	io:format("hi~n"),

	proper:with_title("hello"),
	
	?FORALL(V, {integer(10, 20), bool()}, F(V)).
