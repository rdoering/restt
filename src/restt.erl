%%% RESTt - REST Test Library
%%%

%%% @copyright 2012 Robert Doering
%%% @author Robert Doering
%%% @version {@version}

%%% @doc  
%%% RESTt is a acronym for REST Test, it's a library to test a RESTful-API. 
%%% == General ==
%%% 
%%% == How to write a test ==
%%% 

-module(restt).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TYPEMARKER, xyz_type).

-export([
	quickcheck/1,
	run_quickcheck_example/0,
	param_to_str/3,
	param_to_str/1,
	tool_get_response_as_json/1
	]).

-export_type([resttcfg/0]).

-record(resttcfg, {placeholder_list = [], req_list = [], rep_list = [], test_list = []}).
-type resttcfg() :: #resttcfg{placeholder_list::var()}.

-type min_value() :: number().
-type max_value() :: number().

-record(var, {name, type=undefined, is_generated=false, value=undefined, def={}}).
-type var() :: #var{name::string(), 
					type::'integer'|'float'|'string'|'undefined', 
					is_generated::boolean(), 
					value::term(),
					def::{min_value(), max_value()}|{} }.

-record(const, {name, type=undefined, value=undefined, def={}}).
-type const() :: #const{
					name::string(), 
					type::'integer'|'float'|'string'|'undefined', 
					value::term(),
					def::{min_value(), max_value()}|{} }.
-record(constref, {name}).
-type constref() :: #constref{name::string()}.
-record(varref, {name}).
-type varref() :: #varref{name::string()}.
-type varlist() :: [const() | var()].

-record(constcombo, {fmt = "", names=[]}).
-type constcombo() :: #constcombo{ fmt::string(), names::[string()] }.

%% Defined at http://tonyg.github.com/erlang-rfc4627/doc/
-type json() :: jsonobj() | jsonarray() | jsonnum() | jsonstr() | true | false | null.
-type jsonobj() :: {obj, [{jsonkey(), json()}]}.
-type jsonkey() :: string().
-type jsonarray() :: [json()].
-type jsonnum() :: integer() | float().
-type jsonstr() :: binary().

-type keyValueList() :: [{string(), string()}].
-record(request, {name, host, path="", params=[], method, header=[], body=[]}).	
-type request() :: #request{name::string(),
						host::string(),
						path::string(),
						params::keyValueList(),
						method::atom(),
						header::keyValueList(),
						body::json()}.

-record(reply, {name, match_list}).
-record(test, {name, request_name, reply_name, iter}).

-type response() :: {ok, Status::integer(), ResponseHeaders::term(), ResponseBody::term()} 
					| {ibrowse_req_id, Req_id::term()} | {error, Reason::term()}.


%
% Combine parameters to one URL string
%
-spec param_to_str(varlist(), keyValueList()) -> UrlParams::string().
param_to_str(VarList, KeyValueList) ->
	param_to_str(generate_static_keyvaluepairs(VarList, KeyValueList)).

param_to_str(undefined) ->
	"";
param_to_str(ListOfParams) ->
	param_to_str(ListOfParams, "", 0).

param_to_str([{Key, Value} | Rest], IncompleteResultString, NumOfParam) ->
	case NumOfParam of
		0 -> StringWithDelimiter = "?";
		_ -> StringWithDelimiter = string:concat(IncompleteResultString, "&")
	end,
	NewIncompleteResultString = string:concat(StringWithDelimiter, string:concat(Key, string:concat("=", Value))),
	param_to_str(Rest, NewIncompleteResultString, NumOfParam + 1);
param_to_str([], ResultString, _) ->
	ResultString.


%
%
%
initiate_libs() ->
	application:start(sasl),
	application:start(ibrowse).


%
% Run REST test
%
% @todo: Write a spec of config
%
-spec quickcheck(resttcfg()) -> true | false.
quickcheck(Config) ->
	initiate_libs(),

	InitializedConfig = initiate_restt_config(Config),

	ListOfTests = InitializedConfig#resttcfg.test_list,
	try run_tests(InitializedConfig, ListOfTests) 
	of	_ -> true
	catch
		throw:{warning, Msg} -> 
			io:format("Warning: ~p~n", [Msg]),
			false;
		_:_-> false
	end.


%
% Go step by step through all tests
%
% @spec run_tests(Config, Tests) -> true | false
% where
%	Config = list()
%	Tests = list()
% @end
%
run_tests(_Config, []) ->
	true;
run_tests(Config, [Test | OtherTests]) ->
	{ok, RequestEntry} = cfg_get_request_entry(Config, Test#test.request_name),
	{ok, ReplyEntry} = cfg_get_reply_entry(Config, Test#test.reply_name),
	proper:quickcheck(outer_proper_test(Config, RequestEntry, ReplyEntry)),
	run_tests(Config, OtherTests).


%
% @spec cfg_get_request_entry(Config, EntryName) -> {ok, Entry} | {error}
% where
%	Config = resttcfg()
%	EntryName = string()
%	Entry = request_record()
%
cfg_get_request_entry(Config, EntryName) ->
	case [E || E <- Config#resttcfg.req_list, is_record(E, request), E#request.name == EntryName] 
	of
		[Entry] -> {ok, Entry}; 
		[] ->
			Msg = lists:flatten(io_lib:format("Warning: missing request entry ~p~n", [EntryName])),
			throw({warning, Msg})
	end.


%
% @spec cfg_get_reply_entry(Config, EntryName) -> {ok, Entry} | {error}
% where
%	Config = resttcfg()
%	EntryName = string()
%	Entry = request_record()
%
cfg_get_reply_entry(Config, EntryName) ->
	case [E || E <- Config#resttcfg.rep_list, is_record(E, reply), E#reply.name == EntryName]
	of
		[Entry] -> {ok, Entry};
		[] ->
			Msg = lists:flatten(io_lib:format("Warning: missing reply entry ~p~n", [EntryName])),
			throw({warning, Msg})
	end.


outer_proper_test(Config, RequestEntry, ReplyEntry) ->
	?FORALL(ConfigWithGeneratedVars, Config, inner_proper_test(ConfigWithGeneratedVars, RequestEntry, ReplyEntry)).


inner_proper_test(Config, RequestEntry, ReplyEntry) -> 
	ServerReply = http_request(Config#resttcfg.placeholder_list, RequestEntry),
	try evaluate_server_reply(Config, ReplyEntry#reply.match_list, ServerReply) 
	of {ok} -> true
	catch
		throw:{failed, Reason} ->
			io:format("Failed: ~p~n", [Reason]),
			false;
		_:_ -> false
	end.


-spec http_request(varlist(), request()) -> response().
http_request(VarList, Request=#request{}) ->
	ParamStr = param_to_str(VarList, Request#request.params),
	PathAndParams = string:concat(Request#request.path, ParamStr),
	Url = string:concat(Request#request.host, PathAndParams),
	ibrowse:send_req(Url, Request#request.header, Request#request.method, Request#request.body, []).


%%
%% @doc Evaluate response from server.
%%
%% @spec evaluate_server_reply(Config, ConditionList, ServerReply) -> Result
%% where
%%  Config = config()
%% 	ConditionList = {header, Header::list_of_key_value_tupels()} | {jbody, Body::rfc4627_obj()} | {status, Num::integer()}
%%	ServerReply =  {ok:atom(), Status::string(), ResponseHeaders::list_of_key_value_tupels(), ResponseBody::json_string()}
%%	Result = ok | {failed, What::kind_as_atom(), expected(), realResponse()}
%% @end
%% 
evaluate_server_reply(_Config, [], _ServerReply) ->
	{ok};
evaluate_server_reply(Config, [{header_exact, Headers} | ConditionRest], {ok, _Status, ResponseHeaders, _ResponseBody} = ServerReply) ->
	HeadersSorted=lists:sort(Headers),
	ResponseHeadersSorted=lists:sort(ResponseHeaders),
	case HeadersSorted of
		ResponseHeadersSorted -> evaluate_server_reply(Config, ConditionRest, ServerReply);
		_Else ->
			Msg = lists:flatten(io_lib:format("Expected exactly header ~p does not match ~p.", [HeadersSorted, ResponseHeadersSorted])),
			throw({failed, Msg})
	end;
evaluate_server_reply(Config, [{header_part, Headers} | ConditionRest], {ok, _Status, ResponseHeaders, _ResponseBody} = ServerReply) ->
	%TODO: check each header
	Members = [X||X<-Headers, lists:member(X,ResponseHeaders)],
	MembersCount = length(Members),
	HeadersCount = length(Headers),
	case MembersCount of
		HeadersCount -> evaluate_server_reply(Config, ConditionRest, ServerReply);
		_Else ->
			Msg = lists:flatten(io_lib:format("Expected partially header ~p does not match ~p.", [Headers, ResponseHeaders])),
			throw({failed, Msg})
	end;
evaluate_server_reply(Config, [{json_body, ExpectedReplyBody} | ConditionRest], {ok, _Status, _ResponseHeaders, ResponseBody} = ServerReply) ->
	{ok, ResponseBodyJson, _} = rfc4627:decode(ResponseBody),
	evaluate_json(Config, ExpectedReplyBody, ResponseBodyJson),
	evaluate_server_reply(Config, ConditionRest, ServerReply);
evaluate_server_reply(Config, [{status, Num} | ConditionRest], {ok, Status, _ResponseHeaders, _ResponseBody} = ServerReply) ->
	{StatusInteger, _} = string:to_integer(Status),
	case Num of
		StatusInteger -> evaluate_server_reply(Config, ConditionRest, ServerReply);
		_Else ->
			Msg = lists:flatten(io_lib:format("Expected response status code is ~p and not ~p.", [Num, StatusInteger])),
			throw({failed, Msg})
	end.


evaluate_json(_Config, [],[]) ->
    {ok};
evaluate_json(Config, {obj, ExpectedReply}, {obj, Reply}) ->
	evaluate_json(Config, ExpectedReply, Reply);
evaluate_json(Config, {#constref{name=VarName}, ExpectedReplyValue}, {ReplyKey, ReplyValue}) ->
    evaluate_Placeholder(Config, VarName, ReplyKey),
    evaluate_json(Config, ExpectedReplyValue, ReplyValue);
evaluate_json(Config, {Key, ExpectedReplyValue}, {Key, ReplyValue}) when is_list(Key)->
    evaluate_json(Config, ExpectedReplyValue, ReplyValue);
evaluate_json(Config, [ExpectedReplyKeyValue | ExpectedReplyRest], [ReplyKeyValue | ReplyRest]) ->
    evaluate_json(Config, ExpectedReplyKeyValue, ReplyKeyValue),
    evaluate_json(Config, ExpectedReplyRest, ReplyRest);
evaluate_json(Config, #varref{name=VarName}, ReplyValue) ->
	evaluate_Placeholder(Config, VarName, ReplyValue);
evaluate_json(Config, #constref{name=VarName}, ReplyValue) ->
    evaluate_Placeholder(Config, VarName, ReplyValue);
evaluate_json(_Config, Value, Value) ->
    {ok};
evaluate_json(_Config, ExpectedReplyValue, ReplyValue) ->
	Msg = lists:flatten(io_lib:format("Pattern ~p does not match ~p.", [ExpectedReplyValue, ReplyValue])),
	throw({failed, Msg}).

evaluate_Placeholder(Config, VarName, Term) ->
	case cfg_get_placeholder_entry(Config, VarName) of
		{error} -> 
			Msg = lists:flatten(io_lib:format("Variable ~p not declared.", [VarName])),
			throw({failed, Msg});
		
		{ok, VarEntry} ->
			evaluate_Placeholder_type(VarEntry, Term),
			evaluate_Placeholder_value(VarEntry, Term),
			evaluate_Placeholder_def(VarEntry, Term)
	end.

evaluate_Placeholder_type(#var{type=Type, name=PlaceholderName}, Term) ->
	evaluate_Placeholder_type(Type, PlaceholderName, Term);
evaluate_Placeholder_type(#const{type=Type, name=PlaceholderName}, Term) ->
	evaluate_Placeholder_type(Type, PlaceholderName, Term).

evaluate_Placeholder_type(Type, PlaceholderName, Term) ->
	case debutten:validate(Term, {Type}) of
		false ->
			Msg = lists:flatten(io_lib:format("Placeholder ~p does not match ~p.", [PlaceholderName, Term])),
			throw({failed, Msg});
		true -> 
			{ok}
	end.

evaluate_Placeholder_value(#var{}, _Term) ->
	{ok};
evaluate_Placeholder_value(#const{value=Value, name=VarName}, Term) ->
	case Term of
		Value ->
			{ok};
		_Else ->
			Msg = lists:flatten(io_lib:format("Const ~p (Value:~p) does not match ~p.", [VarName, Value, Term])),
			throw({failed, Msg})
	end.

evaluate_Placeholder_def(#const{def={}}, _Term) ->
	{ok};
evaluate_Placeholder_def(#var{def={}}, _Term) ->
	{ok};
evaluate_Placeholder_def(#var{name=VarName, def={Min, Max}}, Term) ->
	try
		true = Term >= Min,
		true = Term =< Max
	catch
		_:_ -> 
			Msg = lists:flatten(io_lib:format("Variable ~p(value:~p) is not in btw. ~p and ~p.", [VarName, Term, Min, Max])),
			throw({failed, Msg})
	end.


%
% @spec cfg_get_placeholder_entry(Config, EntryName) -> {ok, Entry} | {error}
% where
%	Config = resttcfg()
%	EntryName = string()
%	Entry = request_record()
%
cfg_get_placeholder_entry(Config, EntryName) ->
	case cfg_get_const_entry(Config, EntryName)
	of
		{ok, Entry} -> {ok, Entry} ;
		_ -> cfg_get_var_entry(Config, EntryName)
	end.
cfg_get_var_entry(Config, EntryName) ->
	case [E || E <- Config#resttcfg.placeholder_list, is_record(E, var), E#var.name == EntryName]
	of
		[Entry] -> {ok, Entry} ;
		[] -> {error}
	end.
cfg_get_const_entry(Config, EntryName) ->
	case [E || E <- Config#resttcfg.placeholder_list, is_record(E, const), E#const.name == EntryName]
	of
		[Entry] -> {ok, Entry} ;
		[] -> {error}
	end.



%
%
%
-spec initiate_restt_config(resttcfg()) -> resttcfg().
initiate_restt_config(Config = #resttcfg{placeholder_list=Vars}) -> 
	NewVars = initiate_consts_as_proper_variables(Vars, []),
	Config#resttcfg{placeholder_list=NewVars}.

initiate_consts_as_proper_variables([VarEntry=#var{} | Rest], Generated_Value_List) ->
	initiate_consts_as_proper_variables(Rest, [VarEntry | Generated_Value_List]);
initiate_consts_as_proper_variables([ConstEntry=#const{type=float, def={Min, Max}} | Rest], Generated_Value_List) ->
	initiate_consts_as_proper_variables(Rest, [ConstEntry#const{value=float(Min, Max)} | Generated_Value_List]);
initiate_consts_as_proper_variables([ConstEntry=#const{type=float, def={}} | Rest], Generated_Value_List) ->
	initiate_consts_as_proper_variables(Rest, [ConstEntry#const{value=float()} | Generated_Value_List]);
initiate_consts_as_proper_variables([ConstEntry=#const{type=integer, def={Min, Max}} | Rest], Generated_Value_List) ->
	initiate_consts_as_proper_variables(Rest, [ConstEntry#const{value=integer(Min, Max)} | Generated_Value_List]);
initiate_consts_as_proper_variables([ConstEntry=#const{type=integer, def={}} | Rest], Generated_Value_List) ->
	initiate_consts_as_proper_variables(Rest, [ConstEntry#const{value=integer()} | Generated_Value_List]);
initiate_consts_as_proper_variables([ConstEntry=#const{type=string} | Rest], Generated_Value_List) ->
	initiate_consts_as_proper_variables(Rest, [ConstEntry#const{value=string()} | Generated_Value_List]);
initiate_consts_as_proper_variables([], Generated_Value_List) ->
	Generated_Value_List.


%%
%% Take the value for each const of ParameterList and create a new ParameterValueList.
%% Use these list to create a the result-string. 
%%
-spec convert_constcombo_to_string(varlist(), constcombo()) -> {error, string()} | {ok, string()}.
convert_constcombo_to_string(Vars, #constcombo{fmt=Fmt, names=ParameterList}) ->
	{ok, ParameterValueList} = convert_constcombo_to_string(Vars, lists:reverse(ParameterList), []),
	Result = lists:flatten(io_lib:format(Fmt, ParameterValueList)),
	{ok, Result}.

convert_constcombo_to_string(Vars, [Const_name | Rest], ConvertedList) ->
	case get_value_of_const(Vars, Const_name) of
		{ok, StaticValue} ->
			convert_constcombo_to_string(Vars, Rest, [StaticValue | ConvertedList]);
		Error ->
			Error
	end;
convert_constcombo_to_string(_Vars, [], CompleteConvertedParameterList) ->
	{ok, CompleteConvertedParameterList}.


%%
%%
%%
-spec get_value_of_const(varlist(), string()) -> {ok, term()} | {error}.
get_value_of_const(VarList, ConstName) ->
	ValueList = [V || #const{value=V, name=Name} <- VarList, Name == ConstName],
	case ValueList of
		[] ->
			{error, lists:flatten(io_lib:format("Can't find const ~p.", [ConstName]))};
		[FirstValue | _] ->
			{ok, FirstValue}
	end.


%%
%% 
%%
-spec generate_static_request(varlist(), request()) -> request().
generate_static_request(VarList, Request) ->
	StaticParams = generate_static_keyvaluepairs(VarList, Request#request.params),
	StaticHeader = generate_static_keyvaluepairs(VarList, Request#request.header),
	StaticBody = generate_static_body(VarList, Request#request.body),
	Request#request{params=StaticParams, header=StaticHeader, body=StaticBody}.


-spec generate_static_keyvaluepairs(varlist(), keyValueList()) -> {ok, keyValueList()} | {error, string()}.
generate_static_keyvaluepairs(VarList, KeyValueList) ->
	generate_static_keyvaluepairs(VarList, lists:reverse(KeyValueList), []).

-spec generate_static_keyvaluepairs(varlist(), keyValueList(), keyValueList()) -> {ok, keyValueList()} | {error, string()}.
generate_static_keyvaluepairs(VarList, [ {Key, Value} | Rest], GeneratedList) ->
	StaticKey = generate_static_string(VarList, Key),
	StaticValue = generate_static_string(VarList, Value),
	generate_static_keyvaluepairs(VarList, Rest, [{StaticKey, StaticValue} | GeneratedList]);
generate_static_keyvaluepairs(_VarList, [], FinalGeneratedList) ->
	FinalGeneratedList.


-spec generate_static_body(varlist(), {json_body, json()}) -> {ok, json()} | {error}.
generate_static_body(VarList, {json_body, JsonBody}) ->
	generate_static_jsonbody(VarList, JsonBody);
generate_static_body(_VarList, UnknownBody) ->
	UnknownBody.

-spec generate_static_jsonbody(varlist(), json()) -> json().
generate_static_jsonbody(VarList, {obj, KeyValueList}) ->
    {obj, generate_static_json_keyvalue_list(VarList, KeyValueList)};
generate_static_jsonbody(_VarList, []) ->
	[];
generate_static_jsonbody(VarList, [Json | RestJsonList]) ->
	[generate_static_jsonbody(VarList, Json) | generate_static_jsonbody(VarList, RestJsonList)];
generate_static_jsonbody(VarList, #constref{name=Name})->
	{ok, Value} = get_value_of_const(VarList, Name),
	Value;
generate_static_jsonbody(VarList, ComboToGet=#constcombo{})->
	convert_constcombo_to_string(VarList, ComboToGet);
generate_static_jsonbody(_VarList, Term) ->
	Term.

generate_static_json_keyvalue_list(_VarList, []) ->
	[];
generate_static_json_keyvalue_list(VarList, [{Key, Value} | Rest]) ->
	[{generate_static_string(VarList, Key), generate_static_jsonbody(VarList, Value)} | generate_static_json_keyvalue_list(VarList, Rest)].


-spec generate_static_string(varlist(), constcombo() | constref() | term()) -> string(). 
generate_static_string(VarList, V)->
	Term = generate_static_value(VarList, V),
	convert_to_string_if_necessary(Term).


-spec convert_to_string_if_necessary(string() | term()) -> string().
convert_to_string_if_necessary(List) when is_list(List) ->
	case io_lib:printable_list(List) of
		true  ->
			List;
		_Else -> 
			io_lib:format("~p", [List])
	end;
convert_to_string_if_necessary(Term) ->
	[String] = io_lib:format("~p", [Term]),
	String.


-spec generate_static_value(varlist(), constcombo() | constref() | term()) -> term(). 
generate_static_value(VarList, V=#constcombo{})->
	{ok, Value} = convert_constcombo_to_string(VarList, V),
	Value;
generate_static_value(VarList, #constref{name=Name})->
	{ok, Value} = get_value_of_const(VarList, Name),
	Value;
generate_static_value(_VarList, Content) ->
	Content.


%
%
%
tool_get_response_as_json(Request) ->
	tool_get_response_as_json(Request, false).

tool_get_response_as_json(Request = #request{}, GetValue) ->
	initiate_libs(),

	Response = http_request(Request),
	case Response of
		{ok, _Status, _ResponseHeaders, ResponseBody} ->
			case rfc4627:decode(ResponseBody) of
				{ok, ResponseBodyJson, _} ->
					case GetValue of
						true ->
							ResponseBodyJson;
						_Else ->
							io:format("~p~n", [ResponseBodyJson])
					end;
				_Else ->
					{error, "JSON-parser failed", ResponseBody}
			end;
		_Else ->
			{error, "Request failed", Request, Response} 
	end;
tool_get_response_as_json(SingleURI, GetValue) ->
	tool_get_response_as_json(#request{host=SingleURI, method=get}, GetValue). 


-spec http_request(request()) -> response().
http_request(Request) ->
	http_request([], Request).


evaluate_json_test() ->
	Vars = [#var{name="Var1", type=string, def={}},
			#var{name="Var2", type=string, def={}},
			#var{name="vHours", type=integer, def={0, 24}},
			#var{name="vMinutes", type=integer, def={0, 60}},
			#var{name="vFloatPercent", type=float, def={0.0, 1.0}} ],

	Config = #resttcfg{placeholder_list=Vars, req_list=undefined, rep_list=undefined, test_list=undefined},

	Pattern0 = { obj,[{"result", #constref{name="Var0"}}]},
	Input0 = { obj,[{"result","super"}]},
	?assertMatch({failed, _}, evaluate_json(Config, Pattern0, Input0)),
	
	Pattern1 = { obj,[{"result", #constref{name="Var1"}}]},
	Input1 = { obj,[{"result","super"}]},
	?assertMatch({ok}, evaluate_json(Config, Pattern1, Input1)),

	Pattern2 = { obj,[{#constref{name="Var2"}, "super"}]},
	Input2 = { obj,[{"result","super"}]},
	?assertMatch({ok}, evaluate_json(Config, Pattern2, Input2)),

	Pattern3 = { obj,[{"time", #constref{name="vHours"}}]},
	Input3 = { obj,[{"time",10}]},
	?assertMatch({ok}, evaluate_json(Config, Pattern3, Input3)),

	Pattern4 = { obj,[{"time", #constref{name="vHours"}}]},
	Input4 = { obj,[{"time",61}]},
	?assertMatch({failed, _}, evaluate_json(Config, Pattern4, Input4)),

	Pattern5 = { obj,[{"percent", #constref{name="vFloatPercent"}}]},
	Input5 = { obj,[{"percent",0.121211234}]},
	?assertMatch({ok}, evaluate_json(Config, Pattern5, Input5)),

	Pattern6 = { obj,[{"percent", #constref{name="vFloatPercent"}}]},
	Input6 = { obj,[{"percent",1.021211234}]},
	?assertMatch({failed, _}, evaluate_json(Config, Pattern6, Input6)),

	Pattern={	obj,[{"results",
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

	Input = {	obj,[{"results",
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


	?assertMatch({ok}, evaluate_json(Config, Pattern, Input)).


quickcheck_test_() ->
	{timeout, 6*60, run_quickcheck_example}.

run_quickcheck_example() ->
	% Make sure, using right type. (For example: 190 isn't a float!)
	Vars = [#const{name="vLat", type=float, value=0.000011, def={-180.0, 180.0}},
			#const{name="vLon", type=float, value=0.000022, def={-180.0, 180.0}},
			#var{name="vHours", type=integer, def={0, 24}},
			#var{name="vMinutes", type=integer, def={0, 60}},
			#var{name="vFloat1", type=float, def={-180.0, 180.0}},
			#const{name="vHoursG", type=integer, def={0, 24}},
			#const{name="vMinutesG", type=integer},
			#const{name="vFloatPercentG", type=float, def={0.0, 1.0}} ],

	Requests = [#request{
					name="req1", 
					host="http://maps.googleapis.com", 
					path="/maps/api/geocode/json", 
					params=[{"address", "Berlin,Germany"}, {"sensor", "false"}], 
					method=get},
				#request{
					name="req2", 
					host="http://maps.googleapis.com", 
					path="/maps/api/geocode/json", 
					params=[{"latlng", #constcombo{fmt="~p,~p", names=["vLat", "vLon"]}}, 
						{"sensor", "false"}], 
					method=get}
				],


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
								 				{obj,[{"lat",#varref{name="vFloat1"}},
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
				]},
			{reply,
				"rep2",
				[	{status, 200},
					{json_body, 
						{obj,[{"results",
        					 [{obj,[{"elevation",1608.637939453125},
     				          {"location",{obj,[{"lat",39.7391536},{"lng",-104.9847034}]}},
     				          {"resolution",4.771975994110107}]},
     				    {obj,[{"elevation",-50.78903579711914},
     				          {"location",{obj,[{"lat",36.455556},{"lng",-116.866667}]}},
     				          {"resolution",19.08790397644043}]}]},
     				  {"status",<<"OK">>}]} }]}
			],
			
	Tests = [{test, "test1", "req1", "rep1", 100},
			 {test, "test2", "req2", "rep1", 100},
			 {test, "test3", "req1", "rep2", 100},
			 {test, "test4", "req2", "rep2", 100},
			 {test, "test5", "req1", "rep1", 100},
			 {test, "test6", "req2", "rep2", 100}],

	Config = #resttcfg{placeholder_list=Vars, req_list=Requests, rep_list=Replies, test_list=Tests},
	?assertEqual(true, quickcheck(Config)).


%
% Request for http://maps.googleapis.com/maps/api/geocode/json?address=Berlin,Germany&sensor=false
%
http_request_test() ->
	A = #request{name="sample1",
		host="http://maps.googleapis.com", 
		path="/maps/api/geocode/json", 
		params=[{"address", "Berlin,Germany"}, 
				{"sensor", "false"}], 
		method=get},

	io:format("Request: ~n~p~n", [A]),
	Res = http_request(A),
	io:format("Result: ~n~p~n", [Res]),
	{Rc, _State, _Header, _Body} = Res,
	?assert( Rc == ok).


param_to_str_test() ->
	Tests = [{"?Limit=12", [{"Limit", "12"}]},
			 {"?Q=w&Limit=12", [{"Q", "w"}, {"Limit", "12"}]},
			 {"", []}],
	param_to_str_test(Tests).
param_to_str_test([{Expected, Input} | Rest]) ->
	?assertMatch(Expected, param_to_str(Input)),
	param_to_str_test(Rest);
param_to_str_test([]) ->
	ok.


tool_get_response_as_json_test() ->
	Request1 = #request{
		name="req1", 
		host="http://maps.googleapis.com", 
		path="/maps/api/geocode/json", 
		params=[{"address", "Berlin,Germany"}, {"sensor", "false"}], 
		method=get},
	Request2 = #request{
		name="req2", 
		host="http://maps.googleapis.com", 
		path="/maps/api/geocode/json", 
		params=[{"latlng", "40.714224,-73.961452"}, {"sensor", "false"}], 
		method=get},
	Request3 = #request{
		name="req3", 
		host="http://maps.googleapis.com", 
		path="/maps/api/elevation/json", 
		params=[{"locations", "39.7391536,-104.9847034|36.455556,-116.866667"}, {"sensor", "false"}], 
		method=get},

	io:format("----------------[ ~p ]-------------------~n ~p ~n~n~n", [Request1#request.name, tool_get_response_as_json(Request1, true)]),
	io:format("----------------[ ~p ]-------------------~n ~p ~n~n~n", [Request2#request.name, tool_get_response_as_json(Request2, true)]),
	io:format("----------------[ ~p ]-------------------~n ~p ~n~n~n", [Request3#request.name, tool_get_response_as_json(Request3, true)]).


convert_constcombo_to_string_test() ->
	Vars = [#const{name="vHoursG", type=integer, value=12, def={0, 24}},
			#const{name="vMinutesG", type=integer, value=2},
			#const{name="vFloatPercentG", type=float, value=0.33, def={0.0, 1.0}} ],
	?assertMatch({ok, "12:2"}, convert_constcombo_to_string(Vars, #constcombo{fmt="~p:~p", names=["vHoursG", "vMinutesG"]})).


generate_static_request_test() ->
	Vars = [#const{name="vLat", type=float, value=23.000011, def={-180, 180}},
			#const{name="vLon", type=float, value=5.000022, def={-180, 180}},
			#const{name="test", type=integer, value=666 },
			#const{name="ID", type=integer, value=23 },
			#const{name="vString", type=string, value="ACK" } ],

	Request = #request{
		name="req2", 
		host="http://maps.googleapis.com", 
		path="/maps/api/geocode/json", 
		params=[
			{"latlng", #constcombo{fmt="~p,~p", names=["vLat", "vLon"]}},
			{"latlng2", #constref{name="vLon"}}, 
			{"sensor", "false"}, 
			{#constref{name="test"}, "Inhalt"}], 
		header=[
			{"CLIENT_ID", #constcombo{fmt="~p~p", names=["test", "ID"]}},
			{"TOKEN", #constref{name="test"}}, 
			{"sensor", "false"}, 
			{#constref{name="test"}, "Inhalt"},
			{#constref{name="vString"}, #constref{name="test"}}], 
		method=get,
		body={json_body, 
						{obj,[{"results",
        					 [{obj,[{"elevation", #constref{name="vLat"}},
     				          {"location",{obj,[{"lat",39.7391536},{"lng",-104.9847034}]}},
     				          {#constref{name="test"},4.771975994110107},
     				          {"String.. content", #constref{name="vString"}},
     				          {"Integer. content", #constref{name="test"}},
     				          {"float... content", #constref{name="vLat"}}]},
     				    {obj,[{"elevation",-50.78903579711914},
     				          {#constcombo{fmt="hallo~p", names=["test"]},{obj,[{"lat",36.455556},{"lng",-116.866667}]}},
     				          {"resolution",#constref{name="vLon"}}]}]},
     				  {"status",<<"OK">>}]} }},

    ExpectedStaticRequest = {request,"req2","http://maps.googleapis.com",
         "/maps/api/geocode/json",
         [{"latlng","23.000011,5.000022"},
          {"latlng2","5.000022"},
          {"sensor","false"},
          {"666","Inhalt"}],
         get,
         [{"CLIENT_ID","66623"},
          {"TOKEN","666"},
          {"sensor","false"},
          {"666","Inhalt"},
          {"ACK","666"}],
         {obj,[{"results",
                [{obj,[{"elevation",23.000011},
                       {"location",
                        {obj,[{"lat",39.7391536},{"lng",-104.9847034}]}},
                       {"666",4.771975994110107},
                       {"String.. content","ACK"},
                       {"Integer. content",666},
                       {"float... content",23.000011}]},
                 {obj,[{"elevation",-50.78903579711914},
                       {"hallo666",{obj,[{"lat",36.455556},{"lng",-116.866667}]}},
                       {"resolution",5.000022}]}]},
               {"status",<<"OK">>}]}},

    ?assertMatch(ExpectedStaticRequest, generate_static_request(Vars, Request)).
    
