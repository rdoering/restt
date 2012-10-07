-module(test).

-export([test/0]).


test() ->
    ServerResult={  obj,[{"results",
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
    ExpectedResult={    obj,[{"results",
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
    evaluate_json(ExpectedResult).

evaluate_json([]) ->
    ok;
evaluate_json({obj, RepContent}) ->
    io:format("Object:~p~n",[RepContent]),
    evaluate_json(RepContent);
evaluate_json({Key, Value}) ->
    io:format("Key:~p  Value:~p~n",[Key, Value]),
    evaluate_json(Value);
evaluate_json([KeyValue | Rest]) ->
    io:format("Listelem:~p~n",[KeyValue]),
    case evaluate_json(KeyValue) of
        ok -> evaluate_json(Rest);
        _Else -> failed
    end;
evaluate_json(Value) ->
    io:format("Single Value:~p~n",[Value]),
	ok.