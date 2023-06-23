-module(erl_oracle@foreign).

-export([ runCommand/1
        , base64Decode/1
        ]).

runCommand(Cmd) ->
    fun() ->
            case exec:run(binary_to_list(Cmd), [stdout, {stderr, stdout}, sync]) of
                {ok, Output} ->
                    Stdout = get_value(stdout, <<"">>, Output),
                    {right, iolist_to_binary(Stdout)};

                {error, Output} ->
                    ExitStatus = get_value(exit_status, <<"">>, Output),
                    Stdout = get_value(stdout, <<"">>, Output),
                    {left, #{exitStatus => ExitStatus, output => iolist_to_binary(Stdout)}}
            end
    end.

get_value(Name, Default, List) ->
    case lists:keyfind(Name, 1, List) of
        false -> Default;
        {Name, Value} -> Value
    end.

base64Decode(Encoded) ->
    try
        {just, base64:decode(Encoded)}
    catch
        _:_:_ ->
            {nothing}
    end.
