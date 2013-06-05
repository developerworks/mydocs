resource_collector(From) ->
    Nodes = [node() | nodes()],
    fun() ->
        Res =
            lists:foldr(
                fun(Node, Acc) ->
                    case rpc:call(Node, erlang, whereis, [?MODULE]) of
                        undefined -> Acc;
                        _Pid ->
                            gen_server:call({?MODULE, Node}, list_resources) ++ Acc
                    end
                end, [], Nodes),
        gen_server:reply(From, Res)
    end.