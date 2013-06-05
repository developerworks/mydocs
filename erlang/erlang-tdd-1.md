Erlang 测试和测试驱动: Erlang TDD 项目实践 WorkerNet 第一部分
====================================

History – Perl, Ericsson and late evenings
---------------------------------------------------------

This series of posts will go through the design, testing and implementation of a distributed Worker-Net (something that I saw/implemented @Ericsson AB in 2009 –
at the time written in a combination of Perl [~6000 lines of Perl!], bash scripts and other exotic animals). Needles to say,
this could have been written in a more suitable language (as Erlang), and so I tried – on evenings, it was a great way of learning Erlang and a great fun.
My wife’s computer served a lot as testing ground. However, nothing is so good it can’t be re-done in a better way, and it suits perfectly well for this series.

WorkerNet – What?
--------------------------

A WorkerNet (WN) is a distributed application across erlang nodes that allows people to send jobs and related files across the network to a resource that can perform the job.
A job can be anything defined by the sender but must be bound to an available resource type.

WorkerNet(WN) 是一个分布式的横跨多个Erlang节点应用程序, 它使人们能够通过网络向一个执行作业的资源发送作业,以及和作业相关的文件.
一个作业可以是发送定义的任何东西,但是必须是一个可用的资源类型.


![WorkerNet – What](https://raw.github.com/developerworks/mydocs/master/erlang/images/explanation12.png "WorkerNet – What")


Resources are defined by each node and published publicly in the network. Each node in the WN must serve as an entry point to the network.
Each node must know about the available pool of resources and the types of resources.
Each resource-type in the network has a queue, and the incoming jobs must be scheduled fairly across the resources, some jobs may have higher priority than others.
Anything capable of running the erts with network interfaces can serve as a node in the WN, the WN is thus scalable and can easily be resized in any direction.

资源由每个节点定义, 并在网络中发布.WN中的每个节点必须充当网络中的一个入口. 每个节点必须知道可用的资源池, 以及资源的类型.
网络中的每个资源类型有一个队列,进入队列的作业必须公平的调度,某些作业具有比其他作业更高的优先级.
任何可运行ERTS和网络接口的节点可作为WN中的一个节点,WN是可伸缩的, 并且在任何方向上都能容易的调整大小.


A layered (modular) architecture is often cleaner and easier to test, so such a one will be chosen here. Each process will then utilize the functionality of a layer through one facade module.

一个分层(模块化)架构通常更加清晰,并且更容易测试. 因此这里选择这样这样的架构. 每个进程通过一个facade模块使用一层的功能.

![WorkerNet – What](https://raw.github.com/developerworks/mydocs/master/erlang/images/node1.png "WorkerNet – What")

Iteration 1  - The design and testing of the Resource Layer
--------------------------------------------------------------------------------

The first iteration will start of with the test based design and implementation of the resource layer.
To share my ideas with you in the blog, I will present the main use cases I want to be able to satisfy

首个迭代将从资源层基于测试的设计和实现开始.为了分享我的思路,我会演示我想要满足的主要用例.

"I want to be able to specify the types of my resources. Resources should be able to be of multiple types."

"我想要能够指定我的资源类型, 资源应该可以有多种类型."

"I want to be able to specify the amount of available resources for each resource type, for the resources that I register."

"对于那些我注册的资源,我想要能够为每种资源类型指定可用资源的数量,"

"I want to be able to dynamically register and de-register my resources from any node."

"我想要能够在任何节点上动态地注册和注销我的资源."

"I want to be able to list the available resources in the resource layer, through any node"

"我想要能够在任何节点上的资源层列出可用的资源."

Keeping this in mind, and also remembering that the resource layer should be a distributed layer; the first test is written

要注意的是, 资源层应该是一个分布式的层; 吃一个测试为:

```erlang
%%% @author Gianfranco <zenon@zen.local>
%%% @copyright (C) 2010, Gianfranco
%%% Created : 10 Dec 2010 by Gianfranco <zenon@zen.local>
-module(wn_resource_layer_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/worker_net.hrl").

register_resource_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [{"Can register resources locally", fun register_locally/0}
        ]}.

register_locally() ->
    ResourceA = #wn_resource{name = "macbook pro laptop",
    type = [{'os-x', 1}, {bsd, 1}],
    resides = node()},
    ResourceB = #wn_resource{name = "erlang runtime system",
    type = [{erlang, 4}],
    resides = node()},
    ok = wn_resource_layer:register(ResourceA),
    ok = wn_resource_layer:register(ResourceB),
    List = lists:sort(wn_resource_layer:list_resources()),
    ?assertMatch([ResourceB, ResourceA], List).

%% -----------------------------------------------------------------
setup() ->
    {ok, _} = net_kernel:start([eunit_resource, shortnames]),
    erlang:set_cookie(node(), eunit),
    {ok, _} = wn_resource_layer:start_link().

cleanup(_) ->
    ok = net_kernel:stop(),
    ok = wn_resource_layer:stop().
```


头文件

```erlang
%%% @author Gianfranco <zenon@zen.local>
%%% @copyright (C) 2010, Gianfranco
%%% Created : 10 Dec 2010 by Gianfranco <zenon@zen.local>

-record(wn_resource,
{name :: string(),
    type :: [{atom(), non_neg_integer() | infinity}],
    resides :: node()
}).
```

尽可能的完善类型声明,这样可以使用dialyzer对代码进行静态分析.接下来花一点时间实现,插入`gen_server`框架代码,在这个基础上修改和添加函数

API

```erlang
%%%===========================================================
%%% API
%%%===========================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, not_used, []).

-spec(register(#wn_resource{}) -> ok | {error, term()}).
register(Resource) ->
    gen_server:call(?MODULE, {register, Resource}).

-spec(list_resources() -> [#wn_resource{}]).
list_resources() ->
    gen_server:call(?MODULE, list_all_resources).

-spec(stop() -> ok).
stop() ->
    gen_server:call(?MODULE, stop).
```


修改的回调函数


```erlang
init(not_used) ->
    {ok, #state{resources = ets:new(resources, [set])}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(list_all_resources, From, State) ->
    spawn_link(resource_collector(From)),
    {noreply, State};

handle_call(list_resources, _From, State) ->
    {reply, [V || {_, V} <- ets:tab2list(State#state.resources)], State};

handle_call({register, Resource}, From, State) ->
    #wn_resource{resides = Node} = Resource,
    case {Node == node(), lists:member(Node, nodes())} of
        {true, _} ->
            Reply = try_register(State, Resource),
            {reply, Reply, State};
        {false, true} ->
            gen_server:cast({?MODULE, Node}, {register, From, Resource}),
            {noreply, State};
        {false, false} ->
            {reply, {error, noresides}, State}
    end.

handle_cast({register, From, Resource}, State) ->
    gen_server:reply(From, try_register(State, Resource)),
    {noreply, State}.
```

添加内部函数到 `wn_resource_layer.erl`

```erlang
%%%===========================================================
%%% Internal functions
%%%===========================================================
try_register(State, Resource) ->
    #wn_resource{name = Name} = Resource,
    case ets:lookup(State#state.resources, Name) of
        [] -> ets:insert(State#state.resources, {Name, Resource}),
            ok;
        _ ->
            {error, already_exists}
    end.

resource_collector(From) ->
    Nodes = [node() | nodes()],
    fun() ->
        Res =
            lists:foldr(
                fun(Node, Acc) ->
                    gen_server:call({?MODULE, Node}, list_resources) ++ Acc
                end, [], Nodes),
        gen_server:reply(From, Res)
    end.
```

现在创建`Makefile`

```sh
all:
     erlc -pa . -o ebin/  src/*.erl test/*.erl

test:  all
     erl -pa ebin/ -eval 'eunit:test(wn_resource_layer), init:stop().'

dialyze:
     dialyzer src/*.erl test/*.erl

full: all test dialyze
```

运行`make full`获得如下结果

```sh
zen:worker_net-0.1 zenon$ make full
erlc -pa . -o ebin/  src/*.erl test/*.erl
erl -pa ebin/ -eval 'eunit:test(wn_resource_layer), init:stop().'
Erlang R14B (erts-5.8.1) [source] [smp:4:4] [rq:4] [async-threads:0]
[hipe] [kernel-poll:false]

Eshell V5.8.1  (abort with ^G)
1>   Test passed.
dialyzer src/*.erl test/*.erl
  Checking whether the PLT /Users/zenon/.dialyzer_plt is up-to-date... yes
  Proceeding with analysis...
Unknown functions:
  eunit:test/1
 done in 0m0.43s
done (passed successfully)
zen:worker_net-0.1 zenon$
```

当然这不是第一此编译(在这之前,我解决了一下错误和问题.)
添加第一个测试来检测是否能够添加和访问远程节点上的资源. 添加并重构测试,添加的测试生成器为:

```erlang
distr_resource_test_() ->
    {foreach,
        fun distr_setup/0,
        fun distr_cleanup/1,
        [fun register_distributed/1
        ]
    }.
```

with an added test istantiator

```erlang
register_distributed([N1, N2]) ->
    {"Can Register Distributed", fun() ->
        rpc:call(N1, wn_resource_layer, start_link, []),
        rpc:call(N2, wn_resource_layer, start_link, []),
        ResourceA = #wn_resource{name = "erlang R14",
        type = [{erlang, infinity}],
        resides = N1},
        ResourceB = #wn_resource{name = "os-x macbook pro",
        type = [{'os-x', 1}],
        resides = N2},
        ResourceC = #wn_resource{name = "g++",
        type = [{'g++', 1}],
        resides = node()},
        ok = wn_resource_layer:register(ResourceA),
        ok = wn_resource_layer:register(ResourceB),
        ok = wn_resource_layer:register(ResourceC),
        ListA = lists:sort(wn_resource_layer:list_resources()),
        ListB = lists:sort(rpc:call(N1, wn_resource_layer, list_resources, [])),
        ListC = lists:sort(rpc:call(N2, wn_resource_layer, list_resources, [])),
        ?assertEqual([ResourceA, ResourceC, ResourceB], ListA),
        ?assertEqual([ResourceA, ResourceC, ResourceB], ListB),
        ?assertEqual([ResourceA, ResourceC, ResourceB], ListC)
    end}.
```


This test passed without problem. Next test will test that the resource layer can be started and restarted with re-registration.
This test starts a layer on slave nodes, registers, resources, accesses them, stops layers, starts layers, registers and finds resources. All in a controlled manner.

测试全部通过,没有任何问题. 接下来测试资源层可以启动/重启.
此测试在slave节点上启动资源层, 注册资源, 访问资源, 停止资源层, 启动资源层, 注册和查找资源.

```erlang
register_restart_register([N1, N2]) ->
    {"Can Register, Restart and Register", fun() ->
        rpc:call(N1, wn_resource_layer, start_link, []),
        rpc:call(N2, wn_resource_layer, start_link, []),
        ResourceA = #wn_resource{name = "erlang R14",
        type = [{erlang, infinity}],
        resides = N1},
        ResourceB = #wn_resource{name = "os-x macbook pro",
        type = [{'os-x', 1}],
        resides = N2},
        ResourceC = #wn_resource{name = "g++",
        type = [{'g++', 1}],
        resides = node()},
        ok = wn_resource_layer:register(ResourceA),
        ok = wn_resource_layer:register(ResourceB),
        ok = wn_resource_layer:register(ResourceC),
        M = fun() -> lists:sort(wn_resource_layer:list_resources()) end,
        S1 = fun() -> lists:sort(rpc:call(N1, wn_resource_layer, list_resources, []))
        end,
        S2 = fun() -> lists:sort(rpc:call(N2, wn_resource_layer, list_resources, []))
        end,
        ?assertEqual([ResourceA, ResourceC, ResourceB], M()),
        ?assertEqual([ResourceA, ResourceC, ResourceB], S1()),
        ?assertEqual([ResourceA, ResourceC, ResourceB], S2()),
        rpc:call(N1, wn_resource_layer, stop, []),
        ?assertEqual([ResourceC, ResourceB], M()),
        ?assertEqual([ResourceC, ResourceB], S2()),
        rpc:call(N2, wn_resource_layer, stop, []),
        ?assertEqual([ResourceC], M()),
        {ok, _} = rpc:call(N1, wn_resource_layer, start_link, []),
        {ok, _} = rpc:call(N2, wn_resource_layer, start_link, []),
        ok = wn_resource_layer:register(ResourceA),
        ?assertEqual([ResourceA, ResourceC], M()),
        ok = wn_resource_layer:register(ResourceB),
        ?assertEqual([ResourceA, ResourceC, ResourceB], M()),
        ?assertEqual([ResourceA, ResourceC, ResourceB], S1()),
        ?assertEqual([ResourceA, ResourceC, ResourceB], S2())
    end}.
```


After having written the test and tried it with ‘make full’,
It became evident that one flaw of the current implementation was that (should be for whoever is trying this out themselves)
 is that the resource layer treats ALL seen nodes as having a resource layer running,
this is not a healthy assumption not be the case and we need a fix to prevent the gen_server:call/2 in case there is no running wn_resource_layer gen_server running.

在测试编写完成并执行`make full`后,

```erlang
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
```

The fix seen above in `wn_resource_layer.erl` was to add the case-of with `rpc:call/4` `erlang:whereis(?MODULE)`.
Fixed and running, the `make full` reports. What is now left to fulfill the initial `requirements` is a test that proves the ability to deregister resources dynamically through any node.
Test first as always.