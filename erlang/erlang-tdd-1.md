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
