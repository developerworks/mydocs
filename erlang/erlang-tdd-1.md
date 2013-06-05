Erlang 测试和测试驱动: Erlang TDD 项目实践 WorkerNet 第一部分
====================================

History – Perl, Ericsson and late evenings
历史 - Perl, 爱立信和深夜
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


![Query the subtree nodes](https://raw.github.com/developerworks//master/assets/call%20p_prefix_nodes_get_subtree_by_node_id.png "Query the subtree nodes")

