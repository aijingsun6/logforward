logforward(记录下来)
=====
logforward 是一个erlang高性能的日志框架

### 我为什么要造轮子?
我用过的```erlang```日志框架有2个:```log4erl,lager```他们无论从设计还是实现上都很棒

但是我通过阅读他们的代码，结合自己对erlang的理解（不知道自己的理解是否OK），我觉得他们都存在瓶颈，而且这个瓶颈很明显

他们的主要操作都在一个gen_event进程里面，这个进程的处理事情有点多，每个appender都要***顺序处理***

这个process是一个单点，也是一个热点，容易造成消息堆积，也容易造成reduction很高

虽然lager针对消息堆积做了限制处理，但是会影响其他进程的处理速度，因为在消息数量很高的时候，lager使用的是sync_notify

这种同步处理，甚至还提供了kill该process的功能，容易造成日志丢失。

我心里的理想日志框架，需要做到完全异步处理，相当于操作都是cast操作，对基本业务不会有阻塞操作，而且不希望看到单点

### 特性


### 设计思路
1. 通过sink来串行给appender分发日志，但是appender可以是异步处理的(handle_msg)
2. 如果想要限流，那么可以通过额外的计数器，比如IO的积累量，而不是靠gen_event的消息的累计量

### 日志的格式 
| name | note | example | 
| --- | --- | --- |
| module | 模块名称 |  |
| date | 日期 | yyyy-MM-dd |
| time | 时间 | hh:mm:ss.SSS |
| datetime | 时间 | yyyy-MM-dd hh:mm:ss.SSS |
| msg | 消息体 | |
| eol | 换行符 | "\r\n" |
| line | 行号 | |
| level | 日志等级 | DEBUG,INFO...|
| pid  | 进程名称 | |
| node | 节点 | |
| metadata | 额外信息 | | 
| nmsg | 第N条日志 ||
| nth | 第N | |
```
例子：
%datetime [ %level ] - %msg%eol
格式化后的结果就是这个样子
2019-09-10 12:00:00.000 [INFO] - hello\r\n

```

### 优化点：
- [x] 异步写文件，减少sink的负担
- [x] 格式化函数使用缓存，减少计算次数,毕竟同一个人总期望使用相同格式的日志嘛





