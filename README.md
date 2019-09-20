logforward(记录下来)
=====
logforward 是一个erlang高性能的日志框架

### 我为什么要造轮子?
我用过的```erlang```日志框架有2个:```log4erl,lager```他们无论从设计还是实现上都很棒

但是我通过阅读他们的代码，结合自己对erlang的理解，我觉得他们都存在瓶颈，而且这个瓶颈很明显

他们的主要操作都在一个gen_event进程里面，这个进程的处理事情有点多，每个appender都要***顺序处理***

这个process是一个单点，也是一个热点，容易造成消息堆积，也容易造成reduction很高

虽然lager针对消息堆积做了限制处理，但是会影响其他进程的处理速度，因为在消息数量很高的时候，lager使用的是sync_notify

这种同步处理，甚至还提供了kill该process的功能，容易造成日志丢失。

我心里的理想日志框架，需要做到完全异步处理，相当于操作都是cast操作，对基本业务不会有阻塞操作，而且不希望看到单点

### 特性
##### 支持 logforward_transform,不过要比lager更简单
```
{parse_transform, logforward_transform}
```

### 设计思路
1. 通过sink来串行给appender分发日志，但是appender可以是异步处理的(handle_msg)
2. 限流，通过sink消息队列数量

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
| function | 函数入口 ||
| function_arity |  函数参数个数 | |

```
例子：
%datetime [ %level ] - %msg%eol
格式化后的结果就是这个样子
2019-09-10 12:00:00.000 [INFO] - hello\r\n
```

### 文件的格式
| name | note | example |
| --- | --- | --- |
| date | 日期 | yyyy-MM-dd |
| nth  | 第N个文件   | |

```
info_%nth.log
那么文件名称就是
info_0.log
info_1.log
...

info_%date.%nth.log
info_2019-09-11.0.log
info_2019-09-11.1.log
info_2019-09-11.2.log
...

```
### sink
```
格式：
{sink, [{dir, "logs"}, {cut_level, info}], [
          {console, logforward_console_appender, [{level, info}]},
          {info, logforward_file_appender, [{level, info}]}
        ]}
        

Sink::{SinkName,SinkOptions,[Appender]}
Appender:: {AppenderName,AppenderModule,AppenderOptions }
SinkName:: atom()
SinkOptions::proplist()
AppenderName::atom()
AppenderModule::atom()
AppenderOptions:: proplist()
```
##### sink 配置项
| name | type | note |
| --- | --- | ---|
| dir | string() | 目录 |
| cut_level | atom() | 限制等级|
| garbage_msg | int | 每处理N条消息，进行一次垃圾回收|
| throttle | int | 消息队列数量达到N,开始限流，知道下次消息队列数量低于这个值 |
| report_msg | int | 每处理N条消息，上报消息队列长度，用于限流,report_msg << throttle  |

##### appender 配置项
| name | type | note |
| --- | --- | --- |
| dir | string() | 目录,可覆盖sink的配置 |
| level | atom() | 限制等级|
| formatter | atom() | 格式化module|
| pattern | string() | 日志格式 |
| file_pattern | string() | 文件日志，文件名格式|
| rotate_type | type | 日志滚动类别：data_size,msg_size,time|
| rotate_size | int | 日志得到多少滚动|
| max | int | 最多保留文件数量 |

### 优化：
- [x] 异步写文件，减少sink的负担
- [x] 格式化函数使用缓存，减少计算次数,毕竟同一个人一般情况下，日志的格式倾向于一致，无论是console,还是file
- [x] 限流，对比lager,采用block方式，达到阀值后，不让向sink发送消息,而lager只是对调用proc堵塞，但是可以新开proc，向sink发送消息
- [x] 垃圾回收，sink在运行一段时间后，内存消耗较大，采用定期垃圾清理




