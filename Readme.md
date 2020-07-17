# httptest

httptest 是一个面向 scala 程序员的 http 测试工具， 类似于 [wrk](https://github.com/wg/wrk)

计划支持功能：
1. 单 URL 压测，支持设定 并发数、总请求数、keep-alive、超时等参数
2. 多 URL 压测，支持权重设置
3. 功能测试，可自定义对比返回结果
4. 使用 scala 作为脚本，可自定义动态参数、自定义结果对比
5. 使用 async http，可支持单机上 50K+ 的QPS 测试能力 

