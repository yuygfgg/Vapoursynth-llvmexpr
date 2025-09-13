# 递归函数和函数内控制流实现文档

## 概述

本文档描述了在 `infix2postfix.py` 中实现的递归函数支持和函数内控制流功能。实现基于调用栈模拟系统，将函数调用从"宏展开/内联"模式转变为真正的"基于栈的调用"模型。

## 主要特性

### 1. 函数递归支持
- 支持任意深度的递归调用（最大深度：32层）
- 基于调用栈模拟系统实现
- 自动生成唯一的返回地址ID

### 2. 函数内控制流
- 支持函数内的 `if/else` 语句
- 支持函数内的 `goto` 和标签
- 作用域验证：禁止跨函数 goto

### 3. 调用栈模拟系统
- 使用 RPN 变量模拟调用栈
- 自动生成 push/pop 例程
- 返回分派器处理函数返回

## 实现细节

### 调用栈变量
- `__internal_cs_ptr`: 调用栈指针
- `__internal_cs_0`, `__internal_cs_1`, ...: 存储返回地址的栈帧
- 最大递归深度：32层

### 函数调用协议

#### 调用者 (Caller)
1. 计算所有参数，压入主 RPN 堆栈
2. 生成唯一的返回地址ID
3. 调用 `__internal_push_routine` 压入返回地址
4. 无条件跳转到目标函数
5. 在跳转后放置返回标签

#### 被调用者 (Callee) - 函数入口
1. 从主 RPN 堆栈弹出参数到局部变量
2. 执行函数体逻辑

#### 被调用者 (Callee) - 函数返回
1. 计算返回值，留在主 RPN 堆栈顶部
2. 调用 `__internal_pop_routine` 弹出返回地址
3. 跳转到 `__internal_return_dispatcher`

#### 返回分派器
1. 检查从调用栈弹出的返回地址ID
2. 根据ID跳转到对应的返回标签

### 作用域验证

#### 函数内标签验证
- 函数内定义的标签只能在函数内使用
- 禁止跨函数 goto
- 禁止全局 goto 到函数内标签

#### 标签作用域规则
```python
# 正确：函数内标签和goto
function my_func() {
    start:
    if (condition) {
        goto start
    }
    return 1
}

# 错误：跨函数goto
function func1() {
    label1:
    return 1
}

function func2() {
    goto label1  # 错误：跨函数goto
}
```

## 代码结构

### 新增函数

1. `generate_function_call()` - 生成函数调用RPN代码
2. `generate_function_definition()` - 生成函数定义RPN代码
3. `process_function_body()` - 处理函数体
4. `generate_call_stack_code()` - 生成调用栈模拟代码

### 修改的函数

1. `infix2postfix()` - 主函数，添加函数定义生成
2. `_process_code_block()` - 添加函数作用域支持
3. `convert_expr()` - 重写函数调用处理逻辑
4. `replace_function()` - 更新函数元数据存储

### 全局状态

- `next_return_id`: 返回地址ID计数器
- `MAX_RECURSION_DEPTH`: 最大递归深度常量

## 示例

### 递归阶乘函数
```python
function factorial(n) {
    if (n <= 1) {
        return 1
    } else {
        return n * factorial(n - 1)
    }
}

RESULT = factorial(5)
```

### 函数内控制流
```python
function countdown(x) {
    start:
    if (x > 0) {
        x = x - 1
        goto start
    }
    return x
}

RESULT = countdown(3)
```

## 生成的RPN代码结构

### 函数调用
```
arg1 arg2 ... argN  # 参数（逆序）
{return_id} __internal_push_routine#  # 压入返回地址
1 #function_{func_name}#  # 跳转到函数
#__ret_{return_id}  # 返回点
```

### 函数定义
```
#function_{func_name}  # 函数标签
paramN! paramN-1! ... param1!  # 参数存储（逆序）
...  # 函数体RPN代码
__internal_pop_routine#  # 弹出返回地址
1 #__internal_return_dispatcher#  # 跳转到返回分派器
```

### 调用栈模拟
```
#__internal_push_routine
__internal_cs_ptr@ 0 = #push_0#
...
#push_0
__internal_cs_0! __internal_cs_ptr@ 1 + __internal_cs_ptr! 1 #push_end#
...

#__internal_pop_routine
__internal_cs_ptr@ 0 = #pop_0#
...
#pop_0
__internal_cs_0@ __internal_cs_ptr@ 1 - __internal_cs_ptr! 1 #pop_end#
...

#__internal_return_dispatcher
dup 1 = #__ret_1#
dup 2 = #__ret_2#
...
```

## 测试

运行 `python3 simple_test.py` 来验证实现的结构完整性。

## 注意事项

1. 递归深度限制为32层，超出会触发栈溢出错误
2. 函数内标签作用域严格限制在函数内
3. 调用栈模拟增加了RPN代码的复杂性
4. 需要确保 `llvmexpr.cpp` 的 `validate_and_build_cfg` 能正确处理复杂的控制流图