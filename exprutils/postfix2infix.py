import regex as re
from math import ceil, log2
from ._sorting_networks import OPTIMAL_SORTING_NETWORKS

OPERATORS = {
    "+": (2, "({0} + {1})"),
    "-": (2, "({0} - {1})"),
    "*": (2, "({0} * {1})"),
    "/": (2, "({0} / {1})"),
    "%": (2, "fmodf({0}, {1})"),
    "pow": (2, "powf({0}, {1})"),
    "**": (2, "powf({0}, {1})"),
    "sqrt": (1, "sqrtf({0})"),
    ">": (2, "({0} > {1})"),
    "<": (2, "({0} < {1})"),
    "=": (2, "({0} == {1})"),
    ">=": (2, "({0} >= {1})"),
    "<=": (2, "({0} <= {1})"),
    "and": (2, "(({0} > 0.0f) && ({1} > 0.0f))"),
    "or": (2, "(({0} > 0.0f) || ({1} > 0.0f))"),
    "xor": (2, "((!!({0} > 0.0f)) ^ (!!({1} > 0.0f)))"),
    "not": (1, "(!({0} > 0.0f))"),
    "exp": (1, "expf({0})"),
    "exp2": (1, "exp2f({0})"),
    "log": (1, "logf({0})"),
    "log2": (1, "log2f({0})"),
    "log10": (1, "log10f({0})"),
    "abs": (1, "fabsf({0})"),
    "sin": (1, "sinf({0})"),
    "cos": (1, "cosf({0})"),
    "tan": (1, "tanf({0})"),
    "asin": (1, "asinf({0})"),
    "acos": (1, "acosf({0})"),
    "atan": (1, "atanf({0})"),
    "sinh": (1, "sinhf({0})"),
    "cosh": (1, "coshf({0})"),
    "tanh": (1, "tanhf({0})"),
    "floor": (1, "floorf({0})"),
    "ceil": (1, "ceilf({0})"),
    "round": (1, "roundf({0})"),
    "trunc": (1, "truncf({0})"),
    "atan2": (2, "atan2f({0}, {1})"),
    "copysign": (2, "copysignf({0}, {1})"),
    "min": (2, "fminf({0}, {1})"),
    "max": (2, "fmaxf({0}, {1})"),
    "fma": (3, "fmaf({0}, {1}, {2})"),
    "?": (3, "(({0} != 0.0f) ? {1} : {2})"),
    "clip": (3, "fminf(fmaxf({0}, {1}), {2})"),
    "clamp": (3, "fminf(fmaxf({0}, {1}), {2})"),
    "bitand": (2, "((int){0} & (int){1})"),
    "bitor": (2, "((int){0} | (int){1})"),
    "bitxor": (2, "((int){0} ^ (int){1})"),
    "bitnot": (1, "(~(int){0})"),
}

RE_REL_BRACKET = re.compile(r"^(?:src(\d+)|([x-za-w]))\[(-?\d+),(-?\d+)\](?::([cm]))?$")
RE_ABS = re.compile(r"^(?:src(\d+)|([x-za-w]))\[\]$")
RE_CUR = re.compile(r"^(?:src(\d+)|([x-za-w]))$")
RE_PROP = re.compile(r"^(?:src(\d+)|([x-za-w]))\.([a-zA-Z_][a-zA-Z0-9_]*)$")


def get_clip_name(match):
    if match.group(1):
        return f"src{match.group(1)}"
    if match.group(2):
        return match.group(2)
    raise ValueError("Invalid clip match")


def tokenize(expr: str):
    return expr.split()


class RPNError(Exception):
    pass


def _oem_merge_pairs(pairs, lo, n, r):
    m = r * 2
    if m < n:
        _oem_merge_pairs(pairs, lo, n, m)
        _oem_merge_pairs(pairs, lo + r, n, m)
        for i in range(lo + r, lo + n - r, m):
            pairs.append((i, i + r))
    else:
        pairs.append((lo, lo + r))


def _generate_oem_sort_pairs(pairs, lo, n):
    if n > 1:
        m = n // 2
        _generate_oem_sort_pairs(pairs, lo, m)
        _generate_oem_sort_pairs(pairs, lo + m, n - m)
        _oem_merge_pairs(pairs, lo, n, 1)


def get_sorting_network(n):
    if n in OPTIMAL_SORTING_NETWORKS:
        return OPTIMAL_SORTING_NETWORKS[n]
    pairs = []
    p = 1 << ceil(log2(n))
    _generate_oem_sort_pairs(pairs, 0, p)
    # Filter out pairs with indices >= n
    return [pair for pair in pairs if pair[1] < n]


def postfix2infix(expr: str):
    """
    Converts an RPN expression string to a C-style infix string.
    Inspired by mvsfunc.postfix2infix

    Args:
        expr: The RPN expression string.

    Returns:
        A string containing the equivalent C-style code.

    Raises:
        RPNError: If the expression is invalid.
    """
    tokens = tokenize(expr)
    stack = []
    has_control_flow = False
    sort_var_counter = 0

    all_vars = {
        token[:-1]
        for token in tokens
        if (token.endswith("!") or token.endswith("@")) and token[:-1]
    }

    statements = [f"float {var} = 0.0f;" for var in sorted(list(all_vars))]
    if statements:
        statements.append("")

    for i, token in enumerate(tokens):
        if token in OPERATORS:
            arity, fmt = OPERATORS[token]
            if len(stack) < arity:
                raise RPNError(
                    f"Stack underflow for operator '{token}' at token {i}. Need {arity}, have {len(stack)}."
                )

            operands = stack[-arity:]
            stack = stack[:-arity]

            new_expr = fmt.format(*operands)
            stack.append(new_expr)
            continue

        try:
            int_val = int(token, 0)
            stack.append(f"{int_val}.0f")
            continue
        except ValueError:
            try:
                float(token)
                stack.append(f"{token}f")
                continue
            except ValueError:
                pass

        if token in ["X", "Y", "N", "width", "height", "pi"]:
            stack.append(token)
            continue

        if token.startswith("dup"):
            n = 0 if token == "dup" else int(token[3:])
            if len(stack) <= n:
                raise RPNError(f"Stack underflow for '{token}' at token {i}")
            stack.append(stack[-1 - n])
            continue

        if token.startswith("swap"):
            n = 1 if token == "swap" else int(token[4:])
            if n == 0:
                continue
            if len(stack) <= n:
                raise RPNError(f"Stack underflow for '{token}' at token {i}")
            stack[-1], stack[-1 - n] = stack[-1 - n], stack[-1]
            continue

        if token.startswith("drop"):
            n = 1 if token == "drop" else int(token[4:])
            if len(stack) < n:
                raise RPNError(f"Stack underflow for '{token}' at token {i}")
            stack = stack[:-n]
            continue

        if token.startswith("sort"):
            try:
                n = int(token[4:])
            except (ValueError, IndexError):
                raise RPNError(f"Invalid sort operator: '{token}' at token {i}")

            if n < 2:
                continue

            if len(stack) < n:
                raise RPNError(f"Stack underflow for '{token}' at token {i}")

            has_control_flow = True
            operands = stack[-n:]
            stack = stack[:-n]

            temp_vars = [f"_sort_tmp_{sort_var_counter + j}" for j in range(n)]
            sort_var_counter += n

            for j in range(n):
                statements.append(f"float {temp_vars[j]} = {operands[j]};")

            network = get_sorting_network(n)

            if network:
                statements.append("float _swap_tmp;")
                for u, v in network:
                    statements.append(
                        f"if ({temp_vars[u]} > {temp_vars[v]}) {{ "
                        f"_swap_tmp = {temp_vars[u]}; "
                        f"{temp_vars[u]} = {temp_vars[v]}; "
                        f"{temp_vars[v]} = _swap_tmp; "
                        f"}}"
                    )

            for var in temp_vars:
                stack.append(var)

            continue

        if token.endswith("!"):
            var_name = token[:-1]
            if not var_name:
                raise RPNError(f"Empty variable name at token {i}.")
            if not stack:
                raise RPNError(f"Stack underflow for store '!' at token {i}.")
            value = stack.pop()
            statements.append(f"{var_name} = {value};")
            has_control_flow = True
            continue

        if token.endswith("@"):
            var_name = token[:-1]
            if not var_name:
                raise RPNError(f"Empty variable name at token {i}.")
            stack.append(var_name)
            continue

        if token.startswith("#"):
            label_name = token[1:]
            if not label_name:
                raise RPNError(f"Empty label name at token {i}.")
            statements.append(f"{label_name}:")
            has_control_flow = True
            continue

        if token.endswith("#"):
            label_name = token[:-1]
            if not label_name:
                raise RPNError(f"Empty label name at token {i}.")
            if not stack:
                raise RPNError(f"Stack underflow for jump '#' at token {i}.")
            condition = stack.pop()
            statements.append(f"if (({condition}) > 0.0f) goto {label_name};")
            has_control_flow = True
            continue

        match = RE_REL_BRACKET.match(token)
        if match:
            clip = get_clip_name(match)
            rel_x, rel_y = match.group(3), match.group(4)
            mode_str = f', "{match.group(5)}"' if match.group(5) else ""
            stack.append(f"{clip}.rel({rel_x}, {rel_y}{mode_str})")
            continue

        match = RE_ABS.match(token)
        if match:
            if len(stack) < 2:
                raise RPNError(
                    f"Stack underflow for absolute access '{token}' at token {i}."
                )
            clip = get_clip_name(match)
            abs_y = stack.pop()
            abs_x = stack.pop()
            stack.append(f"{clip}.abs({abs_x}, {abs_y})")
            continue

        match = RE_PROP.match(token)
        if match:
            clip = get_clip_name(match)
            prop = match.group(3)
            stack.append(f'{clip}.prop("{prop}")')
            continue

        match = RE_CUR.match(token)
        if match:
            stack.append(token)
            continue

        raise RPNError(f"Invalid or unknown token: '{token}' at position {i}.")

    if has_control_flow:
        if len(stack) == 1:
            statements.append(f"return {stack.pop()};")
        elif len(stack) > 1:
            raise RPNError(
                f"Stack has {len(stack)} items at the end, expected 1 for the return value. Leftovers: {stack}"
            )
        elif len(stack) == 0 and not any(
            s.strip().startswith("return") for s in statements
        ):
            raise RPNError(
                "Stack is empty at the end of processing and no return statement was generated."
            )
        if stack and not any(s.strip().startswith("return") for s in statements):
            raise RPNError(f"Stack not empty at end of processing. Leftovers: {stack}")
        return "\n".join(statements)
    else:
        if len(stack) != 1:
            raise RPNError(
                f"Stack must have exactly 1 value at the end, but has {len(stack)}. Leftovers: {stack}"
            )
        return f"return {stack.pop()};"
