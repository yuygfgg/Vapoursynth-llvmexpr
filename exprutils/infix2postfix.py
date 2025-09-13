from .utils import (
    get_op_arity,
    get_stack_effect,
    is_clip_postfix,
    is_constant_postfix,
    tokenize_expr,
    is_clip_infix,
    is_constant_infix,
    is_token_numeric,
)
import regex as re
from functools import lru_cache
from typing import Optional
import sys
from enum import StrEnum

sys.setrecursionlimit(5000)


class GlobalMode(StrEnum):
    """
    Global mode for a function.

    Attributes:
        ALL: All global variables are available.
        NONE: No global variables are available. (default)
        SPECIFIC: Only the global variables declared are available.
    """

    ALL = "all"
    NONE = "none"
    SPECIFIC = "specific"


def infix2postfix(infix_code: str) -> str:
    R"""
    Convert infix expressions to postfix expressions.

    Args:
        infix_code: Input infix code.

    Returns:
        Converted postfix expr.

    Raises:
        SyntaxError: If infix code failed to convert to postfix expr.

    """

    # Remove comments
    infix_code = "\n".join(
        line.split("#", 1)[0].rstrip() for line in infix_code.split("\n")
    )

    # Check for duplicate function definitions.
    find_duplicate_functions(infix_code)

    # Process global declarations.
    # global declaration syntax: <global<var1><var2>...> or <global.all> or <global.none>
    # Also record for functions if global declaration appears immediately before function definition.
    declared_globals: dict[str, int] = (
        {}
    )  # mapping: global variable -> declaration line number
    global_vars_for_functions: dict[str, set[str]] = {}
    global_mode_for_functions: dict[str, GlobalMode] = {}
    lines = infix_code.split("\n")
    modified_lines: list[str] = []

    # Build a mapping from line number to function name for function declarations.
    function_lines = {}
    for i, line in enumerate(lines):
        func_match = _FUNC_PATTERN.match(line.strip())
        if func_match:
            func_name = func_match.group(1)
            function_lines[i] = func_name

    i = 0
    while i < len(lines):
        line = lines[i].strip()

        global_decl_match = _GLOBAL_DECL_PATTERN.match(line)
        if global_decl_match:
            content = global_decl_match.group(1).strip()
            # Extract all global variable names from this line.
            j = i + 1
            # If the next non-global line is a function definition, apply these globals for that function.
            if j < len(lines):
                function_match = _FUNCTION_DEF_PATTERN.match(lines[j])
                if function_match:
                    func_name = function_match.group(1)
                    args = function_match.group(2)
                    if content == ".all":
                        global_mode_for_functions[func_name] = GlobalMode.ALL
                    elif content == ".none":
                        global_mode_for_functions[func_name] = GlobalMode.NONE
                    else:
                        global_mode_for_functions[func_name] = GlobalMode.SPECIFIC
                        globals_list = _GLOBAL_MATCH_PATTERN.findall(content)

                        if not globals_list and content:
                            raise SyntaxError(
                                f"Invalid global declaration syntax: <global{content}>",
                                i + 1,
                            )
                        for gv in globals_list:
                            if gv not in declared_globals:
                                declared_globals[gv] = (
                                    i + 1
                                )  # store declaration line number (1-indexed)
                        if func_name not in global_vars_for_functions:
                            global_vars_for_functions[func_name] = set()
                        global_vars_for_functions[func_name].update(globals_list)
                        function_params = parse_args(args)
                        if any(
                            global_var in function_params for global_var in globals_list
                        ):
                            raise SyntaxError(
                                "Function parameters must not duplicate with global declarations.",
                                j,
                            )
                else:
                    raise SyntaxError(
                        "Global declaration must be followed by a function definition.",
                        j,
                    )
            else:
                raise SyntaxError(
                    "Global declaration must not be at the last line of code.",
                    j,
                )
            # Replace the global declaration lines with empty line.
            modified_lines.append("\n")
            i += 1
        else:
            modified_lines.append(lines[i])
            i += 1

    expanded_code = "\n".join(modified_lines)

    functions: dict[str, tuple[list[str], str, int, set[str], bool, set[str]]] = {}

    def replace_function(match: re.Match[str]) -> str:
        func_name = match.group(1)
        params_str = match.group(2)
        body = match.group(3)
        func_start_index = match.start()
        line_num = 1 + expanded_code[:func_start_index].count("\n")
        params = [p.strip() for p in params_str.split(",") if p.strip()]

        seen_params: dict[str, int] = {}
        for i, p in enumerate(params):
            if p in seen_params:
                first_i = seen_params[p]
                raise SyntaxError(
                    f"Duplicate parameter name '{p}' at position {i + 1}. It was first declared at position {first_i + 1}.",
                    line_num,
                )
            seen_params[p] = i

        if is_builtin_function(func_name):
            raise SyntaxError(
                f"Function name '{func_name}' conflicts with built-in functions!",
                line_num,
            )
        for param in params:
            if param.startswith("__internal_"):
                raise SyntaxError(
                    f"Parameter name '{param}' cannot start with '__internal_' (reserved prefix)",
                    line_num,
                )
        # Get globals declared for this function if any.
        global_vars = global_vars_for_functions.get(func_name, set())
        body_lines_strip = [line.strip() for line in body.split("\n") if line.strip()]
        has_return = any(line.startswith("return") for line in body_lines_strip)

        # Allow control flow at function scope: if/else/goto/labels
        # We will track function-local labels for proper scoping
        function_local_labels = set()
        for offset, line_text in enumerate(body.split("\n")):
            stripped = line_text.strip()
            if not stripped:
                continue
            # Track function-local labels
            label_match = re.match(r"^\s*([a-zA-Z_]\w*)\s*:", stripped)
            if label_match:
                label_name = label_match.group(1)
                function_local_labels.add(label_name)

        functions[func_name] = (params, body, line_num, global_vars, has_return, function_local_labels)
        return "\n" * match.group(0).count("\n")

    cleaned_code = _FUNCTION_PATTERN.sub(replace_function, expanded_code)

    # Record global assignments with line numbers.
    global_assignments: dict[str, int] = {}
    # current_globals holds the set of global variables defined so far (by assignment).
    current_globals: set[str] = set()
    postfix_tokens: list[str] = []
    label_counter = [0]

    # Global label and goto references to allow cross-block jumps
    all_labels: set[str] = set()
    all_goto_refs: list[tuple[str, int]] = []
    
    # Call stack simulation state
    next_return_id = 1
    MAX_RECURSION_DEPTH = 32

    def _process_code_block(code_block: str, line_offset: int, function_local_labels: set[str] = None) -> list[str]:
        tokens: list[str] = []
        remaining_code = code_block.lstrip()
        current_line = line_offset

        while remaining_code:
            start_len = len(remaining_code)

            label_match = _LABEL_PATTERN.match(remaining_code)
            if_goto_match = _IF_GOTO_PATTERN.match(remaining_code)
            goto_match = _GOTO_PATTERN.match(remaining_code)
            if_match = _IF_PATTERN.match(remaining_code)

            if label_match:
                label_name = label_match.group(1)
                all_labels.add(label_name)
                tokens.append(f"#{label_name}")
                remaining_code = remaining_code[label_match.end() :]
            elif if_goto_match:
                condition, label_name = if_goto_match.groups()
                line_num = current_line + remaining_code[
                    : if_goto_match.start(1)
                ].count("\n")
                # Validate goto target based on scope
                if function_local_labels is not None:
                    # Inside function - only allow function-local labels
                    if label_name not in function_local_labels:
                        raise SyntaxError(
                            f"goto target '{label_name}' is not defined in function scope.",
                            line_num,
                        )
                else:
                    # Global scope - allow global labels
                    all_goto_refs.append((label_name, line_num))
                cond_postfix = convert_expr(
                    condition,
                    current_globals,
                    functions,
                    line_num,
                    global_mode_for_functions,
                )
                tokens.append(f"{cond_postfix} {label_name}#")
                remaining_code = remaining_code[if_goto_match.end() :]
            elif goto_match:
                label_name = goto_match.group(1)
                # Validate goto target based on scope
                if function_local_labels is not None:
                    # Inside function - only allow function-local labels
                    if label_name not in function_local_labels:
                        raise SyntaxError(
                            f"goto target '{label_name}' is not defined in function scope.",
                            current_line,
                        )
                else:
                    # Global scope - allow global labels
                    all_goto_refs.append((label_name, current_line))
                tokens.append(f"1 {label_name}#")
                remaining_code = remaining_code[goto_match.end() :]
            elif if_match:
                label_counter[0] += 1
                current_label_id = label_counter[0]

                condition, if_body = if_match.groups()

                line_num = current_line + remaining_code[: if_match.start(1)].count(
                    "\n"
                )

                cond_postfix = convert_expr(
                    condition,
                    current_globals,
                    functions,
                    line_num,
                    global_mode_for_functions,
                )

                remaining_code = remaining_code[if_match.end() :]

                else_label = f"__internal_else_{current_label_id}"
                endif_label = f"__internal_endif_{current_label_id}"

                else_match = _ELSE_PATTERN.match(remaining_code)
                if else_match:
                    else_body = else_match.group(1)
                    remaining_code = remaining_code[else_match.end() :]

                    # if-else block
                    tokens.append(f"{cond_postfix} not {else_label}#")
                    tokens.extend(
                        _process_code_block(if_body, line_num + condition.count("\n"), function_local_labels)
                    )
                    tokens.append(f"1 {endif_label}#")
                    tokens.append(f"#{else_label}")
                    tokens.extend(
                        _process_code_block(else_body, line_num + if_body.count("\n"), function_local_labels)
                    )
                    tokens.append(f"#{endif_label}")
                else:
                    # if-only block
                    tokens.append(f"{cond_postfix} not {endif_label}#")
                    tokens.extend(
                        _process_code_block(if_body, line_num + condition.count("\n"), function_local_labels)
                    )
                    tokens.append(f"#{endif_label}")

            else:
                # Simple statement (assignment or expression)
                try:
                    stmt, remaining_code = remaining_code.split("\n", 1)
                except ValueError:
                    stmt, remaining_code = remaining_code, ""

                stmt = stmt.strip()
                if not stmt:
                    current_line += 1
                    continue

                line_num = current_line

                if _ASSIGN_PATTERN.search(stmt):
                    var_name, expr = stmt.split("=", 1)
                    var_name = var_name.strip()
                    if var_name.startswith("__internal_"):
                        raise SyntaxError(
                            f"Variable name '{var_name}' cannot start with '__internal_' (reserved prefix)",
                            line_num,
                        )
                    if is_constant_infix(var_name):
                        raise SyntaxError(
                            f"Cannot assign to constant '{var_name}'.", line_num
                        )
                    expr = expr.strip()

                    m_call = _M_CALL_PATTERN.match(expr)
                    if m_call:
                        func_name = m_call.group(1)
                        if func_name in functions:
                            if not functions[func_name][4]:
                                raise SyntaxError(
                                    f"Function '{func_name}' does not return a value and cannot be used in an assignment.",
                                    line_num,
                                )
                            func_global_mode = global_mode_for_functions.get(
                                func_name, GlobalMode.NONE
                            )
                            if func_global_mode == GlobalMode.SPECIFIC:
                                for gv in functions[func_name][3]:
                                    if gv not in current_globals:
                                        raise SyntaxError(
                                            f"Global variable '{gv}' used in function '{func_name}' is not defined before its first call.",
                                            line_num,
                                            func_name,
                                        )

                    if var_name not in current_globals and re.search(
                        r"(?<!\$)\b" + re.escape(var_name) + r"\b", expr
                    ):
                        raise SyntaxError(
                            f"Variable '{var_name}' used before definition", line_num
                        )

                    if var_name not in global_assignments:
                        global_assignments[var_name] = line_num
                    current_globals.add(var_name)

                    postfix_expr = convert_expr(
                        expr,
                        current_globals,
                        functions,
                        line_num,
                        global_mode_for_functions,
                    )
                    full_postfix_expr = f"{postfix_expr} {var_name}!"
                    if compute_stack_effect(full_postfix_expr, line_num) != 0:
                        raise SyntaxError(
                            "Assignment statement has unbalanced stack.", line_num
                        )
                    tokens.append(full_postfix_expr)
                else:
                    m_call = _M_CALL_PATTERN.match(stmt)
                    if m_call:
                        func_name = m_call.group(1)
                        if func_name in functions:
                            func_global_mode = global_mode_for_functions.get(
                                func_name, GlobalMode.NONE
                            )
                            if func_global_mode == GlobalMode.SPECIFIC:
                                for gv in functions[func_name][3]:
                                    if gv not in current_globals:
                                        raise SyntaxError(
                                            f"Global variable '{gv}' used in function '{func_name}' is not defined before its first call.",
                                            line_num,
                                            func_name,
                                        )

                    postfix_expr = convert_expr(
                        stmt,
                        current_globals,
                        functions,
                        line_num,
                        global_mode_for_functions,
                    )
                    if compute_stack_effect(postfix_expr, line_num) != 0:
                        raise SyntaxError(
                            "Expression statement has unbalanced stack. Maybe you forgot to assign it to a variable?",
                            line_num,
                        )
                    tokens.append(postfix_expr)

            consumed_chars = start_len - len(remaining_code)
            consumed_lines = code_block[:consumed_chars].count("\n")
            current_line += consumed_lines
            remaining_code = remaining_code.lstrip(" \t")
            if remaining_code.startswith("\n"):
                current_line += 1
                remaining_code = remaining_code.lstrip()

        return tokens

    postfix_tokens = _process_code_block(cleaned_code, 1, None)

    # Validate that all goto targets refer to a defined label anywhere in global scope
    for target_label, ln in all_goto_refs:
        if target_label not in all_labels:
            raise SyntaxError(
                f"goto target '{target_label}' is undefined.",
                ln,
            )

    # Check that all declared global variables are defined.
    for gv, decl_line in declared_globals.items():
        if gv not in global_assignments:
            raise SyntaxError(
                f"Global variable '{gv}' declared but not defined.", decl_line
            )

    final_result = " ".join(postfix_tokens)
    final_result = final_result.replace("(", "").replace(")", "")
    if "RESULT!" not in final_result:
        raise SyntaxError("Final result must be assigned to variable 'RESULT'!")

    ret = final_result + " RESULT@"

    # Add function definitions
    function_definitions = []
    for func_name, (params, body, func_line_num, global_vars, has_return, function_local_labels) in functions.items():
        func_def = generate_function_definition(
            func_name, params, body, func_line_num, global_vars,
            has_return, function_local_labels, current_globals, functions,
            global_mode_for_functions
        )
        function_definitions.append(func_def)
    
    # Add call stack simulation code
    call_stack_code = generate_call_stack_code()
    
    # Combine all parts
    all_parts = [ret] + function_definitions + [call_stack_code]
    ret = " ".join(all_parts)

    return ret


def generate_call_stack_code() -> str:
    """
    Generate RPN code for call stack simulation system.
    """
    tokens = []
    
    # Initialize call stack pointer
    tokens.append("0 __internal_cs_ptr!")
    
    # Generate push routine
    tokens.append("#__internal_push_routine")
    for i in range(MAX_RECURSION_DEPTH):
        tokens.append(f"__internal_cs_ptr@ {i} = #push_{i}#")
    tokens.append("#push_overflow")  # Stack overflow error
    tokens.append("#push_0")
    tokens.append("__internal_cs_0! __internal_cs_ptr@ 1 + __internal_cs_ptr! 1 #push_end#")
    for i in range(1, MAX_RECURSION_DEPTH):
        tokens.append(f"#push_{i}")
        tokens.append(f"__internal_cs_{i}! __internal_cs_ptr@ 1 + __internal_cs_ptr! 1 #push_end#")
    tokens.append("#push_end")
    
    # Generate pop routine
    tokens.append("#__internal_pop_routine")
    for i in range(MAX_RECURSION_DEPTH):
        tokens.append(f"__internal_cs_ptr@ {i} = #pop_{i}#")
    tokens.append("#pop_underflow")  # Stack underflow error
    tokens.append("#pop_0")
    tokens.append("__internal_cs_0@ __internal_cs_ptr@ 1 - __internal_cs_ptr! 1 #pop_end#")
    for i in range(1, MAX_RECURSION_DEPTH):
        tokens.append(f"#pop_{i}")
        tokens.append(f"__internal_cs_{i}@ __internal_cs_ptr@ 1 - __internal_cs_ptr! 1 #pop_end#")
    tokens.append("#pop_end")
    
    # Generate return dispatcher
    tokens.append("#__internal_return_dispatcher")
    for i in range(1, MAX_RECURSION_DEPTH * 2):  # Allow for more return IDs
        tokens.append(f"dup {i} = #__ret_{i}#")
    tokens.append("#__ret_invalid")  # Invalid return ID error
    
    return " ".join(tokens)


def generate_function_call(
    func_name: str,
    args_postfix: list[str],
    params: list[str],
    body: str,
    func_line_num: int,
    global_vars: set[str],
    has_return: bool,
    function_local_labels: set[str],
    variables: set[str],
    functions: dict[str, tuple[list[str], str, int, set[str], bool, set[str]]],
    global_mode_for_functions: dict[str, GlobalMode],
    line_num: int,
    current_function: Optional[str],
    return_id: int
) -> str:
    """
    Generate call stack-based function call RPN code.
    """
    
    # Generate function call code
    call_tokens = []
    
    # Push arguments to main stack (in reverse order for LIFO)
    for arg in reversed(args_postfix):
        call_tokens.append(arg)
    
    # Push return ID to call stack
    call_tokens.append(f"{return_id} __internal_push_routine#")
    
    # Jump to function
    call_tokens.append(f"1 #function_{func_name}#")
    
    # Return point
    call_tokens.append(f"#__ret_{return_id}")
    
    return " ".join(call_tokens)


def generate_function_definition(
    func_name: str,
    params: list[str],
    body: str,
    func_line_num: int,
    global_vars: set[str],
    has_return: bool,
    function_local_labels: set[str],
    variables: set[str],
    functions: dict[str, tuple[list[str], str, int, set[str], bool, set[str]]],
    global_mode_for_functions: dict[str, GlobalMode]
) -> str:
    """
    Generate function definition RPN code with prologue and epilogue.
    """
    tokens = []
    
    # Function label
    tokens.append(f"#function_{func_name}")
    
    # Prologue: Pop parameters from main stack to local variables
    for param in reversed(params):  # Reverse because stack is LIFO
        tokens.append(f"__internal_{func_name}_{param}!")
    
    # Process function body
    body_tokens = process_function_body(
        body, func_line_num, func_name, global_vars, 
        function_local_labels, variables, functions, 
        global_mode_for_functions
    )
    tokens.extend(body_tokens)
    
    # Epilogue: Handle return
    if has_return:
        # Return value is already on main stack
        tokens.append("__internal_pop_routine#")
        tokens.append("1 #__internal_return_dispatcher#")
    else:
        # Void function - no return value
        tokens.append("__internal_pop_routine#")
        tokens.append("1 #__internal_return_dispatcher#")
    
    return " ".join(tokens)


def process_function_body(
    body: str,
    func_line_num: int,
    func_name: str,
    global_vars: set[str],
    function_local_labels: set[str],
    variables: set[str],
    functions: dict[str, tuple[list[str], str, int, set[str], bool, set[str]]],
    global_mode_for_functions: dict[str, GlobalMode]
) -> list[str]:
    """
    Process function body and convert to RPN with proper scoping.
    """
    # Use the existing _process_code_block function with function local labels
    return _process_code_block(body, func_line_num, function_local_labels)


_SCIENTIFIC_E_PATTERN = re.compile(r"(?<=[0-9\.])e(?=[+-]?[0-9])", re.IGNORECASE)
_HEX_P_PATTERN = re.compile(r"(?<=[0-9a-fA-F\.])p(?=[+-]?[0-9])", re.IGNORECASE)
_FUNC_CALL_PATTERN = re.compile(r"(\w+)\s*\(")
_FUNC_INFO_PATTERN = re.compile(r"__internal_([a-zA-Z_]\w*)_([a-zA-Z_]\w+)$")
_FUNC_PATTERN = re.compile(r"function\s+(\w+)")
_GLOBAL_DECL_PATTERN = re.compile(r"^<global(.*)>$")
_FUNCTION_DEF_PATTERN = re.compile(r"^\s*function\s+(\w+)\s*\(([^)]*)\)\s*\{")
_M_CALL_PATTERN = re.compile(r"^(\w+)\s*\(")
_PROP_ACCESS_PATTERN = re.compile(r"^\$?(\w+)\.([a-zA-Z_]\w*)$")
_PROP_ACCESS_GENERIC_PATTERN = re.compile(r"(\$?[a-zA-Z_]\w*)\.([a-zA-Z_]\w*)")
_NTH_PATTERN = re.compile(r"^nth_(\d+)$")
_M_LINE_PATTERN = re.compile(r"^([a-zA-Z_]\w*)\s*=\s*(.+)$")
_M_STATIC_PATTERN = re.compile(r"^\$?(\w+)\[\s*(-?\d+)\s*,\s*(-?\d+)\s*\](\:\\w+)?$")
_FIND_DUPLICATE_FUNCTIONS_PATTERN = re.compile(r"\bfunction\s+(\w+)\s*\(.*?\)")
_GLOBAL_MATCH_PATTERN = re.compile(r"<([a-zA-Z_]\w*)>")
_ASSIGN_PATTERN = re.compile(r"(?<![<>!])=(?![=])")
_REL_PATTERN = re.compile(r"\w+\[(.*?)\]")
_FUNC_SUB_PATTERN = re.compile(r"\w+\([^)]*\)|\[[^\]]*\]")
_IDENTIFIER_PATTERN = re.compile(r"([\$a-zA-Z_]\w*)(?!\s*\()")
_LETTER_PATTERN = re.compile(r"[a-zA-Z_]\w*")
_FUNCTION_PATTERN = re.compile(
    r"function\s+(\w+)\s*\(([^)]*)\)\s*\{([^{}]*(?:\{[^{}]*\}[^{}]*)*)\}"
)
_BUILD_IN_FUNC_PATTERNS = [
    re.compile(r) for r in [rf"^{prefix}\d+$" for prefix in ["nth_", "sort"]]
]
_BRACED_BLOCK_CONTENT_PATTERN = r"\{([^{}]*(?:\{[^{}]*\}[^{}]*)*)\}"
_IF_PATTERN = re.compile(r"^\s*if\s*\((.*?)\)\s*" + _BRACED_BLOCK_CONTENT_PATTERN)
_ELSE_PATTERN = re.compile(r"^\s*else\s*" + _BRACED_BLOCK_CONTENT_PATTERN)
_LABEL_PATTERN = re.compile(r"^\s*([a-zA-Z_]\w*):")
_IF_GOTO_PATTERN = re.compile(r"^\s*if\s*\((.*)\)\s*goto\s+([a-zA-Z_]\w*)")
_GOTO_PATTERN = re.compile(r"^\s*goto\s+([a-zA-Z_]\w*)")
_LABEL_PATTERN = re.compile(r"^\s*([a-zA-Z_]\w*):")
_IF_GOTO_PATTERN = re.compile(r"^\s*if\s*\((.*)\)\s*goto\s+([a-zA-Z_]\w*)")


class SyntaxError(Exception):
    """Custom syntax error class with line information"""

    def __init__(
        self,
        message: str,
        line_num: Optional[int] = None,
        function_name: Optional[str] = None,
    ):
        self.line_num = line_num
        self.function_name = function_name
        if function_name and line_num is not None:
            super().__init__(
                f"Line {line_num}: In function '{function_name}', {message}"
            )
        elif line_num is not None:
            super().__init__(f"Line {line_num}: {message}")
        else:
            super().__init__(message)


@lru_cache
def strip_outer_parentheses(expr: str) -> str:
    """
    Remove outer parentheses if the entire expression is enclosed.
    """
    if not expr.startswith("(") or not expr.endswith(")"):
        return expr
    count = 0
    for i, char in enumerate(expr):
        if char == "(":
            count += 1
        elif char == ")":
            count -= 1
        if count == 0 and i < len(expr) - 1:
            return expr
    return expr[1:-1]


@lru_cache
def match_full_function_call(expr: str) -> Optional[tuple[str, str]]:
    """
    Try to match a complete function call with proper nesting.
    Returns (func_name, args_str) if matched; otherwise returns None.
    """
    expr = expr.strip()
    m = _FUNC_CALL_PATTERN.match(expr)
    if not m:
        return None
    func_name = m.group(1)
    start = m.end() - 1  # position of '('
    depth = 0
    for i in range(start, len(expr)):
        if expr[i] == "(":
            depth += 1
        elif expr[i] == ")":
            depth -= 1
            if depth == 0:
                if i == len(expr) - 1:
                    args_str = expr[start + 1 : i]
                    return func_name, args_str
                else:
                    return None
    return None


@lru_cache
def extract_function_info(
    internal_var: str, current_function: Optional[str] = None
) -> tuple[Optional[str], Optional[str]]:
    """
    Given a renamed internal variable (i.e. __internal_funcname_varname),
    extract the original function name and variable name.
    """
    if current_function is not None and internal_var.startswith(
        f"__internal_{current_function}_"
    ):
        return current_function, internal_var[len(f"__internal_{current_function}_") :]
    match = _FUNC_INFO_PATTERN.match(internal_var)
    if match:
        return match.group(1), match.group(2)
    return None, None


def find_duplicate_functions(code: str):
    """
    Check if any duplicate function is defined.
    """
    function_lines = {}
    lines = code.split("\n")
    for line_num, line in enumerate(lines, start=1):
        match = _FIND_DUPLICATE_FUNCTIONS_PATTERN.search(line)
        if match:
            func_name = match.group(1)
            if func_name in function_lines:
                raise SyntaxError(
                    f"Duplicated function '{func_name}' defined at lines {function_lines[func_name]} and {line_num}",
                    line_num=line_num,
                )
            function_lines[func_name] = line_num


def compute_stack_effect(
    postfix_expr: str, line_num: Optional[int] = None, func_name: Optional[str] = None
) -> int:
    """
    Compute the net stack effect of a postfix expression.
    """
    tokens = tokenize_expr(postfix_expr)
    stack_size = 0
    for i, token in enumerate(tokens):
        arity = get_op_arity(token)
        if stack_size < arity:
            raise SyntaxError(
                f"Stack underflow for operator {token} at token index {i} \n"
                f"Stack size: {stack_size}, arity: {arity} \n"
                f"Tokens: {' '.join(tokens)}",
                line_num,
                func_name,
            )

        stack_size += get_stack_effect(token)

    return stack_size


def parse_ternary(
    expr: str, line_num: Optional[int] = None, func_name: Optional[str] = None
) -> Optional[tuple[str, str, str]]:
    """
    For a given expression string without delimiters, detects the presence of a ternary operator in the outer layer.
    If present, returns a triple (condition, true_expr, false_expr); otherwise returns None.
    """
    expr = expr.strip()
    n = len(expr)
    depth = 0
    q_index = -1
    for i, c in enumerate(expr):
        if c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
        elif c == "?" and depth == 0:
            q_index = i
            break
    if q_index == -1:
        return None

    condition = expr[:q_index].strip()

    ternary_level = 1
    depth = 0
    colon_index = -1
    for i in range(q_index + 1, n):
        c = expr[i]
        if c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
        elif c == "?" and depth == 0:
            ternary_level += 1
        elif c == ":" and depth == 0:
            ternary_level -= 1
            if ternary_level == 0:
                colon_index = i
                break
    if colon_index == -1:
        raise SyntaxError("Ternary operator missing ':'", line_num, func_name)
    true_expr = expr[q_index + 1 : colon_index].strip()
    false_expr = expr[colon_index + 1 :].strip()
    return (condition, true_expr, false_expr)


def validate_static_relative_pixel_indices(
    expr: str, line_num: int, function_name: Optional[str] = None
) -> None:
    """
    Validate that the static relative pixel access indices are numeric constants.
    """
    static_relative_pixel_indices = _REL_PATTERN.finditer(expr)
    for match in static_relative_pixel_indices:
        indices = match.group(1).split(",")
        for idx in indices:
            if not is_token_numeric(idx.strip()):
                raise SyntaxError(
                    f"Static relative pixel access index must be a numeric constant, got '{idx.strip()}'",
                    line_num,
                    function_name,
                )


def check_variable_usage(
    expr: str,
    variables: set[str],
    line_num: int,
    function_name: Optional[str] = None,
    local_vars: Optional[set[str]] = None,
    literals_in_scope: Optional[set[str]] = None,
) -> None:
    """
    Check that all variables in the expression have been defined.
    In function scope, if local_vars is provided, only local variables are checked.
    """

    # Scientific notation and hexadecimal notation handling
    expr = _SCIENTIFIC_E_PATTERN.sub(" ", expr)
    expr = _HEX_P_PATTERN.sub(" ", expr)

    def prop_repl(m: re.Match[str]) -> str:
        clip_candidate = m.group(1)
        if is_clip_infix(clip_candidate) or is_clip_infix(f"${clip_candidate}"):
            return " "
        return m.group(0)

    expr = _PROP_ACCESS_GENERIC_PATTERN.sub(prop_repl, expr)
    expr = _FUNC_SUB_PATTERN.sub("", expr)
    identifiers = _IDENTIFIER_PATTERN.finditer(expr)
    for match in identifiers:
        var_name = match.group(1)
        if (
            (literals_in_scope and var_name in literals_in_scope)
            or is_constant_infix(var_name)
            or (
                (local_vars is not None and var_name in local_vars)
                or (local_vars is None and var_name in variables)
            )
        ):
            continue
        if var_name.startswith("__internal_"):
            func_name_extracted, orig_var = extract_function_info(
                var_name, function_name
            )
            if local_vars is not None and orig_var in local_vars:
                continue
            raise SyntaxError(
                f"Variable '{orig_var}' used before definition",
                line_num,
                func_name_extracted,
            )
        if not re.search(rf"{re.escape(var_name)}\s*=", expr):
            if var_name.startswith("$"):
                raise SyntaxError(
                    f"Unknown constant '{var_name}'", line_num, function_name
                )
            raise SyntaxError(
                f"Variable '{var_name}' used before definition",
                line_num,
                function_name,
            )


def convert_expr(
    expr: str,
    variables: set[str],
    functions: dict[str, tuple[list[str], str, int, set[str], bool, set[str]]],
    line_num: int,
    global_mode_for_functions: dict[str, GlobalMode],
    current_function: Optional[str] = None,
    local_vars: Optional[set[str]] = None,
    literals_in_scope: Optional[set[str]] = None,
) -> str:
    """
    Convert a single infix expression to a postfix expression.
    Supports binary and unary operators, function calls, and custom function definitions.
    """
    expr = expr.strip()

    if is_token_numeric(expr):
        return expr

    prop_access_match = _PROP_ACCESS_PATTERN.match(expr)
    if prop_access_match:
        clip_name = prop_access_match.group(1)
        prop_name = prop_access_match.group(2)
        if is_clip_infix(f"${clip_name}") or is_clip_infix(clip_name):
            return f"{clip_name.lstrip('$')}.{prop_name}"

    # Check variable usage and validate static relative pixel indices.
    check_variable_usage(
        expr, variables, line_num, current_function, local_vars, literals_in_scope
    )
    validate_static_relative_pixel_indices(expr, line_num, current_function)

    func_call_full = match_full_function_call(expr)
    if func_call_full:
        func_name, args_str = func_call_full
        if func_name not in functions and not is_builtin_function(func_name):
            raise SyntaxError(
                f"Undefined function '{func_name}'", line_num, current_function
            )

        m_nth = _NTH_PATTERN.match(func_name)
        if m_nth:
            N_val = int(m_nth.group(1))
            args = parse_args(args_str)
            M = len(args)
            if M < N_val:
                raise SyntaxError(
                    f"nth_{N_val} requires at least {N_val} arguments, but only {M} were provided.",
                    line_num,
                    current_function,
                )
            if N_val < 1:
                raise SyntaxError(
                    f"nth_{N_val} is not supported!",
                    line_num,
                    current_function,
                )
            args_postfix = [
                convert_expr(
                    arg,
                    variables,
                    functions,
                    line_num,
                    global_mode_for_functions,
                    current_function,
                    local_vars,
                    literals_in_scope,
                )
                for arg in args
            ]
            sort_token = f"sort{M}"
            drop_before = f"drop{N_val-1}" if (N_val - 1) > 0 else ""
            drop_after = f"drop{M-N_val}" if (M - N_val) > 0 else ""
            tokens: list[str] = []
            tokens.append(" ".join(args_postfix))
            tokens.append(sort_token)
            if drop_before:
                tokens.append(drop_before)
            tokens.append("__internal_nth_tmp!")
            if drop_after:
                tokens.append(drop_after)
            tokens.append("__internal_nth_tmp@")
            return " ".join(tokens).strip()

        args = parse_args(args_str)
        args_postfix = [
            convert_expr(
                arg,
                variables,
                functions,
                line_num,
                global_mode_for_functions,
                current_function,
                local_vars,
                literals_in_scope,
            )
            for arg in args
        ]

        if func_name == "dyn":
            if len(args) != 3:
                raise SyntaxError(
                    f"Built-in function {func_name} requires 3 arguments, but {len(args)} were provided.",
                    line_num,
                    current_function,
                )
            # Check if the first argument is a clip
            clip_arg = args[0]
            if not is_clip_infix(clip_arg):
                raise SyntaxError(
                    f"{args[0]} is not a source clip.", line_num, current_function
                )
            return f"{args_postfix[1]} {args_postfix[2]} {clip_arg.lstrip('$')}[]"

        if func_name in ["min", "max", "atan2", "copysign"]:
            if len(args) != 2:
                raise SyntaxError(
                    f"Built-in function {func_name} requires 2 arguments, but {len(args)} were provided.",
                    line_num,
                    current_function,
                )
            return f"{args_postfix[0]} {args_postfix[1]} {func_name}"
        elif func_name in ["clamp", "fma"]:
            if len(args) != 3:
                raise SyntaxError(
                    f"Built-in function {func_name} requires 3 arguments, but {len(args)} were provided.",
                    line_num,
                    current_function,
                )
            return f"{args_postfix[0]} {args_postfix[1]} {args_postfix[2]} {func_name}"
        else:
            builtin_unary = [
                "sin",
                "cos",
                "log",
                "exp",
                "round",
                "floor",
                "abs",
                "sqrt",
                "trunc",
                "exp2",
                "log2",
                "log10",
                "tan",
                "asin",
                "acos",
                "atan",
                "sinh",
                "cosh",
                "tanh",
                "ceil",
            ]
            if func_name in builtin_unary:
                if len(args) != 1:
                    raise SyntaxError(
                        f"Built-in function {func_name} requires 1 argument, but {len(args)} were provided.",
                        line_num,
                        current_function,
                    )
                return f"{args_postfix[0]} {func_name}"
        # Handle custom function calls with call stack simulation
        if func_name in functions:
            params, body, func_line_num, global_vars, has_return, function_local_labels = functions[func_name]
            if len(args) != len(params):
                raise SyntaxError(
                    f"Function {func_name} requires {len(params)} parameters, but {len(args)} were provided.",
                    line_num,
                    current_function,
                )
            
            # Generate call stack-based function call
            global next_return_id
            return_id = next_return_id
            next_return_id += 1
            
            return generate_function_call(
                func_name, args_postfix, params, body, func_line_num, 
                global_vars, has_return, function_local_labels, 
                variables, functions, global_mode_for_functions, 
                line_num, current_function, return_id
            )

    stripped = strip_outer_parentheses(expr)
    if stripped != expr:
        return convert_expr(
            stripped,
            variables,
            functions,
            line_num,
            global_mode_for_functions,
            current_function,
            local_vars,
            literals_in_scope,
        )

    m_static = _M_STATIC_PATTERN.match(expr)
    if m_static:
        clip = m_static.group(1)
        statX = m_static.group(2)
        statY = m_static.group(3)
        suffix = m_static.group(4) or ""
        if not is_clip_postfix(clip):
            raise SyntaxError(f"'{clip}' is not a clip!", line_num, current_function)
        try:
            int(statX)
            int(statY)
        except ValueError:
            raise SyntaxError(
                f"Static relative pixel access indices must be integers, got '[{statX},{statY}]'",
                line_num,
                current_function,
            )
        return f"{clip}[{statX},{statY}]{suffix}"

    ternary_parts = parse_ternary(expr, line_num, current_function)
    if ternary_parts is not None:
        cond, true_expr, false_expr = ternary_parts
        cond_conv = convert_expr(
            cond,
            variables,
            functions,
            line_num,
            global_mode_for_functions,
            current_function,
            local_vars,
            literals_in_scope,
        )
        true_conv = convert_expr(
            true_expr,
            variables,
            functions,
            line_num,
            global_mode_for_functions,
            current_function,
            local_vars,
            literals_in_scope,
        )
        false_conv = convert_expr(
            false_expr,
            variables,
            functions,
            line_num,
            global_mode_for_functions,
            current_function,
            local_vars,
            literals_in_scope,
        )
        return f"{cond_conv} {true_conv} {false_conv} ?"

    operators = [
        ("||", "or"),
        ("&&", "and"),
        ("|", "bitor"),
        ("^", "bitxor"),
        ("&", "bitand"),
        ("<", "<"),
        ("<=", "<="),
        (">", ">"),
        (">=", ">="),
        ("==", "="),
        ("!=", "= not"),
        ("+", "+"),
        ("-", "-"),
        ("*", "*"),
        ("/", "/"),
        ("%", "%"),
        ("**", "pow"),
    ]
    for op_str, postfix_op in operators:
        left, right = find_binary_op(expr, op_str)
        if left is not None and right is not None:
            left_postfix = convert_expr(
                left,
                variables,
                functions,
                line_num,
                global_mode_for_functions,
                current_function,
                local_vars,
                literals_in_scope,
            )
            right_postfix = convert_expr(
                right,
                variables,
                functions,
                line_num,
                global_mode_for_functions,
                current_function,
                local_vars,
                literals_in_scope,
            )
            if _LETTER_PATTERN.fullmatch(
                left.strip()
            ) and not left_postfix.strip().endswith("@"):
                if not (is_constant_postfix(f"{left_postfix.strip()}")) and not (
                    literals_in_scope and left_postfix.strip() in literals_in_scope
                ):
                    left_postfix = left_postfix.strip() + "@"
            if _LETTER_PATTERN.fullmatch(
                right.strip()
            ) and not right_postfix.strip().endswith("@"):
                if not (is_constant_postfix(f"{right_postfix.strip()}")) and not (
                    literals_in_scope and right_postfix.strip() in literals_in_scope
                ):
                    right_postfix = right_postfix.strip() + "@"

            return f"{left_postfix} {right_postfix} {postfix_op}"

    if expr.startswith("!"):
        operand = convert_expr(
            expr[1:],
            variables,
            functions,
            line_num,
            global_mode_for_functions,
            current_function,
            local_vars,
            literals_in_scope,
        )
        return f"{operand} not"

    if expr.startswith("-"):
        operand = convert_expr(
            expr[1:],
            variables,
            functions,
            line_num,
            global_mode_for_functions,
            current_function,
            local_vars,
            literals_in_scope,
        )
        return f"{operand} -1 *"

    if expr.startswith("~"):
        operand = convert_expr(
            expr[1:],
            variables,
            functions,
            line_num,
            global_mode_for_functions,
            current_function,
            local_vars,
            literals_in_scope,
        )
        return f"{operand} bitnot"

    if is_constant_infix(expr):
        # Remove $ prefix from constants in postfix expression
        return expr[1:]

    if _LETTER_PATTERN.fullmatch(expr):
        if literals_in_scope and expr in literals_in_scope:
            return expr

        # Only add '@' for known variables
        is_var = False
        if local_vars is not None:
            # Function scope
            is_var = expr in local_vars
            if not is_var and expr.startswith("__internal_"):
                _, orig_var = extract_function_info(expr, current_function)
                if orig_var in local_vars:
                    is_var = True
        else:
            # Global scope
            if variables is not None:
                is_var = expr in variables

        if is_var:
            return expr if expr.endswith("@") else expr + "@"

        raise SyntaxError(
            f"Variable '{expr}' used before definition", line_num, current_function
        )

    return expr


def find_binary_op(expr: str, op: str) -> tuple[Optional[str], Optional[str]]:
    """
    Find the last occurrence of the binary operator op at the outer level and return the left and right parts.
    """
    if not op:
        return None, None

    paren_level = 0
    levels = [0] * len(expr)
    for i, char in enumerate(expr):
        levels[i] = paren_level
        if char == "(":
            paren_level += 1
        elif char == ")":
            paren_level -= 1

    for i in range(len(expr) - len(op), -1, -1):
        if levels[i] == 0 and expr.startswith(op, i):
            candidate = i
            left_valid = (candidate == 0) or (expr[candidate - 1] in " (,\t")
            after = candidate + len(op)
            right_valid = (after == len(expr)) or (expr[after] in " )\t,")

            if left_valid and right_valid:
                left = expr[:candidate].strip()
                right = expr[candidate + len(op) :].strip()
                return left, right

    return None, None


def parse_args(args_str: str) -> list[str]:
    """
    Parse the arguments of a function call.
    """
    args: list[str] = []
    current: list[str] = []
    bracket_depth = 0
    square_bracket_depth = 0
    for c in args_str + ",":
        if c == "," and bracket_depth == 0 and square_bracket_depth == 0:
            args.append("".join(current).strip())
            current = []
        else:
            current.append(c)
            if c == "(":
                bracket_depth += 1
            elif c == ")":
                bracket_depth -= 1
            elif c == "[":
                square_bracket_depth += 1
            elif c == "]":
                square_bracket_depth -= 1
    return [arg for arg in args if arg]


@lru_cache
def is_builtin_function(func_name: str) -> bool:
    """
    Check if the function name belongs to a built-in function.
    """
    builtin_unary = [
        "sin",
        "cos",
        "log",
        "exp",
        "round",
        "floor",
        "abs",
        "sqrt",
        "trunc",
        "exp2",
        "log2",
        "log10",
        "tan",
        "asin",
        "acos",
        "atan",
        "sinh",
        "cosh",
        "tanh",
        "ceil",
    ]
    builtin_binary = ["min", "max", "atan2", "copysign"]
    builtin_ternary = ["clamp", "dyn", "fma"]
    if any(r.match(func_name) for r in _BUILD_IN_FUNC_PATTERNS):
        return True
    return (
        func_name in builtin_unary
        or func_name in builtin_binary
        or func_name in builtin_ternary
    )
