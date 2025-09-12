import sys
import os

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from exprutils import infix2postfix, postfix2infix

TEST_CASES = {
    "if_else": """
        if ($x > 10) {
            RESULT = 100
        } else {
            RESULT = 200
        }
    """,
    "if_only": """
        RESULT = 50
        if ($x > 10) {
            RESULT = 100
        }
    """,
    "if_goto": """
        RESULT = 1
        if ($x > 10) goto skip
        RESULT = 2
        skip:
    """,
    "unconditional_goto": """
        RESULT = 1
        goto final
        RESULT = 2
        final:
    """,
    "nested_if_else": """
        if ($x > 10) {
            if ($y > 20) {
                RESULT = 1
            } else {
                RESULT = 2
            }
        } else {
            RESULT = 3
        }
    """,
    "goto_inside_if": """
        RESULT = 0
        if ($x > 10) {
            RESULT = 1
            goto end
            RESULT = 2
        }
        RESULT = 3
        end:
    """
}

def main():
    print("--- Running Control Flow Conversion Tests ---")
    for name, infix_code in TEST_CASES.items():
        print("\n" + "="*50)
        print(f"TEST CASE: {name}")
        print("="*50)
        
        print("--- Infix Input ---")
        print(infix_code.strip())
        
        try:
            print("\n--- Postfix Output ---")
            postfix_expr = infix2postfix(infix_code)
            print(postfix_expr)
            
            print("\n--- Round-trip to Infix ---")
            roundtrip_infix_code = postfix2infix(postfix_expr)
            print(roundtrip_infix_code.strip())
            
        except Exception as e:
            print(f"\n*** AN ERROR OCCURRED: {e} ***")
        
        print("="*50 + "\n")

if __name__ == '__main__':
    main()
