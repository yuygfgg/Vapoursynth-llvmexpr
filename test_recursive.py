#!/usr/bin/env python3
"""
Test script for recursive function support in infix2postfix.py
"""

import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), 'exprutils'))

# Import directly from the module
from exprutils.infix2postfix import infix2postfix

def test_factorial():
    """Test recursive factorial function"""
    print("Testing recursive factorial function...")
    
    code = """
function factorial(n) {
    if (n <= 1) {
        return 1
    } else {
        return n * factorial(n - 1)
    }
}

RESULT = factorial(5)
"""
    
    try:
        result = infix2postfix(code)
        print("✓ Factorial function converted successfully")
        print(f"Result length: {len(result.split())} tokens")
        print("First 20 tokens:", " ".join(result.split()[:20]))
        return True
    except Exception as e:
        print(f"✗ Error: {e}")
        return False

def test_fibonacci():
    """Test recursive fibonacci function"""
    print("\nTesting recursive fibonacci function...")
    
    code = """
function fibonacci(n) {
    if (n <= 1) {
        return n
    } else {
        return fibonacci(n - 1) + fibonacci(n - 2)
    }
}

RESULT = fibonacci(6)
"""
    
    try:
        result = infix2postfix(code)
        print("✓ Fibonacci function converted successfully")
        print(f"Result length: {len(result.split())} tokens")
        return True
    except Exception as e:
        print(f"✗ Error: {e}")
        return False

def test_function_with_goto():
    """Test function with goto statements"""
    print("\nTesting function with goto statements...")
    
    code = """
function test_goto(x) {
    start:
    if (x > 0) {
        x = x - 1
        goto start
    }
    return x
}

RESULT = test_goto(3)
"""
    
    try:
        result = infix2postfix(code)
        print("✓ Function with goto converted successfully")
        print(f"Result length: {len(result.split())} tokens")
        return True
    except Exception as e:
        print(f"✗ Error: {e}")
        return False

def test_cross_function_goto_error():
    """Test that cross-function goto is not allowed"""
    print("\nTesting cross-function goto error...")
    
    code = """
function func1() {
    label1:
    return 1
}

function func2() {
    goto label1  # This should cause an error
    return 2
}

RESULT = func1()
"""
    
    try:
        result = infix2postfix(code)
        print("✗ Cross-function goto should have caused an error")
        return False
    except Exception as e:
        print(f"✓ Cross-function goto correctly rejected: {e}")
        return True

def main():
    """Run all tests"""
    print("Testing recursive function support in infix2postfix.py")
    print("=" * 60)
    
    tests = [
        test_factorial,
        test_fibonacci,
        test_function_with_goto,
        test_cross_function_goto_error
    ]
    
    passed = 0
    total = len(tests)
    
    for test in tests:
        if test():
            passed += 1
    
    print("\n" + "=" * 60)
    print(f"Tests passed: {passed}/{total}")
    
    if passed == total:
        print("🎉 All tests passed!")
    else:
        print("❌ Some tests failed")

if __name__ == "__main__":
    main()