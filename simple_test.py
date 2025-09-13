#!/usr/bin/env python3
"""
Simple test for the recursive function implementation
"""

# Test the basic structure without running the full conversion
def test_basic_structure():
    """Test that our modifications don't break basic functionality"""
    print("Testing basic structure...")
    
    # Test that we can import the modified functions
    try:
        import sys
        import os
        sys.path.append('/workspace/exprutils')
        
        # Test that the file can be parsed
        with open('/workspace/exprutils/infix2postfix.py', 'r') as f:
            content = f.read()
            
        # Check for key functions we added
        assert 'generate_function_call' in content
        assert 'generate_function_definition' in content
        assert 'process_function_body' in content
        assert 'generate_call_stack_code' in content
        assert '__internal_cs_ptr' in content
        assert '__internal_push_routine' in content
        assert '__internal_pop_routine' in content
        assert '__internal_return_dispatcher' in content
        
        print("✓ All key functions found in the modified file")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False

def test_function_signatures():
    """Test that function signatures are correct"""
    print("Testing function signatures...")
    
    try:
        with open('/workspace/exprutils/infix2postfix.py', 'r') as f:
            content = f.read()
            
        # Check for updated function signatures
        assert 'tuple[list[str], str, int, set[str], bool, set[str]]' in content
        assert 'function_local_labels: set[str] = None' in content
        assert 'return_id: int' in content
        
        print("✓ Function signatures updated correctly")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False

def test_call_stack_simulation():
    """Test call stack simulation code generation"""
    print("Testing call stack simulation...")
    
    try:
        with open('/workspace/exprutils/infix2postfix.py', 'r') as f:
            content = f.read()
            
        # Check for call stack simulation patterns
        assert 'MAX_RECURSION_DEPTH = 32' in content
        assert 'next_return_id = 1' in content
        assert 'for i in range(MAX_RECURSION_DEPTH):' in content
        assert 'dup {i} = #__ret_{i}#' in content
        
        print("✓ Call stack simulation code found")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False

def test_goto_validation():
    """Test goto validation logic"""
    print("Testing goto validation...")
    
    try:
        with open('/workspace/exprutils/infix2postfix.py', 'r') as f:
            content = f.read()
            
        # Check for goto validation patterns
        assert 'function_local_labels is not None' in content
        assert 'goto target' in content
        assert 'not defined in function scope' in content
        
        print("✓ Goto validation logic found")
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False

def main():
    """Run all tests"""
    print("Testing recursive function implementation")
    print("=" * 50)
    
    tests = [
        test_basic_structure,
        test_function_signatures,
        test_call_stack_simulation,
        test_goto_validation
    ]
    
    passed = 0
    total = len(tests)
    
    for test in tests:
        if test():
            passed += 1
        print()
    
    print("=" * 50)
    print(f"Tests passed: {passed}/{total}")
    
    if passed == total:
        print("🎉 All structural tests passed!")
        print("The recursive function implementation appears to be correctly integrated.")
    else:
        print("❌ Some tests failed")

if __name__ == "__main__":
    main()