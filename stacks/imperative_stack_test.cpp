// imperative_stack_test.cpp
#include "ImperativeStack.cpp"
#include <iostream>
using namespace std;

int main() {
    Stack<int> stack;

    // Push elements onto the stack
    stack.push(10);
    stack.push(20);
    cout << "Top element after pushing 10 and 20: " << stack.peek() << endl;

    // Pop an element
    cout << "Popped element: " << stack.pop() << endl;

    // Peek at the top element
    cout << "Top element after popping: " << stack.peek() << endl;

    // Check if the stack is empty
    cout << "Is the stack empty? " << (stack.isEmpty() ? "Yes" : "No") << endl;

    return 0;
}
