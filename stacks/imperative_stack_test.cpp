// imperative_stack_test.cpp
#include "ImperativeStack.cpp"
#include <iostream>
#include <chrono>

using namespace std;
using namespace std::chrono;

int main() {
    Stack<int> stack;

    // Start time
    auto start = high_resolution_clock::now();

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

    // End time
    auto end = high_resolution_clock::now();

    // Calculate the duration
    duration<double> duration = end - start;
    cout << "Time taken: " << duration.count() << " seconds" << endl;

    return 0;
}