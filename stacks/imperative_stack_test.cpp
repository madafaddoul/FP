// ImperativeStackTest.cpp
#include "ImperativeStack.cpp"
#include <iostream>
#include <chrono>
using namespace std;
using namespace std::chrono;

int main() {
    auto start = high_resolution_clock::now();

    Stack<int> stack;
    stack.push(10);
    stack.push(20);
    cout << "Stack after pushing 10 and 20: ";
    for (int val : stack.toList()) cout << val << " ";
    cout << endl;

    cout << "Top element (peek): " << stack.peek() << endl;

    int poppedElement = stack.pop();
    cout << "Popped element: " << poppedElement << endl;

    cout << "Stack after popping: ";
    for (int val : stack.toList()) cout << val << " ";
    cout << endl;

    // Additional tests for new functions
    cout << "Is stack empty: " << stack.isEmpty() << endl;
    cout << "Size of stack: " << stack.size() << endl;

    stack.reverse();
    cout << "Stack after reversing: ";
    for (int val : stack.toList()) cout << val << " ";
    cout << endl;

    vector<int> list = {1, 2, 3};
    stack.fromList(list);
    cout << "Stack from list [1, 2, 3]: ";
    for (int val : stack.toList()) cout << val << " ";
    cout << endl;

    stack.map([](int x) { return x + 1; });
    cout << "Stack after mapping (+1): ";
    for (int val : stack.toList()) cout << val << " ";
    cout << endl;

    auto end = high_resolution_clock::now();
    auto diff = duration_cast<microseconds>(end - start).count();
    cout << "Time taken: " << diff << " microseconds" << endl;

    return 0;
}