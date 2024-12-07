// ImperativeStack.cpp
#include <iostream>
#include <vector>
using namespace std;

// Stack Implementation
template <typename T>
class Stack {
private:
    vector<T> elements;

public:
    // Push an element onto the stack
    void push(T value) {
        elements.push_back(value);
    }

    // Pop an element from the stack
    T pop() {
        if (elements.empty()) {
            throw runtime_error("Pop from an empty stack");
        }
        T top = elements.back();
        elements.pop_back();
        return top;
    }

    // Peek at the top element
    T peek() {
        if (elements.empty()) {
            throw runtime_error("Peek from an empty stack");
        }
        return elements.back();
    }

    // Check if the stack is empty
    bool isEmpty() {
        return elements.empty();
    }
};
