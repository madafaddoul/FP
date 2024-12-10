// ImperativeStack.cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <stdexcept>
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

    // Peek at the top element of the stack
    T peek() const {
        if (elements.empty()) {
            throw runtime_error("Peek from an empty stack");
        }
        return elements.back();
    }

    // Check if the stack is empty
    bool isEmpty() const {
        return elements.empty();
    }

    // Get the size of the stack
    size_t size() const {
        return elements.size();
    }

    // Reverse the stack
    void reverse() {
        std::reverse(elements.begin(), elements.end());
    }

    // Convert the stack to a list
    vector<T> toList() const {
        return elements;
    }

    // Create a stack from a list
    void fromList(const vector<T>& list) {
        elements = list;
    }

    // Map a function over the stack
    template <typename Func>
    void map(Func func) {
        for (auto& element : elements) {
            element = func(element);
        }
    }
};