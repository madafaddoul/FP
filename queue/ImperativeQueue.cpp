// ImperativeQueue.cpp
#include <stack>
#include <stdexcept>

template <typename T>
class Queue {
public:
    void enqueue(T x) {
        inStack.push(x);
    }

    T dequeue() {
        if (outStack.empty()) {
            while (!inStack.empty()) {
                outStack.push(inStack.top());
                inStack.pop();
            }
        }
        if (outStack.empty()) {
            throw std::runtime_error("Dequeue from an empty queue");
        }
        T front = outStack.top();
        outStack.pop();
        return front;
    }

    T peek() {
        if (outStack.empty()) {
            while (!inStack.empty()) {
                outStack.push(inStack.top());
                inStack.pop();
            }
        }
        if (outStack.empty()) {
            throw std::runtime_error("Peek from an empty queue");
        }
        return outStack.top();
    }

    bool isEmpty() const {
        return inStack.empty() && outStack.empty();
    }

private:
    std::stack<T> inStack;
    std::stack<T> outStack;
};