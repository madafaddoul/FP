// ImperativeQueue.cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <stdexcept>
using namespace std;

// Queue Implementation
template <typename T>
class Queue {
private:
    vector<T> elements;

public:
    // Enqueue an element onto the queue
    void enqueue(T value) {
        elements.push_back(value);
    }

    // Dequeue an element from the queue
    T dequeue() {
        if (elements.empty()) {
            throw runtime_error("Dequeue from an empty queue");
        }
        T front = elements.front();
        elements.erase(elements.begin());
        return front;
    }

    // Peek at the front element of the queue
    T peek() const {
        if (elements.empty()) {
            throw runtime_error("Peek from an empty queue");
        }
        return elements.front();
    }

    // Check if the queue is empty
    bool isEmpty() const {
        return elements.empty();
    }

    // Get the size of the queue
    size_t size() const {
        return elements.size();
    }

    // Reverse the queue
    void reverse() {
        std::reverse(elements.begin(), elements.end());
    }

    // Convert the queue to a list
    vector<T> toList() const {
        return elements;
    }

    // Create a queue from a list
    void fromList(const vector<T>& list) {
        elements = list;
    }

    // Map a function over the queue
    template <typename Func>
    void map(Func func) {
        for (auto& element : elements) {
            element = func(element);
        }
    }
};