// imperative_queue_test.cpp
#include "ImperativeQueue.cpp"
#include <iostream>
#include <chrono>

using namespace std;
using namespace std::chrono;

int main() {
    Queue<int> queue;

    // Start time
    auto start = high_resolution_clock::now();

    // Enqueue elements
    queue.enqueue(10);
    queue.enqueue(20);
    cout << "Queue after enqueuing 10 and 20: " << queue.peek() << endl;
    cout << "Is queue empty? " << (queue.isEmpty() ? "Yes" : "No") << endl;

    // Additional tests
    queue.enqueue(30);
    queue.enqueue(40);
    cout << "Queue after enqueuing 30 and 40: " << queue.peek() << endl;

    int dequeuedElement = queue.dequeue();
    cout << "Dequeued element: " << dequeuedElement << endl;
    cout << "Queue after dequeuing: " << queue.peek() << endl;

    dequeuedElement = queue.dequeue();
    cout << "Dequeued element: " << dequeuedElement << endl;
    cout << "Queue after dequeuing: " << queue.peek() << endl;

    dequeuedElement = queue.dequeue();
    cout << "Dequeued element: " << dequeuedElement << endl;
    cout << "Is queue empty? " << (queue.isEmpty() ? "Yes" : "No") << endl;

    // End time
    auto end = high_resolution_clock::now();

    // Calculate the duration
    duration<double> duration = end - start;
    cout << "Time taken: " << duration.count() << " seconds" << endl;

    return 0;
}