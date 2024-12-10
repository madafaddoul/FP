// ImperativeQueueTest.cpp
#include "ImperativeQueue.cpp"
#include <iostream>
#include <chrono>
using namespace std;
using namespace std::chrono;

int main() {
    auto start = high_resolution_clock::now();

    Queue<int> queue;
    queue.enqueue(10);
    queue.enqueue(20);
    cout << "Queue after enqueuing 10 and 20: ";
    for (int val : queue.toList()) cout << val << " ";
    cout << endl;

    cout << "Front element (peek): " << queue.peek() << endl;

    int dequeuedElement = queue.dequeue();
    cout << "Dequeued element: " << dequeuedElement << endl;

    cout << "Queue after dequeuing: ";
    for (int val : queue.toList()) cout << val << " ";
    cout << endl;

    // Additional tests
    cout << "Is queue empty: " << queue.isEmpty() << endl;
    cout << "Size of queue: " << queue.size() << endl;

    queue.reverse();
    cout << "Queue after reversing: ";
    for (int val : queue.toList()) cout << val << " ";
    cout << endl;

    vector<int> list = {1, 2, 3};
    queue.fromList(list);
    cout << "Queue from list [1, 2, 3]: ";
    for (int val : queue.toList()) cout << val << " ";
    cout << endl;

    queue.map([](int x) { return x + 1; });
    cout << "Queue after mapping (+1): ";
    for (int val : queue.toList()) cout << val << " ";
    cout << endl;

    auto end = high_resolution_clock::now();
    auto diff = duration_cast<microseconds>(end - start).count();
    cout << "Time taken: " << diff << " microseconds" << endl;

    return 0;
}
