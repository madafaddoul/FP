// imperative_queue_test.cpp
#include "ImperativeQueue.cpp"
#include <iostream>

using namespace std;

int main() {
    Queue<int> queue;

    // Enqueue elements
    queue.enqueue(10);
    queue.enqueue(20);
    cout << "Queue after enqueuing 10 and 20: " << queue.peek() << endl;
    cout << "Is queue empty? " << (queue.isEmpty() ? "Yes" : "No") << endl;

    // Peek at the front element
    int frontElement = queue.peek();
    cout << "Front element (peek): " << frontElement << endl;

    // Dequeue an element
    int dequeuedElement = queue.dequeue();
    cout << "Dequeued element: " << dequeuedElement << endl;
    cout << "Queue after dequeuing: " << queue.peek() << endl;
    cout << "Is queue empty? " << (queue.isEmpty() ? "Yes" : "No") << endl;

    // Additional tests
    queue.enqueue(30);
    queue.enqueue(40);
    cout << "Queue after enqueuing 30 and 40: " << queue.peek() << endl;

    dequeuedElement = queue.dequeue();
    cout << "Dequeued element: " << dequeuedElement << endl;
    cout << "Queue after dequeuing: " << queue.peek() << endl;

    dequeuedElement = queue.dequeue();
    cout << "Dequeued element: " << dequeuedElement << endl;
    cout << "Queue after dequeuing: " << queue.peek() << endl;

    dequeuedElement = queue.dequeue();
    cout << "Dequeued element: " << dequeuedElement << endl;
    cout << "Is queue empty? " << (queue.isEmpty() ? "Yes" : "No") << endl;

    return 0;
}