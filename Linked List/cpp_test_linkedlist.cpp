#include <iostream>
#include <chrono>
#include "cpp_linkedlist.cpp" 

using namespace std;
using namespace std::chrono;

int main() {
    LinkedList list;

    // Add some elements to the list
    for (int i = 0; i < 1000; ++i) {
        list.addLast(i);
    }

    // Measure time for search
    auto start = high_resolution_clock::now();
    list.search(500);
    auto end = high_resolution_clock::now();
    cout << "Search time: " << duration_cast<microseconds>(end - start).count() << " microseconds" << endl;

    // Measure time for toList
    int arr[1000];
    start = high_resolution_clock::now();
    list.toList(arr, 1000);
    end = high_resolution_clock::now();
    cout << "ToList time: " << duration_cast<microseconds>(end - start).count() << " microseconds" << endl;

    // Measure time for reverse
    start = high_resolution_clock::now();
    list.reverse();
    end = high_resolution_clock::now();
    cout << "Reverse time: " << duration_cast<microseconds>(end - start).count() << " microseconds" << endl;


    return 0;
}