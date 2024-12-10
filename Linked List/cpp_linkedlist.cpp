#include <iostream>
#include <stdexcept>

using namespace std;

class LinkedList {
    struct Node {
        int value;  
        Node* next; 
    };

    Node* first; 
    int s;

public:
    LinkedList() {
        first = nullptr;
        s = 0;
    }

    // Add an element at the beginning of the linked list
    void addFirst(int x) { // O(1)
        Node* tmp = new Node; 
        tmp->value = x;
        tmp->next = first;
        first = tmp;
        ++s;
    }

    // Add an element at the end of the linked list
    void addLast(int x) {  // O(n)
        if (first == nullptr) {
            addFirst(x);
            return;
        }
        Node* p = first;
        while (p->next != nullptr) // traverse to the end of the list
            p = p->next;
        Node* tmp = new Node;
        tmp->value = x;
        tmp->next = nullptr;
        p->next = tmp;
        ++s;
    }

    // Insert an element at a given position in the linked list
    void insertAt(int x, int pos) {
        if (pos < 0 || pos > size()) {
            throw out_of_range("Index out of bounds");
        }
        if (pos == 0) {
            addFirst(x);
            return;
        }
        if (pos == size()) {
            addLast(x);
            return;
        }
        Node* p = first;
        for (int i = 1; i < pos; ++i) {
            p = p->next;
        }
        Node* tmp = new Node;
        tmp->value = x;
        tmp->next = p->next;
        p->next = tmp;
        ++s;
    }

    // Update the value of an element at a given position
    void updateAt(int index, int value) {
        if (index < 0 || index >= size()) throw out_of_range("Index out of bounds");
        Node* temp = first;
        for (int i = 0; i < index; ++i) {
            temp = temp->next;
        }
        temp->value = value;
    }

    // Retrieve the value of an element at a given position
    int elementAt(int index) {
        if (index < 0 || index >= size()) throw out_of_range("Index out of bounds");
        Node* temp = first;
        for (int i = 0; i < index; ++i) {
            temp = temp->next;
        }
        return temp->value;
    }

    // Concatenate two linked lists
    void concat(LinkedList& other) {
        if (first == nullptr) {
            first = other.first;
        } else {
            Node* p = first;
            while (p->next != nullptr) {
                p = p->next;
            }
            p->next = other.first;
        }
        s += other.s;
        other.first = nullptr;
        other.s = 0;
    }

    // Apply a function to each element in the list
    void map(void (*f)(int&)) {
        Node* temp = first;
        while (temp != nullptr) {
            f(temp->value);
            temp = temp->next;
        }
    }

    // Filter elements in the list based on a predicate
    LinkedList filter(bool (*p)(int)) {
        LinkedList result;
        Node* temp = first;
        while (temp != nullptr) {
            if (p(temp->value)) {
                result.addLast(temp->value);
            }
            temp = temp->next;
        }
        return result;
    }

    // Reduce the list to a single value using a binary function
    int fold(int (*f)(int, int), int acc) {
        Node* temp = first;
        while (temp != nullptr) {
            acc = f(acc, temp->value);
            temp = temp->next;
        }
        return acc;
    }

    // Combine two lists into a list of pairs
    LinkedList zip(LinkedList& other) {
        LinkedList result;
        Node* temp1 = first;
        Node* temp2 = other.first;
        while (temp1 != nullptr && temp2 != nullptr) {
            result.addLast(temp1->value);
            result.addLast(temp2->value);
            temp1 = temp1->next;
            temp2 = temp2->next;
        }
        return result;
    }

    // Combine two lists using a binary function
    LinkedList zipWith(int (*f)(int, int), LinkedList& other) {
        LinkedList result;
        Node* temp1 = first;
        Node* temp2 = other.first;
        while (temp1 != nullptr && temp2 != nullptr) {
            result.addLast(f(temp1->value, temp2->value));
            temp1 = temp1->next;
            temp2 = temp2->next;
        }
        return result;
    }

    // Delete the first occurrence of an element from the linked list
    void deleteValue(int value) {
        if (!first) return;
        if (first->value == value) {
            Node* tmp = first;
            first = first->next;
            delete tmp;
            --s;
            return;
        }
        Node* temp = first;
        while (temp->next && temp->next->value != value) {
            temp = temp->next;
        }
        if (temp->next) {
            Node* toDelete = temp->next;
            temp->next = temp->next->next;
            delete toDelete;
            --s;
        }
    }

    // Check if an element is in the linked list
    bool search(int value) {
        Node* temp = first;
        while (temp) {
            if (temp->value == value) return true;
            temp = temp->next;
        }
        return false;
    }

    // Convert the linked list to an array
    void toList(int* arr, int size) {
        Node* temp = first;
        for (int i = 0; i < size && temp; ++i) {
            arr[i] = temp->value;
            temp = temp->next;
        }
    }

    // Convert an array to a linked list
    void fromList(int* arr, int size) {
        clear();
        for (int i = 0; i < size; ++i) {
            addLast(arr[i]);
        }
    }

    // Calculate the length of the linked list
    int size() {
        return s;
    }

    // Reverse the linked list
    void reverse() {
        Node* prev = nullptr;
        Node* current = first;
        Node* next = nullptr;
        while (current) {
            next = current->next;
            current->next = prev;
            prev = current;
            current = next;
        }
        first = prev;
    }

    // Clear the linked list
    void clear() {
        while (first) {
            Node* temp = first;
            first = first->next;
            delete temp;
        }
        s = 0;
    }

    // Print the linked list
    void print() {
        Node* temp = first;
        while (temp) {
            cout << temp->value << " -> ";
            temp = temp->next;
        }
        cout << "nullptr" << endl;
    }
};

int main() {
    LinkedList list;
    list.addFirst(3);
    list.addFirst(2);
    list.addFirst(1);
    list.addLast(4);
    list.insertAt(5, 2);
    list.print(); 

    list.updateAt(2, 6);
    list.print(); 

    cout << "Element at index 2: " << list.elementAt(2) << endl; // 6

    list.deleteValue(6);
    list.print(); 

    cout << "Search for 3: " << (list.search(3) ? "Found" : "Not Found") << endl; // Found

    int arr[4];
    list.toList(arr, 4);
    for (int i = 0; i < 4; ++i) {
        cout << arr[i] << " ";
    }
    cout << endl;

    list.reverse();
    list.print();

    cout << "Length: " << list.size() << endl; 

    return 0;
}