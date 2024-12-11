# Data Structures Library Documentation

# Data Structures Library Documentation

A comprehensive implementation of core data structures in C++ and Haskell, providing both imperative and functional programming paradigms. This library is designed to aid in understanding the implementation details of fundamental data structures and to serve as a resource for both learning and practical application.

## Project Structure

```
```
.
├── stacks/
│   ├── ImperativeStack.cpp       # C++ implementation of Stack
│   ├── Stack.hs                  # Haskell implementation of Stack
│   ├── imperative_stack_test.cpp # C++ tests for Stack
│   └── StackTest.hs              # Haskell tests for Stack
├── queues/
│   ├── ImperativeQueue.cpp       # C++ implementation of Queue
│   ├── Queue.hs                  # Haskell implementation of Queue
│   ├── imperative_queue_test.cpp # C++ tests for Queue
│   └── QueueTest.hs              # Haskell tests for Queue
├── linked_list/
│   ├── cpp_linkedlist.cpp        # C++ implementation of Linked List
│   ├── haskel_LinkedList.hs      # Haskell implementation of Linked List
├── tree/
│   ├── cpp_tree.cpp              # C++ implementation of Tree
│   ├── haskel_Tree.hs            # Haskell implementation of Tree
├── graph/
│   ├── Graph.cpp                 # C++ implementation of Graph and tests for Graph
│   ├── Graph.hs                  # Haskell implementation of Graph
│   └── GraphTest.hs              # Haskell tests for Graph
├── hashmap/
│   ├── HashMap.cpp               # C++ implementation of HashMap
│   ├── HashMap.hs                # Haskell implementation of HashMap
│   ├── HashMapTest.cpp           # C++ tests for HashMap
│   └── HashMapTest.hs            # Haskell tests for HashMap
└── README.md
```
└── README.md
```

## Getting Started

### Prerequisites

- **GHC (Glasgow Haskell Compiler)** ≥ 8.8
- **C++ Compiler** supporting C++17
- **Make** (optional)

### Building and Running

#### C++ Implementations

Build the C++ implementations using a C++17-compliant compiler:

```bash
# Build the Stack implementation
g++ -std=c++17 -o stack stacks/ImperativeStack.cpp

# Build and run Stack tests
g++ -std=c++17 -o stack_test stacks/tests/ImperativeStackTest.cpp
./stack_test
```

#### Haskell Implementations

Compile the Haskell implementations using GHC:

```bash
# Compile the Stack implementation
ghc -o stack stacks/Stack.hs

# Run Stack tests
runhaskell stacks/tests/StackTest.hs
```

## API Documentation

### 1. Stack<T>

#### Description

A stack is a linear data structure that follows the Last-In-First-Out (LIFO) principle. This implementation provides core stack functionalities with additional utility methods.

#### Methods

- **push(T value)**: Add an element to the top of the stack.
- **T pop()**: Remove and return the element from the top of the stack.
- **T peek()**: Return the element at the top of the stack without removing it.
- **bool isEmpty()**: Check if the stack is empty.
- **size_t size()**: Return the number of elements in the stack.
- **void reverse()**: Reverse the elements in the stack.
- **std::vector<T> toList()**: Convert the stack to a vector/list.
- **void fromList(const std::vector<T>& list)**: Populate the stack from a vector/list.
- **void map(Func func)**: Apply a function to each element in the stack.

#### Usage Example (C++)

```cpp
#include "ImperativeStack.cpp"
#include <iostream>

int main() {
    Stack<int> stack;
    stack.push(10);
    stack.push(20);

    std::cout << "Top element: " << stack.peek() << std::endl; // Output: 20

    stack.pop();

    std::cout << "Stack size: " << stack.size() << std::endl; // Output: 1

    return 0;
}
```

### 2. Queue<T>

#### Description

A queue is a linear data structure that follows the First-In-First-Out (FIFO) principle. This implementation supports basic queue operations along with utility methods.

#### Methods

- **void enqueue(T value)**: Add an element to the end of the queue.
- **T dequeue()**: Remove and return the element from the front of the queue.
- **T peek()**: Return the element at the front of the queue without removing it.
- **bool isEmpty()**: Check if the queue is empty.
- **size_t size()**: Return the number of elements in the queue.
- **void reverse()**: Reverse the elements in the queue.
- **std::vector<T> toList()**: Convert the queue to a vector/list.
- **void fromList(const std::vector<T>& list)**: Populate the queue from a vector/list.
- **void map(Func func)**: Apply a function to each element in the queue.

#### Usage Example (Haskell)

```haskell
import Queue

main = do
    let queue = enqueue 10 $ enqueue 20 emptyQueue
    print $ peek queue         -- Output: Just 10
    let (Just (value, queue')) = dequeue queue
    print value                -- Output: 10
    print $ size queue'        -- Output: 1
```

### 3. LinkedList<T>

#### Description

A linked list is a linear data structure where each element contains a reference to the next (and optionally previous) element. In this library, both singly and doubly linked lists are implemented.

#### Methods

- **void add(T value)**: Add an element to the list.
- **void remove(T value)**: Remove an element from the list.
- **bool contains(T value)**: Check if the list contains an element.
- **size_t size()**: Return the number of elements in the list.
- **void reverse()**: Reverse the elements in the list.
- **std::vector<T> toList()**: Convert the linked list to a vector/list.

### 4. Tree<T>

#### Description

A binary tree is a hierarchical data structure in which each node has at most two children. This implementation includes traversal algorithms and basic tree operations.

#### Methods

- **void insert(T value)**: Insert a value into the tree.
- **bool search(T value)**: Search for a value in the tree.
- **void remove(T value)**: Remove a value from the tree.
- **void inOrderTraversal()**: Traverse the tree in-order.
- **void preOrderTraversal()**: Traverse the tree pre-order.
- **void postOrderTraversal()**: Traverse the tree post-order.

### 5. Graph<T>

#### Description

A graph is a collection of nodes connected by edges. This implementation uses an adjacency list representation and includes common graph algorithms.

#### Methods

- **void addVertex(T value)**: Add a vertex to the graph.
- **void addEdge(T from, T to)**: Add an edge between two vertices.
- **bool vertexExists(T value)**: Check if a vertex exists in the graph.
- **bool edgeExists(T from, T to)**: Check if an edge exists between two vertices.
- **void breadthFirstSearch(T start)**: Perform BFS traversal.
- **void depthFirstSearch(T start)**: Perform DFS traversal.

### 6. HashMap<Key, Value>

#### Description

A hash map is a data structure that maps keys to values for highly efficient lookups. This implementation handles collisions and supports dynamic resizing.

#### Methods

- **void insert(Key key, Value value)**: Insert a key-value pair.
- **Value lookup(Key key)**: Retrieve the value associated with a key.
- **void deleteKey(Key key)**: Remove a key-value pair.
- **bool containsKey(Key key)**: Check if a key exists.
- **size_t size()**: Get the number of key-value pairs.
- **std::vector<Key> keys()**: Get a list of all keys.
- **std::vector<Value> values()**: Get a list of all values.
- **void merge(const HashMap& other)**: Merge another hash map into this one.
- **void mapValues(Func func)**: Apply a function to each value.

## Implementation Details

### Imperative (C++)

- **Templates**: Used for generic data type support.
- **Exception Handling**: Ensures robustness in error scenarios.
- **Standard Library Containers**: Utilizes vectors, lists, and maps for efficient data management.

### Functional (Haskell)

- **Pure Functions**: Functions without side effects for predictable behavior.
- **Pattern Matching**: Simplifies code and improves readability.
- **Recursion**: Employed extensively for data processing.
- **Type Classes**: Leverage Haskell's type system for overloading and abstraction.

## Testing and Benchmarking

Each data structure includes a comprehensive test suite to ensure correctness and performance. Benchmarking tools are also included to evaluate efficiency.

### Running Tests

#### C++ Tests

```bash
# Build and run Stack tests
g++ -std=c++17 -o stack_test stacks/tests/ImperativeStackTest.cpp
./stack_test
```

#### Haskell Tests

```bash
# Run Stack tests
runhaskell stacks/tests/StackTest.hs
```

## Contributing

Contributions are welcome! Please follow the steps below:

1. **Fork** the repository.
2. **Clone** your forked repository.
3. **Create** a new branch for your feature (`git checkout -b feature/your-feature`).
4. **Commit** your changes (`git commit -am 'Add new feature'`).
5. **Push** to the branch (`git push origin feature/your-feature`).
6. **Open** a Pull Request.

Please ensure your code adheres to the project's coding standards and includes appropriate tests.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Acknowledgments

We want to thank our brilliant professor who gave us this project, challenging us and deepening our understanding of data structures and Git/GitHub. 
Kudos to you, Dr.
