# Data Structures Implementation Project

A comprehensive implementation of fundamental data structures in both imperative (C++) and functional (Haskell) paradigms. This project demonstrates the differences and similarities between these two programming approaches.

## Project Structure

```
.
├── stacks/
│   ├── ImperativeStack.cpp
│   ├── Stack.hs
│   └── tests/
│       ├── ImperativeStackTest.cpp
│       └── StackTest.hs
├── queues/
│   ├── ImperativeQueue.cpp
│   ├── Queue.hs
│   └── tests/
│       ├── ImperativeQueueTest.cpp
│       └── QueueTest.hs
├── linked-lists/
│   ├── ImperativeLinkedList.cpp
│   ├── LinkedList.hs
│   └── tests/
│       ├── ImperativeLinkedListTest.cpp
│       └── LinkedListTest.hs
├── trees/
│   ├── ImperativeTree.cpp
│   ├── Tree.hs
│   └── tests/
│       ├── ImperativeTreeTest.cpp
│       └── TreeTest.hs
├── graphs/
│   ├── ImperativeGraph.cpp
│   ├── Graph.hs
│   └── tests/
│       ├── ImperativeGraphTest.cpp
│       └── GraphTest.hs
├── hashmaps/
│   ├── ImperativeHashMap.cpp
│   ├── HashMap.hs
│   └── tests/
│       ├── ImperativeHashMapTest.cpp
│       └── HashMapTest.hs
└── README.md
```

## Features

Each data structure is implemented with both imperative and functional approaches, providing:

- Type-safe implementations
- Comprehensive test suites
- Performance benchmarking
- Clear documentation
- Common operations and utilities

### Implemented Data Structures

1. **Stack**
    - Core operations: push, pop, peek
    - Utility functions: isEmpty, size, reverse
    - Functional operations: map, toList, fromList

2. **Queue**
    - Core operations: enqueue, dequeue, peek
    - Additional utilities included

3. **Linked List**
    - Both singly and doubly linked implementations
    - Comprehensive node management

4. **Tree**
    - Binary tree implementation
    - Traversal algorithms
    - Balancing operations

5. **Graph**
    - Adjacency list representation
    - Common graph algorithms
    - Path finding implementations

6. **HashMap**
    - Efficient key-value storage
    - Collision handling
    - Dynamic resizing

## Getting Started

### Prerequisites
- GHC (Glasgow Haskell Compiler) ≥ 8.8
- C++ compiler supporting C++17
- Make (optional)

### Building

```bash
# For C++ implementations
g++ -std=c++17 -o stack stacks/ImperativeStack.cpp

# For Haskell implementations
ghc -o stack stacks/Stack.hs

# C++ tests
./stack_test

# Haskell tests
runhaskell test/StackTest.hs
```

## Implementation Details

### Imperative (C++)
- Template-based implementations for type flexibility
- Exception handling for error cases
- STL container usage for efficiency

### Functional (Haskell)
- Pure functional implementations
- Pattern matching for elegant solutions
- Type class instances for enhanced functionality

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- Data Structures and Algorithm textbooks references
- Functional Programming in Haskell course materials
- C++ Standard Template Library documentation