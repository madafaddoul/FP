#include <iostream>
#include <vector>
#include <cstdlib>
#include <chrono>
#include "cpp_tree.cpp" 

using namespace std;
using namespace std::chrono;

Node* generateRandomTree(int numNodes) {
    Node* root = nullptr;
    for (int i = 0; i < numNodes; ++i) {
        root = insert(root, rand() % 1000);
    }
    return root;
}

vector<Node*> generateMultipleTrees(int numTrees, int numNodes) {
    vector<Node*> trees;
    for (int i = 0; i < numTrees; ++i) {
        trees.push_back(generateRandomTree(numNodes));
    }
    return trees;
}

int main() {
    srand(time(0));
    const int numTrees = 10;
    const int numNodes = 1000;

    vector<Node*> trees = generateMultipleTrees(numTrees, numNodes);

    for (int i = 0; i < numTrees; ++i) {
        Node* root = trees[i];

        // Measure time for foldTree
        auto start = high_resolution_clock::now();
        int sum = foldTree(root, [](int acc, int x) { return acc + x; }, 0);
        auto end = high_resolution_clock::now();
        cout << "Tree " << i + 1 << " - Fold tree time: " << duration_cast<microseconds>(end - start).count() << " microseconds" << endl;

        // Measure time for deleteValue
        start = high_resolution_clock::now();
        root = deleteValue(root, 30);
        end = high_resolution_clock::now();
        cout << "Tree " << i + 1 << " - Delete value time: " << duration_cast<microseconds>(end - start).count() << " microseconds" << endl;

        // Measure time for clearTree
        start = high_resolution_clock::now();
        clearTree(root);
        end = high_resolution_clock::now();
        cout << "Tree " << i + 1 << " - Clear tree time: " << duration_cast<microseconds>(end - start).count() << " microseconds" << endl;
    }

    return 0;
}