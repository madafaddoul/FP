#include <iostream>
#include <chrono>
#include <vector>
#include <stdexcept>

using namespace std;

template <typename T>
class BinaryTree {
    struct Node {
        T value;
        Node* left;
        Node* right;
    };

    Node* root;
    int s;

public:
    BinaryTree() : root(nullptr), s(0) {}

    ~BinaryTree() {
        clear();
    }

    void insert(const T& value) {
        if (!root) {
            root = new Node{value, nullptr, nullptr};
        } else {
            Node* current = root;
            while (true) {
                if (value < current->value) {
                    if (current->left) {
                        current = current->left;
                    } else {
                        current->left = new Node{value, nullptr, nullptr};
                        break;
                    }
                } else if (value > current->value) {
                    if (current->right) {
                        current = current->right;
                    } else {
                        current->right = new Node{value, nullptr, nullptr};
                        break;
                    }
                } else {
                    break;
                }
            }
        }
        ++s;
    }

    bool search(const T& value) const {
        Node* current = root;
        while (current) {
            if (value == current->value) {
                return true;
            } else if (value < current->value) {
                current = current->left;
            } else {
                current = current->right;
            }
        }
        return false;
    }

    vector<T> inorder() const {
        vector<T> result;
        if (root) {
            vector<Node*> stack;
            Node* current = root;
            while (current || !stack.empty()) {
                while (current) {
                    stack.push_back(current);
                    current = current->left;
                }
                current = stack.back();
                stack.pop_back();
                result.push_back(current->value);
                current = current->right;
            }
        }
        return result;
    }

    vector<T> preorder() const {
        vector<T> result;
        if (root) {
            vector<Node*> stack;
            stack.push_back(root);
            while (!stack.empty()) {
                Node* current = stack.back();
                stack.pop_back();
                result.push_back(current->value);
                if (current->right) stack.push_back(current->right);
                if (current->left) stack.push_back(current->left);
            }
        }
        return result;
    }

    vector<T> postorder() const {
        vector<T> result;
        if (root) {
            vector<Node*> stack1, stack2;
            stack1.push_back(root);
            while (!stack1.empty()) {
                Node* current = stack1.back();
                stack1.pop_back();
                stack2.push_back(current);
                if (current->left) stack1.push_back(current->left);
                if (current->right) stack1.push_back(current->right);
            }
            while (!stack2.empty()) {
                result.push_back(stack2.back()->value);
                stack2.pop_back();
            }
        }
        return result;
    }

    T findMax() const {
        if (!root) throw runtime_error("Tree is empty, no maximum value");
        Node* current = root;
        while (current->right) {
            current = current->right;
        }
        return current->value;
    }

    T findMin() const {
        if (!root) throw runtime_error("Tree is empty, no minimum value");
        Node* current = root;
        while (current->left) {
            current = current->left;
        }
        return current->value;
    }

    void deleteValue(const T& value) {
        if (!root) return;
        Node* parent = nullptr;
        Node* current = root;
        while (current && current->value != value) {
            parent = current;
            if (value < current->value) {
                current = current->left;
            } else {
                current = current->right;
            }
        }
        if (!current) return;

        if (!current->left && !current->right) {
            if (current == root) {
                root = nullptr;
            } else if (parent->left == current) {
                parent->left = nullptr;
            } else {
                parent->right = nullptr;
            }
            delete current;
        } else if (!current->left) {
            if (current == root) {
                root = current->right;
            } else if (parent->left == current) {
                parent->left = current->right;
            } else {
                parent->right = current->right;
            }
            delete current;
        } else if (!current->right) {
            if (current == root) {
                root = current->left;
            } else if (parent->left == current) {
                parent->left = current->left;
            } else {
                parent->right = current->left;
            }
            delete current;
        } else {
            Node* successorParent = current;
            Node* successor = current->right;
            while (successor->left) {
                successorParent = successor;
                successor = successor->left;
            }
            current->value = successor->value;
            if (successorParent->left == successor) {
                successorParent->left = successor->right;
            } else {
                successorParent->right = successor->right;
            }
            delete successor;
        }
        --s;
    }

    int height() const {
        if (!root) return 0;
        vector<Node*> stack;
        stack.push_back(root);
        int maxHeight = 0;
        while (!stack.empty()) {
            Node* current = stack.back();
            stack.pop_back();
            int currentHeight = 1;
            Node* temp = current;
            while (temp->left || temp->right) {
                if (temp->left) {
                    temp = temp->left;
                } else {
                    temp = temp->right;
                }
                ++currentHeight;
            }
            if (currentHeight > maxHeight) {
                maxHeight = currentHeight;
            }
        }
        return maxHeight;
    }

    void clear() {
        if (!root) return;
        vector<Node*> stack;
        stack.push_back(root);
        while (!stack.empty()) {
            Node* current = stack.back();
            stack.pop_back();
            if (current->left) stack.push_back(current->left);
            if (current->right) stack.push_back(current->right);
            delete current;
        }
        root = nullptr;
        s = 0;
    }

    void printInorder() const {
        vector<T> result = inorder();
        for (const T& val : result) {
            cout << val << " ";
        }
        cout << endl;
    }

    void printPreorder() const {
        vector<T> result = preorder();
        for (const T& val : result) {
            cout << val << " ";
        }
        cout << endl;
    }

    void printPostorder() const {
        vector<T> result = postorder();
        for (const T& val : result) {
            cout << val << " ";
        }
        cout << endl;
    }
};

int main() {
    BinaryTree<int> tree;

    // Measure insertion time for 10K nodes
    auto start = chrono::high_resolution_clock::now();
    for (int i = 0; i < 10000; ++i) {
        tree.insert(rand() % 10000);
    }
    auto end = chrono::high_resolution_clock::now();
    cout << "Insertion Time for 10K nodes: " << chrono::duration_cast<chrono::milliseconds>(end - start).count() << "ms" << endl;

    // Measure search time for 10K nodes
    start = chrono::high_resolution_clock::now();
    for (int i = 0; i < 10000; ++i) {
        tree.search(rand() % 10000);
    }
    end = chrono::high_resolution_clock::now();
    cout << "Search Time for 10K nodes: " << chrono::duration_cast<chrono::milliseconds>(end - start).count() << "ms" << endl;

    // Measure deletion time for 10K nodes
    start = chrono::high_resolution_clock::now();
    for (int i = 0; i < 10000; ++i) {
        tree.deleteValue(rand() % 10000);
    }
    end = chrono::high_resolution_clock::now();
    cout << "Deletion Time for 10K nodes: " << chrono::duration_cast<chrono::milliseconds>(end - start).count() << "ms" << endl;

    return 0;
}