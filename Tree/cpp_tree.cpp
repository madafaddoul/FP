#include <iostream>
#include <vector>
#include <stdexcept>

using namespace std;

struct Node {
    int value;
    Node* left;
    Node* right;

    Node(int v) : value(v), left(nullptr), right(nullptr) {}
};

Node* insert(Node* root, int value) {
    if (!root) return new Node(value);
    Node* current = root;
    while (true) {
        if (value < current->value) {
            if (!current->left) {
                current->left = new Node(value);
                break;
            } else {
                current = current->left;
            }
        } else if (value > current->value) {
            if (!current->right) {
                current->right = new Node(value);
                break;
            } else {
                current = current->right;
            }
        } else {
            break; // Value already exists
        }
    }
    return root;
}

bool search(Node* root, int value) {
    Node* current = root;
    while (current) {
        if (value == current->value) return true;
        current = (value < current->value) ? current->left : current->right;
    }
    return false;
}

vector<int> inorder(Node* root) {
    vector<int> result;
    vector<Node*> stack;
    Node* current = root;
    while (!stack.empty() || current) {
        while (current) {
            stack.push_back(current);
            current = current->left;
        }
        current = stack.back();
        stack.pop_back();
        result.push_back(current->value);
        current = current->right;
    }
    return result;
}

vector<int> preorder(Node* root) {
    vector<int> result;
    if (!root) return result;
    vector<Node*> stack;
    stack.push_back(root);
    while (!stack.empty()) {
        Node* current = stack.back();
        stack.pop_back();
        result.push_back(current->value);
        if (current->right) stack.push_back(current->right);
        if (current->left) stack.push_back(current->left);
    }
    return result;
}

vector<int> postorder(Node* root) {
    vector<int> result;
    if (!root) return result;
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
    return result;
}

int findMax(Node* root) {
    if (!root) throw runtime_error("Tree is empty, no maximum value");
    Node* current = root;
    while (current->right) current = current->right;
    return current->value;
}

int findMin(Node* root) {
    if (!root) throw runtime_error("Tree is empty, no minimum value");
    Node* current = root;
    while (current->left) current = current->left;
    return current->value;
}

Node* deleteValue(Node* root, int value) {
    if (!root) return nullptr;
    if (value < root->value) {
        root->left = deleteValue(root->left, value);
    } else if (value > root->value) {
        root->right = deleteValue(root->right, value);
    } else {
        if (!root->left) {
            Node* rightChild = root->right;
            delete root;
            return rightChild;
        }
        if (!root->right) {
            Node* leftChild = root->left;
            delete root;
            return leftChild;
        }
        Node* minNode = root->right;
        while (minNode->left) minNode = minNode->left;
        root->value = minNode->value;
        root->right = deleteValue(root->right, minNode->value);
    }
    return root;
}

int height(Node* root) {
    if (!root) return 0;
    int leftHeight = height(root->left);
    int rightHeight = height(root->right);
    return 1 + max(leftHeight, rightHeight);
}

void mapTree(Node* root, int (*func)(int)) {
    if (!root) return;
    vector<Node*> stack;
    stack.push_back(root);
    while (!stack.empty()) {
        Node* current = stack.back();
        stack.pop_back();
        current->value = func(current->value);
        if (current->right) stack.push_back(current->right);
        if (current->left) stack.push_back(current->left);
    }
}

int foldTree(Node* root, int (*func)(int, int), int acc) {
    if (!root) return acc;
    vector<Node*> stack;
    stack.push_back(root);
    while (!stack.empty()) {
        Node* current = stack.back();
        stack.pop_back();
        acc = func(acc, current->value);
        if (current->right) stack.push_back(current->right);
        if (current->left) stack.push_back(current->left);
    }
    return acc;
}

void clearTree(Node* root) {
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
}

int main() {
    Node* root = nullptr;
    root = insert(root, 50);
    root = insert(root, 30);
    root = insert(root, 70);
    root = insert(root, 20);
    root = insert(root, 40);
    root = insert(root, 60);
    root = insert(root, 80);

    cout << "Inorder: ";
    vector<int> inorderResult = inorder(root);
    for (int val : inorderResult) cout << val << " ";
    cout << endl;

    cout << "Preorder: ";
    vector<int> preorderResult = preorder(root);
    for (int val : preorderResult) cout << val << " ";
    cout << endl;

    cout << "Postorder: ";
    vector<int> postorderResult = postorder(root);
    for (int val : postorderResult) cout << val << " ";
    cout << endl;

    cout << "Max: " << findMax(root) << endl;
    cout << "Min: " << findMin(root) << endl;

    cout << "Tree height: " << height(root) << endl;

    mapTree(root, [](int x) { return x * 2; });
    cout << "After map (x * 2), inorder: ";
    inorderResult = inorder(root);
    for (int val : inorderResult) cout << val << " ";
    cout << endl;

    int sum = foldTree(root, [](int acc, int x) { return acc + x; }, 0);
    cout << "Sum of all values in tree: " << sum << endl;

    root = deleteValue(root, 30);
    cout << "After deleting 30, inorder: ";
    inorderResult = inorder(root);
    for (int val : inorderResult) cout << val << " ";
    cout << endl;

    clearTree(root);
    return 0;
}
