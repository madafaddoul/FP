#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <chrono>
#include <queue>
#include <algorithm>

class Graph {
private:
    std::unordered_map<int, std::unordered_set<int>> adjacencyList;

public:
    // Add a vertex to the graph
    void addVertex(int vertex) {
        if (adjacencyList.find(vertex) == adjacencyList.end()) {
            adjacencyList[vertex] = std::unordered_set<int>();
        }
    }

    // Add an edge to the graph (directed)
    void addEdge(int from, int to) {
        addVertex(from); // Ensure vertices exist
        addVertex(to);
        adjacencyList[from].insert(to);
    }

    // Get a list of all vertices
    std::vector<int> getVertices() const {
        std::vector<int> vertices;
        for (const auto& pair : adjacencyList) {
            vertices.push_back(pair.first);
        }
        return vertices;
    }

    // Get a list of all edges
    std::vector<std::pair<int, int>> getEdges() const {
        std::vector<std::pair<int, int>> edges;
        for (const auto& pair : adjacencyList) {
            for (int neighbor : pair.second) {
                edges.emplace_back(pair.first, neighbor);
            }
        }
        return edges;
    }

    // Check if a vertex exists
    bool vertexExists(int vertex) const {
        return adjacencyList.find(vertex) != adjacencyList.end();
    }

    // Check if an edge exists
    bool edgeExists(int from, int to) const {
        auto it = adjacencyList.find(from);
        if (it != adjacencyList.end()) {
            return it->second.find(to) != it->second.end();
        }
        return false;
    }

    // Remove a vertex from the graph
    void removeVertex(int vertex) {
        adjacencyList.erase(vertex);
        for (auto& pair : adjacencyList) {
            pair.second.erase(vertex);
        }
    }

    // Remove an edge from the graph
    void removeEdge(int from, int to) {
        auto it = adjacencyList.find(from);
        if (it != adjacencyList.end()) {
            it->second.erase(to);
        }
    }

    // Get neighbors of a vertex
    std::vector<int> neighbors(int vertex) const {
        std::vector<int> result;
        auto it = adjacencyList.find(vertex);
        if (it != adjacencyList.end()) {
            result.insert(result.end(), it->second.begin(), it->second.end());
        }
        return result;
    }

    // Get in-degree of a vertex
    int inDegree(int vertex) const {
        int degree = 0;
        for (const auto& pair : adjacencyList) {
            if (pair.second.find(vertex) != pair.second.end()) {
                degree++;
            }
        }
        return degree;
    }

    // Get out-degree of a vertex
    int outDegree(int vertex) const {
        auto it = adjacencyList.find(vertex);
        if (it != adjacencyList.end()) {
            return it->second.size();
        }
        return 0;
    }

    // Check if the graph is empty
    bool isEmpty() const {
        return adjacencyList.empty();
    }

    // Get graph size information
    std::pair<int, int> graphInfo() const {
        int numVertices = adjacencyList.size();
        int numEdges = 0;
        for (const auto& pair : adjacencyList) {
            numEdges += pair.second.size();
        }
        return {numVertices, numEdges};
    }

    // Merge another graph into this one
    void mergeGraph(const Graph& other) {
        for (const auto& pair : other.adjacencyList) {
            addVertex(pair.first);
            for (int neighbor : pair.second) {
                addEdge(pair.first, neighbor);
            }
        }
    }

    // Check if the graph is connected
    bool isConnected() const {
        if (adjacencyList.empty()) return true;
        std::unordered_set<int> visited;
        std::queue<int> q;
        int startVertex = adjacencyList.begin()->first;
        q.push(startVertex);
        visited.insert(startVertex);

        while (!q.empty()) {
            int current = q.front();
            q.pop();

            for (int neighbor : neighbors(current)) {
                if (visited.insert(neighbor).second) {
                    q.push(neighbor);
                }
            }
        }

        return visited.size() == adjacencyList.size();
    }

    // Check if the graph has a cycle
    bool hasCycleUtil(int vertex, std::unordered_set<int>& visited, std::unordered_set<int>& recStack) const {
        if (!visited.insert(vertex).second) {
            return false;
        }
        recStack.insert(vertex);

        for (int neighbor : neighbors(vertex)) {
            if (recStack.find(neighbor) != recStack.end() || hasCycleUtil(neighbor, visited, recStack)) {
                return true;
            }
        }

        recStack.erase(vertex);
        return false;
    }

    bool hasCycle() const {
        std::unordered_set<int> visited;
        std::unordered_set<int> recStack;
        for (const auto& pair : adjacencyList) {
            if (!visited.count(pair.first)) {
                if (hasCycleUtil(pair.first, visited, recStack)) {
                    return true;
                }
            }
        }
        return false;
    }

    // Find a path between two vertices without using std::optional
    std::vector<int> findPath(int start, int end) const {
        if (!vertexExists(start) || !vertexExists(end)) {
            return {}; // Return empty vector if vertices don't exist
        }
        std::unordered_map<int, int> cameFrom;
        std::queue<int> q;
        q.push(start);
        cameFrom[start] = start;

        while (!q.empty()) {
            int current = q.front();
            q.pop();

            if (current == end) {
                std::vector<int> path;
                while (current != start) {
                    path.push_back(current);
                    current = cameFrom[current];
                }
                path.push_back(start);
                std::reverse(path.begin(), path.end());
                return path;
            }

            for (int neighbor : neighbors(current)) {
                if (cameFrom.find(neighbor) == cameFrom.end()) {
                    cameFrom[neighbor] = current;
                    q.push(neighbor);
                }
            }
        }
        return {}; // Return empty vector if no path found
    }

    // Get all paths between two vertices
    void getAllPathsUtil(int current, int end, std::unordered_set<int>& visited,
                         std::vector<int>& path, std::vector<std::vector<int>>& allPaths) const {
        visited.insert(current);
        path.push_back(current);

        if (current == end) {
            allPaths.push_back(path);
        } else {
            for (int neighbor : neighbors(current)) {
                if (!visited.count(neighbor)) {
                    getAllPathsUtil(neighbor, end, visited, path, allPaths);
                }
            }
        }

        path.pop_back();
        visited.erase(current);
    }

    std::vector<std::vector<int>> getAllPaths(int start, int end) const {
        std::vector<std::vector<int>> allPaths;
        if (!vertexExists(start) || !vertexExists(end)) {
            return allPaths;
        }
        std::unordered_set<int> visited;
        std::vector<int> path;
        getAllPathsUtil(start, end, visited, path, allPaths);
        return allPaths;
    }
};

// Utility function to measure execution time
template <typename Func>
void timeIt(const std::string& message, Func func) {
    auto start = std::chrono::high_resolution_clock::now();
    func();
    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> duration = end - start;
    std::cout << message << ": " << duration.count() << " sec\n";
}

int main() {

    timeIt("Complete Graph Test Suite", []() {
        Graph g;
        
        timeIt("Adding 100000 vertices", [&]() {
            for (int i = 1; i <= 100000; i++) {
                g.addVertex(i);
            }
        });

        timeIt("Adding 500000 random edges", [&]() {
            for (int i = 0; i < 500000; i++) {
                g.addEdge(rand() % 100000 + 1, rand() % 100000 + 1);
            }
        });

        auto info = g.graphInfo();
        std::cout << "Graph size - Vertices: " << info.first << ", Edges: " << info.second << "\n";

        timeIt("Testing 10000 vertex lookups", [&]() {
            for (int i = 0; i < 10000; i++) {
                g.vertexExists(rand() % 150000);
            }
        });

        timeIt("Testing 10000 edge lookups", [&]() {
            for (int i = 0; i < 10000; i++) {
                g.edgeExists(rand() % 100000 + 1, rand() % 100000 + 1);
            }
        });

        timeIt("Finding 100 random paths", [&]() {
            for (int i = 0; i < 100; i++) {
                g.findPath(rand() % 100000 + 1, rand() % 100000 + 1);
            }
        });
        });
        
    timeIt("Graph Operations", []() {
        Graph g;

        g.addVertex(1);
        g.addVertex(2);
        g.addEdge(1, 2);

        // Print vertices
        auto vertices = g.getVertices();
        std::cout << "Vertices: ";
        for (int v : vertices) std::cout << v << " ";
        std::cout << "\n";

        // Print edges
        auto edges = g.getEdges();
        std::cout << "Edges: ";
        for (const auto& edge : edges) std::cout << "(" << edge.first << ", " << edge.second << ") ";
        std::cout << "\n";

        // Check vertex existence
        std::cout << "Vertex 1 exists: " << (g.vertexExists(1) ? "True" : "False") << "\n";
        std::cout << "Vertex 3 exists: " << (g.vertexExists(3) ? "True" : "False") << "\n";

        // Check edge existence
        std::cout << "Edge (1, 2) exists: " << (g.edgeExists(1, 2) ? "True" : "False") << "\n";
        std::cout << "Edge (2, 1) exists: " << (g.edgeExists(2, 1) ? "True" : "False") << "\n";

        // Test in-degree
        std::cout << "In-Degree of 2: " << g.inDegree(2) << "\n"; // Should print 1
        std::cout << "In-Degree of 1: " << g.inDegree(1) << "\n"; // Should print 0

        // Test out-degree
        std::cout << "Out-Degree of 1: " << g.outDegree(1) << "\n"; // Should print 1
        std::cout << "Out-Degree of 2: " << g.outDegree(2) << "\n"; // Should print 0

        // Test hasCycle
        std::cout << "Graph has cycle: " << (g.hasCycle() ? "True" : "False") << "\n"; // Should print False

        // Test isConnected
        std::cout << "Graph is connected: " << (g.isConnected() ? "True" : "False") << "\n"; // Should print True

        // Test getAllPaths
        auto allPaths = g.getAllPaths(1, 2);
        std::cout << "All paths from 1 to 2:\n";
        for (const auto& path : allPaths) {
            for (int v : path) std::cout << v << " ";
            std::cout << "\n";
        }

        // Create a cycle and test hasCycle
        g.addEdge(2, 1);
        std::cout << "After adding edge (2,1):\n";
        std::cout << "Graph has cycle: " << (g.hasCycle() ? "True" : "False") << "\n"; // Should print True

        // Test removeEdge
        g.removeEdge(2, 1);
        std::cout << "After removing edge (2,1):\n";
        std::cout << "Edge (2,1) exists: " << (g.edgeExists(2, 1) ? "True" : "False") << "\n"; // Should print False
        std::cout << "Graph has cycle: " << (g.hasCycle() ? "True" : "False") << "\n"; // Should print False

        // Test removeVertex
        g.removeVertex(2);
        std::cout << "After removing vertex 2:\n";
        std::cout << "Vertex 2 exists: " << (g.vertexExists(2) ? "True" : "False") << "\n"; // Should print False
        std::cout << "Graph is connected: " << (g.isConnected() ? "True" : "False") << "\n"; // Should print True

        // Test mergeGraph
        Graph g2;
        g2.addVertex(3);
        g2.addEdge(1, 3);
        g.mergeGraph(g2);
        std::cout << "After merging with g2:\n";
        auto verticesMerged = g.getVertices(); // Renamed variable
        std::cout << "Vertices: ";
        for (int v : verticesMerged) std::cout << v << " ";
        std::cout << "\n";

        auto edgesMerged = g.getEdges(); // Renamed variable
        std::cout << "Edges: ";
        for (const auto& edge : edgesMerged) std::cout << "(" << edge.first << ", " << edge.second << ") ";
        std::cout << "\n";

        // Test graphInfo
        auto info = g.graphInfo();
        std::cout << "Graph Info - Vertices: " << info.first << ", Edges: " << info.second << "\n"; // Should print appropriate counts

        // Test isEmpty
        std::cout << "Graph is empty: " << (g.isEmpty() ? "True" : "False") << "\n"; // Should print False
    });

    return 0;
}
