#include <iostream>
#include <unordered_map>
#include <vector>

class HashMap {
public:
    // Insert a key-value pair into the HashMap
    void insert(const std::string& key, const std::string& value) {
        map[key] = value;
    }

    // Lookup a value by key in the HashMap
    std::string lookup(const std::string& key) const {
        auto it = map.find(key);
        if (it != map.end()) {
            return it->second;
        }
        return "";
    }

    // Delete a key-value pair from the HashMap
    void deleteKey(const std::string& key) {
        map.erase(key);
    }

    // Convert a list of key-value pairs to a HashMap
    void fromList(const std::vector<std::pair<std::string, std::string>>& list) {
        for (const auto& pair : list) {
            map[pair.first] = pair.second;
        }
    }

    // Convert a HashMap to a list of key-value pairs
    std::vector<std::pair<std::string, std::string>> toList() const {
        std::vector<std::pair<std::string, std::string>> list;
        for (const auto& pair : map) {
            list.push_back(pair);
        }
        return list;
    }

private:
    std::unordered_map<std::string, std::string> map;
};
