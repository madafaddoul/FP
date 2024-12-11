#include <iostream>
#include <unordered_map>
#include <vector>
#include <algorithm>

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

    // Update the value associated with a key in the HashMap
    void update(const std::string& key, const std::string& value) {
        insert(key, value);
    }

    // Merge two HashMaps
    void merge(const HashMap& other) {
        for (const auto& pair : other.map) {
            if (map.find(pair.first) == map.end()) {
                map[pair.first] = pair.second;
            }
        }
    }

    // Retrieve a list of all keys in the HashMap
    std::vector<std::string> keys() const {
        std::vector<std::string> keys;
        for (const auto& pair : map) {
            keys.push_back(pair.first);
        }
        return keys;
    }

    // Retrieve a list of all values in the HashMap
    std::vector<std::string> values() const {
        std::vector<std::string> values;
        for (const auto& pair : map) {
            values.push_back(pair.second);
        }
        return values;
    }

    // Get the number of key-value pairs in the HashMap
    size_t size() const {
        return map.size();
    }

    // // Apply a function to all values in the HashMap
    // void mapValues(const std::function<std::string(const std::string&)>& func) {
    //     for (auto& pair : map) {
    //         pair.second = func(pair.second);
    //     }
    // }

private:
    std::unordered_map<std::string, std::string> map;
};
