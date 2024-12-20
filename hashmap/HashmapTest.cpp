#include "Hashmap.cpp"
#include <iostream>
#include <chrono>
#include <random>
#include <sstream>

// Helper function for timing measurements
template<typename F>
double measureTime(F func) {
    auto start = std::chrono::high_resolution_clock::now();
    func();
    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> diff = end - start;
    return diff.count();
}

// Generate random string of given length
std::string randomString(int length) {
    static const char alphanum[] =
        "0123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz";
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(0, sizeof(alphanum) - 2);
    std::string str;
    str.reserve(length);
    for (int i = 0; i < length; ++i) {
        str += alphanum[dis(gen)];
    }
    return str;
}

void testInsert() {
    HashMap hm;
    hm.insert("key1", "value1");
    if (hm.lookup("key1") == "value1") {
        std::cout << "testInsert passed" << std::endl;
    } else {
        std::cout << "testInsert failed" << std::endl;
    }
}

void testLookup() {
    HashMap hm;
    hm.insert("key1", "value1");
    hm.insert("key2", "value2");
    if (hm.lookup("key1") == "value1" && hm.lookup("key2") == "value2" && hm.lookup("key3") == "") {
        std::cout << "testLookup passed" << std::endl;
    } else {
        std::cout << "testLookup failed" << std::endl;
    }
}

void testDelete() {
    HashMap hm;
    hm.insert("key1", "value1");
    hm.insert("key2", "value2");
    hm.deleteKey("key1");
    if (hm.lookup("key1") == "" && hm.lookup("key2") == "value2") {
        std::cout << "testDelete passed" << std::endl;
    } else {
        std::cout << "testDelete failed" << std::endl;
    }
}

void testFromList() {
    HashMap hm;
    hm.fromList({{"key1", "value1"}, {"key2", "value2"}});
    if (hm.lookup("key1") == "value1" && hm.lookup("key2") == "value2") {
        std::cout << "testFromList passed" << std::endl;
    } else {
        std::cout << "testFromList failed" << std::endl;
    }
}

void testToList() {
    HashMap hm;
    hm.fromList({{"key1", "value1"}, {"key2", "value2"}});
    auto list = hm.toList();
    std::unordered_map<std::string, std::string> expected = {{"key1", "value1"}, {"key2", "value2"}};
    bool passed = true;
    for (const auto& pair : list) {
        if (expected[pair.first] != pair.second) {
            passed = false;
            break;
        }
    }
    if (passed) {
        std::cout << "testToList passed" << std::endl;
    } else {
        std::cout << "testToList failed" << std::endl;
    }
}

// Additional tests to verify purely functional behavior
void testPurelyFunctionalInsert() {
    HashMap hm;
    hm.insert("key1", "value1");
    HashMap hm2 = hm;
    hm.insert("key2", "value2");
    if (hm.lookup("key1") == "value1" && hm.lookup("key2") == "value2" && hm2.lookup("key2") == "") {
        std::cout << "testPurelyFunctionalInsert passed" << std::endl;
    } else {
        std::cout << "testPurelyFunctionalInsert failed" << std::endl;
    }
}

void testPurelyFunctionalDelete() {
    HashMap hm;
    hm.insert("key1", "value1");
    hm.insert("key2", "value2");
    HashMap hm2 = hm;
    hm.deleteKey("key1");
    if (hm.lookup("key1") == "" && hm.lookup("key2") == "value2" && hm2.lookup("key1") == "value1") {
        std::cout << "testPurelyFunctionalDelete passed" << std::endl;
    } else {
        std::cout << "testPurelyFunctionalDelete failed" << std::endl;
    }
}

// Tests for new operations
void testUpdate() {
    HashMap hm;
    hm.insert("key1", "value1");
    hm.update("key1", "newValue1");
    if (hm.lookup("key1") == "newValue1") {
        std::cout << "testUpdate passed" << std::endl;
    } else {
        std::cout << "testUpdate failed" << std::endl;
    }
}

void testMerge() {
    HashMap hm1;
    hm1.fromList({{"key1", "value1"}, {"key2", "value2"}});
    HashMap hm2;
    hm2.fromList({{"key2", "newValue2"}, {"key3", "value3"}});
    hm1.merge(hm2);
    if (hm1.lookup("key1") == "value1" && hm1.lookup("key2") == "value2" && hm1.lookup("key3") == "value3") {
        std::cout << "testMerge passed" << std::endl;
    } else {
        std::cout << "testMerge failed" << std::endl;
    }
}

void testKeys() {
    HashMap hm;
    hm.fromList({{"key1", "value1"}, {"key2", "value2"}});
    auto keys = hm.keys();
    if (keys == std::vector<std::string>{"key1", "key2"}) {
        std::cout << "testKeys passed" << std::endl;
    } else {
        std::cout << "testKeys failed" << std::endl;
    }
}

void testValues() {
    HashMap hm;
    hm.fromList({{"key1", "value1"}, {"key2", "value2"}});
    auto values = hm.values();
    if (values == std::vector<std::string>{"value1", "value2"}) {
        std::cout << "testValues passed" << std::endl;
    } else {
        std::cout << "testValues failed" << std::endl;
    }
}

void testSize() {
    HashMap hm;
    hm.fromList({{"key1", "value1"}, {"key2", "value2"}});
    if (hm.size() == 2) {
        std::cout << "testSize passed" << std::endl;
    } else {
        std::cout << "testSize failed" << std::endl;
    }
}


// New performance test functions
void testInsertPerformance() {
    std::cout << "\nTesting Insert Performance:" << std::endl;
    std::vector<int> sizes = {1000, 10000, 100000};
    
    for (int size : sizes) {
        HashMap hm;
        std::vector<std::pair<std::string, std::string>> data;
        // Prepare data
        for (int i = 0; i < size; i++) {
            data.push_back({randomString(10), randomString(10)});
        }
        
        double time = measureTime([&]() {
            for (const auto& pair : data) {
                hm.insert(pair.first, pair.second);
            }
        });
        
        std::cout << "Insert " << size << " elements: " << time << " seconds" << std::endl;
    }
}

void testLookupPerformance() {
    std::cout << "\nTesting Lookup Performance:" << std::endl;
    HashMap hm;
    std::vector<std::string> keys;
    
    // Insert test data
    for (int i = 0; i < 100000; i++) {
        std::string key = randomString(10);
        keys.push_back(key);
        hm.insert(key, randomString(10));
    }
    
    // Test different lookup patterns
    std::vector<int> lookupCounts = {1000, 10000, 100000};
    for (int count : lookupCounts) {
        double time = measureTime([&]() {
            for (int i = 0; i < count; i++) {
                hm.lookup(keys[i % keys.size()]);
            }
        });
        std::cout << count << " lookups: " << time << " seconds" << std::endl;
    }
}

void testBulkOperations() {
    std::cout << "\nTesting Bulk Operations Performance:" << std::endl;
    std::vector<int> sizes = {1000, 10000, 100000};
    
    for (int size : sizes) {
        std::vector<std::pair<std::string, std::string>> data;
        for (int i = 0; i < size; i++) {
            data.push_back({randomString(10), randomString(10)});
        }
        
        HashMap hm;
        double fromListTime = measureTime([&]() {
            hm.fromList(data);
        });
        
        double toListTime = measureTime([&]() {
            auto list = hm.toList();
        });
        
        std::cout << "Size " << size << ":" << std::endl;
        std::cout << "  fromList: " << fromListTime << " seconds" << std::endl;
        std::cout << "  toList: " << toListTime << " seconds" << std::endl;
    }
}

// Modified main function
int main() {
    // Run original tests
    std::cout << "Running functional tests:" << std::endl;
    testInsert();
    testLookup();
    testDelete();
    testFromList();
    testToList();
    testPurelyFunctionalInsert();
    testPurelyFunctionalDelete();
    testUpdate();
    testMerge();
    testKeys();
    testValues();
    testSize();
    
    // Run performance tests
    testInsertPerformance();
    testLookupPerformance();
    testBulkOperations();
    
    return 0;
}