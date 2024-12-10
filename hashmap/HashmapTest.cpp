#include "Hashmap.cpp"
#include <iostream>

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

int main() {
    testInsert();
    testLookup();
    testDelete();
    testFromList();
    testToList();
    return 0;
}