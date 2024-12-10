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

void testMapValues() {
    HashMap hm;
    hm.fromList({{"key1", "value1"}, {"key2", "value2"}});
    hm.mapValues([](const std::string& value) { return value + "_updated"; });
    if (hm.lookup("key1") == "value1_updated" && hm.lookup("key2") == "value2_updated") {
        std::cout << "testMapValues passed" << std::endl;
    } else {
        std::cout << "testMapValues failed" << std::endl;
    }
}

int main() {
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
    testMapValues();
    return 0;
}