import Test.HUnit
import qualified AES

testFips = TestCase (assertEqual "test fips output" AES.fipsExpected (AES.encode $ AES.fips))
testNist = TestCase (assertEqual "test nist output" AES.nistExpected (AES.encode $ AES.nist))
testTest = TestCase (assertEqual "test test output" AES.testExpected (AES.encode $ AES.test))

key1 = "00000000000000000000000000000000"
key1Schedule = ["00000000000000000000000000000000",
                "62636363626363636263636362636363",
                "9b9898c9f9fbfbaa9b9898c9f9fbfbaa",
                "90973450696ccffaf2f457330b0fac99",
                "ee06da7b876a1581759e42b27e91ee2b",
                "7f2e2b88f8443e098dda7cbbf34b9290",
                "ec614b851425758c99ff09376ab49ba7",
                "217517873550620bacaf6b3cc61bf09b",
                "0ef903333ba9613897060a04511dfa9f",
                "b1d4d8e28a7db9da1d7bb3de4c664941",
                "b4ef5bcb3e92e21123e951cf6f8f188e"]

key2 = "ffffffffffffffffffffffffffffffff"
key2Schedule = ["ffffffffffffffffffffffffffffffff",
                "e8e9e9e917161616e8e9e9e917161616",
                "adaeae19bab8b80f525151e6454747f0",
                "090e2277b3b69a78e1e7cb9ea4a08c6e",
                "e16abd3e52dc2746b33becd8179b60b6",
                "e5baf3ceb766d488045d385013c658e6",
                "71d07db3c6b6a93bc2eb916bd12dc98d",
                "e90d208d2fbb89b6ed5018dd3c7dd150",
                "96337366b988fad054d8e20d68a5335d",
                "8bf03f233278c5f366a027fe0e0514a3",
                "d60a3588e472f07b82d2d7858cd7c326"]

key3 = "000102030405060708090a0b0c0d0e0f"
key3Schedule = ["000102030405060708090a0b0c0d0e0f",
                "d6aa74fdd2af72fadaa678f1d6ab76fe",
                "b692cf0b643dbdf1be9bc5006830b3fe",
                "b6ff744ed2c2c9bf6c590cbf0469bf41",
                "47f7f7bc95353e03f96c32bcfd058dfd",
                "3caaa3e8a99f9deb50f3af57adf622aa",
                "5e390f7df7a69296a7553dc10aa31f6b",
                "14f9701ae35fe28c440adf4d4ea9c026",
                "47438735a41c65b9e016baf4aebf7ad2",
                "549932d1f08557681093ed9cbe2c974e",
                "13111d7fe3944a17f307a78b4d2b30c5"]

key4 = "6920e299a5202a6d656e636869746f2a"
key4Schedule = ["6920e299a5202a6d656e636869746f2a",
                "fa8807605fa82d0d3ac64e6553b2214f",
                "cf75838d90ddae80aa1be0e5f9a9c1aa",
                "180d2f1488d0819422cb6171db62a0db",
                "baed96ad323d173910f67648cb94d693",
                "881b4ab2ba265d8baad02bc36144fd50",
                "b34f195d096944d6a3b96f15c2fd9245",
                "a7007778ae6933ae0dd05cbbcf2dcefe",
                "ff8bccf251e2ff5c5c32a3e7931f6d19",
                "24b7182e7555e77229674495ba78298c",
                "ae127cdadb479ba8f220df3d4858f6b1"]

testSchedule1 = TestCase (assertEqual "test schedule 1" key1Schedule
                          (AES.split 32 $ AES.encode $ AES.keyExpansion $ AES.decode key1))
testSchedule2 = TestCase (assertEqual "test schedule 2" key2Schedule
                          (AES.split 32 $ AES.encode $ AES.keyExpansion $ AES.decode key2))
testSchedule3 = TestCase (assertEqual "test schedule 3" key3Schedule
                          (AES.split 32 $ AES.encode $ AES.keyExpansion $ AES.decode key3))
testSchedule4 = TestCase (assertEqual "test schedule 4" key4Schedule
                          (AES.split 32 $ AES.encode $ AES.keyExpansion $ AES.decode key4))

tests = TestList [TestLabel "test_fips" testFips,
                  TestLabel "test_nist" testNist,
                  TestLabel "test_test" testTest,
                  TestLabel "test_schedule_1" testSchedule1,
                  TestLabel "test_schedule_2" testSchedule2,
                  TestLabel "test_schedule_3" testSchedule3,
                  TestLabel "test_schedule_4" testSchedule4]

main = runTestTT tests
