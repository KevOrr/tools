#!/usr/bin/env python3

import unittest
from btree import BTree

class TestBTreeSearch(unittest.TestCase):
    def setUp(self):
        self.btree = BTree(5)

        empty_leaf = [None, None, None, None, None]
        self.btree._root = (1, [4, 6, 11, 20],
                            [(2, [1, 2, None, None], empty_leaf[:]),
                             (2, [5, None, None, None], empty_leaf[:]),
                             (2, [7, 8, 9, 10], empty_leaf[:]),
                             (2, [12, 14, 15, 16], empty_leaf[:]),
                             (2, [None, None, None, None], empty_leaf[:])])

    def test_find_in_keys(self):
        for i in (4, 6, 11, 20):
            self.assertEqual((True, i), self.btree._find(i))
            self.assertEqual(i, self.btree[i])

    def test_find_in_children(self):
        for i in (1, 2, 5, 7, 8, 9, 10, 12, 14, 15, 16):
            self.assertEqual((True, i), self.btree._find(i))
            self.assertEqual(i, self.btree[i])

    def test_find_nonexistant(self):
        for i in (3, 13, 17, 21, 22):
            self.assertEqual(False, self.btree._find(i)[0])
            self.assertRaises(KeyError, lambda: self.btree[i])

if __name__ == '__main__':
    unittest.main()
