#!/usr/bin/env python3

import unittest
from btree import BTree

class TestBTreeBase(unittest.TestCase):
    def setUp(self):
        self.btree = BTree(5)

        empty_leaf = [None, None, None, None, None]
        self.btree._root = [BTree._INTERNAL_NODE_TYPE, [
            [3, [BTree._LEAF_TYPE,
                 [1, 2, BTree._EMPTY_KEY, BTree._EMPTY_KEY]]],
            [7, [BTree._LEAF_TYPE,
                 [5, BTree._EMPTY_KEY, BTree._EMPTY_KEY, BTree._EMPTY_KEY,]]],
            [12, [BTree._LEAF_TYPE,
                  [8, 9, 10, 11]]],
            [20, [BTree._LEAF_TYPE,
                  [13, 14, 15, 16]]],
            [BTree._MAX_KEY, [BTree._LEAF_TYPE,
                              [25, 27, 31, BTree._EMPTY_KEY]]]]]

class TestBTreeSearch(TestBTreeBase):
    def test_find_in_children(self):
        for i in (1, 2, 5, 8, 9, 10, 11, 13, 14, 15, 16):
            self.assertEqual(i, self.btree[i])

    def test_find_in_max_node(self):
        for i in (25, 27, 31):
            self.assertEqual(i, self.btree[i])

    def test_find_raises_in_keys(self):
        for i in (3, 7, 12, 20):
            self.assertRaises(KeyError, lambda: self.btree[i])

    def test_find_raises_nonexistant(self):
        for i in (3, 17, 21, 22):
            self.assertRaises(KeyError, lambda: self.btree[i])

class TestBTreeInsertion(TestBTreeBase):
    def test_insert_plain(self):
        for i in (0, 4, 6):
            self.btree.insert(i)
            self.assertEqual(self.btree[i], i)

if __name__ == '__main__':
    unittest.main()
