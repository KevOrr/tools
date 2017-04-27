#!/usr/bin/env python3

import operator

class BTree():
    _INTERNAL_NODE_TYPE = 1
    _LEAF_TYPE = 2

    def __init__(self, order, gt_func=operator.gt, eq_func=operator.eq):
        self._order = order
        self._root = (BTree._LEAF_TYPE, [None]*(order - 1), [None]*(order))
        self._height = 1
        self._gt = gt_func
        self._eq = eq_func

    @property
    def order(self):
        return self._order

    @property
    def height(self):
        return self._height

    def _find(self, value, root=-1):
        if root is None:
            return (False, None)

        if root == -1:
            root = self._root

        for i, (key, node_less) in enumerate(zip(root[1], root[2])):
            if self._eq(key, value):
                return (True, key)

            elif key is None or self._gt(key, value):
                if root[0] == BTree._LEAF_TYPE:
                    return (False, None)
                elif root[0] == BTree._INTERNAL_NODE_TYPE:
                    return self._find(value, root=node_less)

        # If not a leaf and search value > last key in this key, then search on rightmost child
        if root[0] == BTree._INTERNAL_NODE_TYPE and self._gt(value, key):
            return self._find(value, root=root[2][i+1])

        return (False, None)


    def __getitem__(self, value):
        pass

    def insert(self, value):
        for key in self._root:
            if key is None:
                pass
