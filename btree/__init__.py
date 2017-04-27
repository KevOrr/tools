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

    def _find(self, value, root):
        if root is None:
            return (False, None)

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
        found, item = self._find(value, self._root)
        if found:
            return item
        else:
            e = KeyError(repr(value))
            raise e

    def insert(self, value):
        return self._insert(value, self._root)

    def _insert(self, value, root):
        root_type = root[0]
        keys = root[1]
        children = root[2]

        for i, (key, node_less) in enumerate(zip(keys, children)):
            if self._eq(key, value):
                return False

            elif key is None:
                keys[i] = value

            elif self._gt(key, value):
                keys[:] = keys[:i] + [value] + keys[i+1:]

                if root_type == BTree._INTERNAL_NODE_TYPE:
                    children[:] = children[:i] + [None] + children[i+1:]

                if keys[-1] is None:
                    keys[:] = keys[:-1]

        # If not a leaf and search value > last key in this key, then search on rightmost child
        if root_type == BTree._INTERNAL_NODE_TYPE and self._gt(value, key):
            return self._find(value, root=children[i+1])

        return (False, None)
