#!/usr/bin/env python3

import operator

class BTree():
    _EMPTY_KEY = object()
    _MAX_KEY = object()

    _INTERNAL_NODE_TYPE = object()
    _LEAF_TYPE = object()

    def __init__(self, order, gt_func=operator.gt, eq_func=operator.eq):
        if bin(order - 1)[2:].count('1') != 1:
            e = NotImplementedError('Order must be 1 + 2**n, for some integer n (for now anyway)')
            raise e

        self._root = [BTree._LEAF_TYPE,
                      [[BTree._EMPTY_KEY, None]]*(order - 1) + [[BTree._MAX_KEY, None]]]

        self._gt = gt_func
        self._eq = eq_func

        self._order = order
        self._min_keys = (order - 1) / 2
        self._max_keys = order - 1

        self._height = 1
        self._count = 0

    @property
    def order(self):
        return self._order

    @property
    def height(self):
        return self._height

    def __len__(self):
        return self._count

    def _find(self, value, root):
        '''BTree._find(self, value, root) => (found, item)'''

        if root[0] is BTree._INTERNAL_NODE_TYPE:
            for key, node_less in root[1]:
                if key is BTree._EMPTY_KEY:
                    return (False, None)

                elif key is BTree._MAX_KEY or self._gt(key, value):
                    return self._find(value, node_less)

        elif root[0] is BTree._LEAF_TYPE:
            for item in root[1]:
                if item is BTree._EMPTY_KEY or self._gt(item, value):
                    return (False, None)

                elif self._eq(item, value):
                    return (True, item)

            return (False, None)


        assert False
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
        keys = root[1]

        if root[0] is BTree._LEAF_TYPE:
            for i, (key, node_less) in enumerate(keys):
                if self._eq(key, value):
                    return False

                elif key is BTree._EMPTY_KEY:
                    keys[i] = [value, None]

                elif key is BTree._MAX_KEY:
                    pass

                elif self._gt(key, value):
                    keys[:] = keys[:i] + [[value, None]] + keys[i:]

        elif root[0] is BTree._INTERNAL_NODE_TYPE:
            children[:] = children[:i] + [None] + children[i+1:]

            if keys[-1] is None:
                keys[:] = keys[:-1]
