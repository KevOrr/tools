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

    def __str__(self):
        return self._to_str(self._root)

    def __getitem__(self, value):
        found, item = self._find(value, self._root)
        if found:
            return item
        else:
            e = KeyError(repr(value))
            raise e

    def insert(self, value):
        return self._insert(value, self._root)


    def _to_str(self, root):
        if root[0] is BTree._LEAF_TYPE:
            if self._count_keys(root[1]):
                s = '|' + '|'.join(str(item) for item in root[1] if item is not BTree._EMPTY_KEY) + '|'
                return '-'*len(s) + '\n' + s + '\n' + '-'*len(s)

        elif root[0] is BTree._INTERNAL_NODE_TYPE:
            lines = ['', '', '', '']
            for key, node_less in root[1]:
                if key is BTree._EMPTY_KEY:
                    break


                new_lines = self._to_str(node_less).split('\n')

                # Dunno why this doesn't work
                # lines[0] += '{:>{width}}'.format(key, width=len(new_lines[0]))

                lines[0] += '%*s' % (len(new_lines[0]) + 1, key if key is not BTree._MAX_KEY else '')
                lines[1:] = [a + b + ' ' for a,b in zip(lines[1:], new_lines)]

            return '\n'.join(lines)

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


    def _count_keys(self, lst):
        return len([x for x in lst if x is not BTree._EMPTY_KEY and x is not BTree._MAX_KEY])

    def _split_leaf(self, lst):
        left = lst[:self._min_keys] + [BTree._EMPTY_KEY]*self._min_keys
        right = lst[self._min_keys:]
        right += [BTree._EMPTY_KEY]*(self._max_keys + 1 - len(right))
        return ([BTree._LEAF_TYPE, left], [BTree._LEAF_TYPE, right])

    def _split_node(self, lst):
        center = lst[self._min_keys][0]

        left = lst[:self._min_keys]
        left += [[BTree._MAX_KEY, lst[self._min_keys][1]]]
        left += [[BTree._EMPTY_KEY, [BTree._EMPTY_KEY]*self._max_keys] for i in range(self._min_keys)]

        right = lst[self._min_keys+1:]
        right += [[BTree._EMPTY_KEY, [BTree._EMPTY_KEY]*self._max_keys] for i in range(self._max_keys + 1 - len(right))]

        return (center, [BTree._INTERNAL_NODE_TYPE, left], [BTree._INTERNAL_NODE_TYPE, right])

    def _get_smallest_value(self, node_or_leaf):
        if node_or_leaf[0] is BTree._INTERNAL_NODE_TYPE:
            return node_or_leaf[1][0][0]
        elif node_or_leaf[0] is BTree._LEAF_TYPE:
            return node_or_leaf[1][0]

    def _insert_into_node(self, new_child, root):
        value = self._get_smallest_value(new_child)

        for i, (key, node_less) in root[1]:
            assert key is not BTree._EMPTY_KEY

            if key is BTree._MAX_KEY or self._gt(key, value):
                return root[i:] + [value, new_child] + root[i:]


    # TODO test this
    def _insert(self, value, root):
        '''BTree._insert(self, value, root) => new_child'''

        if root[0] is BTree._INTERNAL_NODE_TYPE:
            for key, node_less in root[1]:
                assert key is not BTree._EMPTY_KEY

                if key is BTree._MAX_KEY or self._gt(key, value):
                    new_child = self._insert(value, node_less)
                    if new_child is not None:

                        # Still space left in current node, just insert
                        if self._count_keys(root) < self._max_keys:
                            root[:] = self._insert_into_node(new_child)

                        # No space left, need caller to save new_sibling
                        else:
                            inserted = self._insert_into_node(new_child, root)
                            root[:], new_sibling = self._split_node(root)

                            # Unless we're actually on the root node, so do something else
                            if root is self._root:
                                new_root = [BTree._INTERNAL_NODE_TYPE,
                                            [BTree._MAX_KEY, [BTree._LEAF_TYPE,
                                                              [BTree._EMPTY_KEY]*self._max_keys]]]
                                new_root = self._insert_into_node(root)
                                new_root = self._insert_into_node(new_sibling)
                                new_root += [[BTree._LEAF_TYPE, [BTree._EMPTY_KEY]*self._max_keys]
                                         for i in range(self._max_keys + 1 - len(new_root))]
                                self._root[:] = new_root
                                self._height += 1
                            else:
                                return new_sibling

                    self._count += 1
                    return

        elif root[0] is BTree._LEAF_TYPE:
            if self._count_keys(root) < self._max_keys:
                self._insert_leaf(self, value, root)
            else:
                root[:], new_child = self._split_leaf(root)
                return new_child

        assert False
