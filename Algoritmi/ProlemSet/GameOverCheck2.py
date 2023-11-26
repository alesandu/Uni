def build_segment_tree(size):
    return [0] * (4 * size)

def update(segment_tree, node, start, end, index, value):
    if start == end == index:
        segment_tree[node] = max(segment_tree[node], value)
        return
    mid = (start + end) // 2
    left_child = 2 * node + 1
    right_child = 2 * node + 2
    if index <= mid:
        update(segment_tree, left_child, start, mid, index, value)
    else:
        update(segment_tree, right_child, mid + 1, end, index, value)
    segment_tree[node] = max(segment_tree[left_child], segment_tree[right_child])

def query(segment_tree, node, start, end, left, right):
    if start > right or end < left:
        return 0
    if start >= left and end <= right:
        return segment_tree[node]
    mid = (start + end) // 2
    left_child = 2 * node + 1
    right_child = 2 * node + 2
    left_max = query(segment_tree, left_child, start, mid, left, right)
    right_max = query(segment_tree, right_child, mid + 1, end, left, right)
    return max(left_max, right_max)

def game_over_checker(n, m, moves):
    segment_tree = build_segment_tree(n)

    for i, (x, w, h) in enumerate(moves):
        if query(segment_tree, 0, 1, n, x, x + w - 1) + h > m:
            return i + 1
        for j in range(x, x + w):
            update(segment_tree, 0, 1, n, j, query(segment_tree, 0, 1, n, j, j) + h)

    return 0

# Example usage
n = 8
m = 16
moves = [(1, 3, 12), (6, 3, 3), (2, 5, 2), (8, 1, 5), (4, 2, 3), (2, 2, 2), (4, 3, 7)]

result = game_over_checker(n, m, moves)
print(result)
