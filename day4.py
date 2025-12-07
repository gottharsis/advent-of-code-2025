import sys

grid = [list(line) for line in sys.stdin.read().strip().splitlines()]
ROWS = len(grid)
COLS = len(grid[0])
NEIGH_8 = [(-1, -1),(-1, 0),(-1, 1),(0, 1),(1, 1),(1, 0),(1, -1),(0, -1)]

valids = 0

for r, row in enumerate(grid):
    for c, ch in enumerate(row):
        if ch != '@':
            continue
        num_neighs = 0
        for dr, dc in NEIGH_8:
            cr, cc = r + dr, c + dc
            if 0 <= cr < ROWS and 0 <= cc < COLS and grid[cr][cc] == '@':
                num_neighs += 1
        if num_neighs < 4:
            valids += 1

print(valids)
