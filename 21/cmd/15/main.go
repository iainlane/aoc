package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"log"
	"os"
	"path/filepath"
)

type cell struct {
	value int
}

type grid [][]cell

type repeatingGrid struct {
	grid    grid
	repeats int
}

type coord struct {
	x, y int
}

func (r repeatingGrid) weight(c coord) int {
	size := len(r.grid)

	cost := r.grid[c.x%size][c.y%size].value
	cost += (c.x / size) + (c.y / size)

	if cost >= 10 {
		cost -= 9
	}

	return cost
}

func (c coord) String() string {
	return fmt.Sprintf("(%d, %d)", c.x, c.y)
}

func neighbours(g repeatingGrid, c coord) []coord {
	var n []coord
	for x := c.x - 1; x <= c.x+1; x++ {
		for y := c.y - 1; y <= c.y+1; y++ {
			// These are the diagonals and the cell itself, excluded in the brief
			if (x != c.x && y != c.y) || (x == c.x && y == c.y) {
				continue
			}
			if x < 0 || y < 0 || x >= len(g.grid)*g.repeats || y >= len(g.grid[0])*g.repeats {
				continue
			}
			n = append(n, coord{x, y})
		}
	}

	return n
}

func (grid repeatingGrid) shortestPath(start, end coord) int {
	pq := make(PriorityQueue, 1)
	pq[0] = &Item{
		value:    start,
		priority: 0,
		index:    0,
	}
	heap.Init(&pq)

	cameFrom := make(map[coord]coord)
	costSoFar := make(map[coord]int)
	cameFrom[start] = coord{-1, -1}
	costSoFar[start] = 0

	for pq.Len() > 0 {
		current := heap.Pop(&pq).(*Item)
		currentCoord := current.value
		if currentCoord == end {
			break
		}

		for _, next := range neighbours(grid, currentCoord) {
			newCost := costSoFar[currentCoord] + grid.weight(next)
			if _, ok := costSoFar[next]; !ok || newCost < costSoFar[next] {
				costSoFar[next] = newCost
				pq.Push(&Item{
					value:    next,
					priority: -newCost,
				})
				cameFrom[next] = currentCoord
			}
		}
	}

	risk := 0
	for {
		risk += grid.weight(end)
		end = cameFrom[end]
		if end == start {
			break
		}
	}

	return risk
}

func main() {
	input, err := os.Open(filepath.Join("data", "15", "input.txt"))

	if err != nil {
		log.Fatal(err)
	}

	reader := bufio.NewReader(input)
	scanner := bufio.NewScanner(reader)

	var grid grid

	var lineNumber int
	for scanner.Scan() {
		line := scanner.Bytes()
		len := len(line)
		grid = append(grid, make([]cell, len))
		for x, b := range line {
			grid[lineNumber][x] = cell{value: int(b - '0')}
		}

		lineNumber++
	}

	nonRepeatingGrid := repeatingGrid{
		grid:    grid,
		repeats: 1,
	}
	risk := nonRepeatingGrid.shortestPath(coord{0, 0}, coord{len(grid) - 1, len(grid[0]) - 1})
	fmt.Println(risk)

	nRepeats := 5
	repeatingGrid := repeatingGrid{
		grid:    grid,
		repeats: nRepeats,
	}
	risk2 := repeatingGrid.shortestPath(coord{0, 0}, coord{(len(grid) * nRepeats) - 1, (len(grid[0]) * nRepeats) - 1})
	fmt.Println(risk2)
}
