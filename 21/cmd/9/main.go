package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"sort"

	log "github.com/sirupsen/logrus"
)

type coord struct {
	x, y int
}

func findLowPoints(grid map[coord]int) []coord {
	var out []coord
	for coordinate, value := range grid {
		if value == 9 {
			continue
		}
		for x := coordinate.x - 1; x <= coordinate.x+1; x++ {
			for y := coordinate.y - 1; y <= coordinate.y+1; y++ {
				// These are the diagonals and the cell itself, excluded in the brief
				if (x != coordinate.x && y != coordinate.y) || (x == coordinate.x && y == coordinate.y) {
					continue
				}
				adjval, ok := grid[coord{x, y}]
				// We're on an edge, skip
				if !ok {
					continue
				}

				log.Debugf("(%d, %d [%d]) adjacent to (%d, %d [%d])\n", x, y, adjval, coordinate.x, coordinate.y, value)
				if adjval <= value {
					log.Debugf("%d > %d, moving onto the next cell\n", adjval, value)
					goto next
				}
			}
		}
		log.Debugf("(%d, %d) is a minimum\n", coordinate.x, coordinate.y)
		out = append(out, coordinate)
	next:
	}

	return out
}

func findIsland(grid map[coord]int, coordinate coord) int {
	queue := []coord{coordinate}
	outCells := make(map[coord]bool)

	for len(queue) > 0 {
		current := queue[0]
		// You could definitely converge this loop and the one in
		// findLowPoints() if you could be bothered
		for x := current.x - 1; x <= current.x+1; x++ {
			for y := current.y - 1; y <= current.y+1; y++ {
				// These are the diagonals and the cell itself, excluded in the brief
				if (x != current.x && y != current.y) || (x == current.x && y == current.y) {
					continue
				}
				adjval, ok := grid[coord{x, y}]
				// We're on an edge, skip
				if !ok {
					continue
				}
				// 9s delineate islands
				if adjval == 9 {
					continue
				}
				// We've already visited this cell
				if outCells[coord{x, y}] {
					continue
				}
				outCells[coord{x, y}] = true
				queue = append(queue, coord{x, y})
			}
		}
		queue = queue[1:]
	}

	return len(outCells)
}

func partOne(grid map[coord]int, coordinates []coord) int {
	var out int

	for _, coordinate := range coordinates {
		out += grid[coordinate] + 1
	}

	return out
}

func partTwo(grid map[coord]int, coordinates []coord) int {
	var islands []int

	for _, coordinate := range coordinates {
		islands = append(islands, findIsland(grid, coordinate))
	}

	sort.Sort(sort.Reverse(sort.IntSlice(islands)))

	out := 1
	for _, island := range islands[:3] {
		out *= island
	}

	return out
}

func main() {
	input, err := os.Open(filepath.Join("data", "9", "input.txt"))

	if err != nil {
		log.Fatal(err)
	}

	reader := bufio.NewReader(input)
	scanner := bufio.NewScanner(reader)

	grid := make(map[coord]int)

	var lineNumber int
	for scanner.Scan() {
		line := scanner.Bytes()
		for x, b := range line {
			grid[coord{x: x, y: lineNumber}] = int(b - '0')
		}

		lineNumber++
	}

	lowPoints := findLowPoints(grid)
	fmt.Println(partOne(grid, lowPoints))
	fmt.Println(partTwo(grid, lowPoints))
}
