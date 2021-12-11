package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	log "github.com/sirupsen/logrus"
)

type coord struct {
	x, y int
}

type grid map[coord]int

func (g grid) String() string {
	keys := make([]coord, 0, len(g))
	var sb strings.Builder
	for k, _ := range g {
		keys = append(keys, k)
	}
	sort.Slice(keys, func(i, j int) bool {
		if keys[i].y == keys[j].y {
			return keys[i].x < keys[j].x
		}
		return keys[i].y < keys[j].y
	})

	for _, k := range keys {
		if k.y != 0 && k.x == 0 {
			sb.WriteString("\n")
		}
		sb.WriteString(fmt.Sprint(g[k]))
	}

	return sb.String()
}

func step(gd grid) (grid, int) {
	var toFlash []coord
	flashed := make(map[coord]bool)

	for coordinate, value := range gd {
		value++
		gd[coordinate] = value
		if value > 9 {
			toFlash = append(toFlash, coordinate)
		}
	}

	for len(toFlash) > 0 {
		flash := toFlash[0]
		toFlash = toFlash[1:]

		if _, ok := flashed[flash]; ok {
			continue
		}

		flashed[flash] = true
		gd[flash] = 0

		for x := flash.x - 1; x <= flash.x+1; x++ {
			for y := flash.y - 1; y <= flash.y+1; y++ {
				// the cell itself
				if x == flash.x && y == flash.y {
					continue
				}
				neighbourCoords := coord{x: x, y: y}
				adjval, ok := gd[neighbourCoords]
				// We're on an edge, skip
				if !ok {
					continue
				}
				if _, ok := flashed[neighbourCoords]; ok {
					continue
				}

				adjval++

				gd[neighbourCoords] = adjval

				if adjval > 9 {
					toFlash = append(toFlash, neighbourCoords)
				}
			}
		}
	}

	return gd, len(flashed)
}

func iterateStep(gd grid, iterations int) (int, int, grid) {
	var out int
	var allFlashed int
	log.Debugln(gd)
	for i := 0; i < iterations; i++ {
		var n int
		gd, n = step(gd)
		log.WithField("step", i+1).Debugln(gd)
		out += n
		log.WithField("step", i+1).Debugf("%d points flashed", n)
	}

	return out, allFlashed, gd
}

func main() {
	//log.SetLevel(log.DebugLevel)
	input, err := os.Open(filepath.Join("data", "11", "input.txt"))

	if err != nil {
		log.Fatal(err)
	}

	reader := bufio.NewReader(input)
	scanner := bufio.NewScanner(reader)

	grid := make(grid)

	var lineNumber int
	for scanner.Scan() {
		line := scanner.Bytes()
		for x, b := range line {
			grid[coord{x: x, y: lineNumber}] = int(b - '0')
		}

		lineNumber++
	}

	const steps = 100
	ans1, allFlashed, grid := iterateStep(grid, steps)
	var ans2 int

	if allFlashed != 0 {
		ans2 = allFlashed
	} else {
		for i := steps; ; i++ {
			var flashed int
			grid, flashed = step(grid)
			if flashed == len(grid) {
				ans2 = i + 1
				break
			}
		}
	}

	fmt.Println(ans1)
	fmt.Println(ans2)
}
