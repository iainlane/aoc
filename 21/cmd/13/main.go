package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	log "github.com/sirupsen/logrus"
)

type grid [][]bool

func (g grid) String() string {
	var sb strings.Builder
	for y := 0; y < len(g); y++ {
		for x := 0; x < len(g[y]); x++ {
			cell := g[y][x]
			if cell {
				sb.WriteString("#")
			} else {
				sb.WriteString(".")
			}
		}
		if y < len(g)-1 {
			sb.WriteString("\n")
		}
	}
	return sb.String()
}

const (
	x = iota
	y = iota
)

type fold struct {
	direction  int
	coordinate int
}

func parseLine(line string, g grid, f []fold) (grid, []fold) {
	parts := strings.Split(line, " ")

	if parts[0] == "fold" {
		var direction int
		foldParts := strings.Split(parts[2], "=")
		switch foldParts[0] {
		case "x":
			direction = x
		case "y":
			direction = y
		default:
			log.Fatal("Unknown fold direction")
		}
		if coordinate, err := strconv.Atoi(foldParts[1]); err == nil {
			f = append(f, fold{direction, coordinate})
			return g, f
		} else {
			log.Fatal("Could not parse fold coordinate")
		}
	}

	coordinateParts := strings.SplitN(line, ",", 2)

	var x, y int

	var err error
	if x, err = strconv.Atoi(coordinateParts[0]); err != nil {
		log.Fatalf("Could not parse x coordinate '%s'", coordinateParts[0])
	}

	if y, err = strconv.Atoi(coordinateParts[1]); err != nil {
		log.Fatalf("Could not parse y coordinate '%s'", coordinateParts[1])
	}

	var ng grid
	if cap(g) < y+1 {
		ng = make(grid, y+1)
	} else {
		ng = make(grid, len(g))
	}

	xlen := x + 1
	if len(g) > 0 && len(g[0]) > xlen {
		xlen = len(g[0])
	}

	for i := range g {
		ng[i] = make([]bool, len(g[i]))
		copy(ng[i], g[i])
	}

	if len(ng[y]) < xlen {
		for n := 0; n < len(ng); n++ {
			ng[n] = append(ng[n], make([]bool, xlen-len(ng[n]))...)
		}
	}

	ng[y][x] = true

	return ng, f
}

func makeSmallerGrid(g grid, x int, y int) grid {
	ng := make(grid, y)
	for i := 0; i < y; i++ {
		ng[i] = make([]bool, x)
		copy(ng[i], g[i][0:x])
	}
	return ng
}

func foldGrid(g grid, f fold) grid {
	var ng grid
	switch f.direction {
	case x:
		nx := len(g[0]) - f.coordinate - 1
		ny := len(g)
		ng = makeSmallerGrid(g, nx, ny)
		for y := 0; y < len(g); y++ {
			for x := f.coordinate + 1; x < len(g[y]); x++ {
				nx := f.coordinate - (x - f.coordinate)
				log.Debugf("%d,%d → %d,%d\n", x, y, nx, y)
				if g[y][x] {
					ng[y][nx] = g[y][x]
				}
			}
		}
	case y:
		nx := len(g[0])
		ny := len(g) - f.coordinate - 1
		ng = makeSmallerGrid(g, nx, ny)
		for y := f.coordinate + 1; y < len(g); y++ {
			for x := 0; x < len(g[y]); x++ {
				ny := f.coordinate - (y - f.coordinate)
				log.Debugf("%d,%d → %d,%d\n", x, y, x, ny)
				if g[y][x] {
					ng[ny][x] = g[y][x]
				}
			}
		}
	}
	return ng
}

func countVisible(g grid) int {
	var count int
	for y := 0; y < len(g); y++ {
		for x := 0; x < len(g[y]); x++ {
			if g[y][x] {
				count++
			}
		}
	}
	return count
}

func main() {
	//log.SetLevel(log.DebugLevel)
	input, err := os.Open(filepath.Join("data", "13", "input.txt"))

	if err != nil {
		log.Fatal(err)
	}

	var g grid
	var folds []fold

	reader := bufio.NewReader(input)
	scanner := bufio.NewScanner(reader)

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}

		g, folds = parseLine(line, g, folds)
	}
	fg := foldGrid(g, folds[0])
	fmt.Println(countVisible(fg))

	for _, fold := range folds[1:] {
		fg = foldGrid(fg, fold)
	}
	fmt.Println(fg)
}
