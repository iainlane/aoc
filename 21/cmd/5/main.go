package main

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strconv"

	log "github.com/sirupsen/logrus"
)

type state struct {
	grid map[int]map[int]*int
	ans  int
}

type move struct {
	fromX int
	fromY int

	toX int
	toY int
}

func makeMove(lx, ly, rx, ry int) (m move) {
	// Normalise so we're going top left to bottom right, and then later we can
	// always fill in cells from the left move to the right move
	if lx < rx {
		m.fromX = lx
		m.fromY = ly

		m.toX = rx
		m.toY = ry

		return
	}

	if lx == rx {
		m.fromX = lx
		m.toX = rx

		if ly < ry {
			m.fromY = ly
			m.toY = ry
		} else {
			m.fromY = ry
			m.toY = ly
		}

		return
	}

	if lx > rx {
		m.fromX = rx
		m.fromY = ry

		m.toX = lx
		m.toY = ly

		return
	}

	return
}

func isHorizontal(m move) bool {
	return m.fromY == m.toY
}

func isVertical(m move) bool {
	return m.fromX == m.toX
}

func getElement(state *state, x, y int) *int {
	gridY := state.grid[x]

	if gridY == nil {
		state.grid[x] = make(map[int]*int)
	}

	_, ok := state.grid[x][y]

	if !ok {
		state.grid[x][y] = new(int)
	}

	return state.grid[x][y]
}

// Advance the state by one move
func play(state *state, move move) {
	if isHorizontal(move) {
		for x := move.fromX; x <= move.toX; x++ {
			n := getElement(state, x, move.fromY)
			(*n)++
			if *n == 2 {
				state.ans++
			}
		}

		return
	}
	if isVertical(move) {
		for y := move.fromY; y <= move.toY; y++ {
			n := getElement(state, move.fromX, y)
			(*n)++
			if *n == 2 {
				state.ans++
			}
		}
		return
	}

	// Diagonal
	if move.fromY < move.toY {
		x := move.fromX
		for y := move.fromY; y <= move.toY; y++ {
			n := getElement(state, x, y)
			x++
			(*n)++
			if *n == 2 {
				state.ans++
			}
		}
	} else {
		x := move.fromX
		for y := move.fromY; y >= move.toY; y-- {
			n := getElement(state, x, y)
			x++
			(*n)++
			if *n == 2 {
				state.ans++
			}
		}
	}
}

func (m move) String() string {
	return fmt.Sprintf("(%d, %d) -> (%d, %d)", m.fromX, m.fromY, m.toX, m.toY)
}

func main() {
	log.SetLevel(log.DebugLevel)
	input, err := os.Open(filepath.Join("data", "5", "input.txt"))

	if err != nil {
		log.Fatal(err)
	}

	scanner := bufio.NewScanner(input)

	statePartOne := state{
		grid: make(map[int]map[int]*int),
	}
	statePartTwo := state{
		grid: make(map[int]map[int]*int),
	}
	for scanner.Scan() {
		b := scanner.Bytes()
		leftright := bytes.SplitN(b, []byte(" -> "), 2)
		lxly := bytes.SplitN(leftright[0], []byte(","), 2)
		rxry := bytes.SplitN(leftright[1], []byte(","), 2)

		lx, err := strconv.Atoi(string(lxly[0]))
		if err != nil {
			log.Fatal(err)
		}
		ly, err := strconv.Atoi(string(lxly[1]))
		if err != nil {
			log.Fatal(err)
		}
		rx, err := strconv.Atoi(string(rxry[0]))
		if err != nil {
			log.Fatal(err)
		}

		ry, err := strconv.Atoi(string(rxry[1]))
		if err != nil {
			log.Fatal(err)
		}

		move := makeMove(lx, ly, rx, ry)

		if isHorizontal(move) || isVertical(move) {
			play(&statePartOne, move)
		}
		play(&statePartTwo, move)
	}
	fmt.Printf("Part one: %d\n", statePartOne.ans)
	fmt.Printf("Part two: %d\n", statePartTwo.ans)
}
