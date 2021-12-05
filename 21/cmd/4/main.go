package main

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/fatih/color"
	log "github.com/sirupsen/logrus"
)

type board struct {
	size int
	rows [][]*cell
	cols [][]*cell
	num  map[int]*cell
	won  bool
}

func score(b *board) int {
	var tot int
	for k, cell := range b.num {
		if !cell.marked {
			tot += k
		}
	}

	return tot
}

func newBoard(size int) *board {
	b := &board{
		size: size,
		rows: make([][]*cell, size),
		cols: make([][]*cell, size),
		num:  make(map[int]*cell),
	}

	for i := 0; i < 5; i++ {
		b.rows[i] = make([]*cell, size)
		b.cols[i] = make([]*cell, size)
	}

	return b
}

func (b board) String() string {
	var out strings.Builder
	for _, row := range b.rows {
		for n, cell := range row {
			out.WriteString(cell.String())
			if n < b.size-1 {
				out.WriteString(" ")
			}
		}
		out.WriteString("\n")
	}

	return out.String()
}

type cell struct {
	number int
	marked bool
	row    []*cell
	col    []*cell
}

func (c cell) String() string {
	bold := color.New(color.Bold).SprintFunc()
	s := fmt.Sprintf("%2d", c.number)

	if c.marked {
		return bold(s)
	} else {
		return s
	}
}

func play(b *board, num int) bool {
	log.Debugf("Playing %d", num)
	c, isInBoard := b.num[num]

	if !isInBoard {
		log.Debug("Not in this board")
		return false
	}

	c.marked = true

	won := true
	for _, cl := range c.row {
		if !cl.marked {
			won = false
		}
	}

	if won {
		log.Debug("This was a winning move")
		return true
	}

	won = true
	for _, cl := range c.col {
		if !cl.marked {
			won = false
		}
	}

	return won
}

func buildBoard(scanner *bufio.Scanner, boardSize int) *board {
	board := newBoard(boardSize)
	for i := 0; i < boardSize; i++ {
		row := scanner.Text()
		fields := make([]int, 5)

		_, err := fmt.Sscanf(row, "%2d %2d %2d %2d %2d", &fields[0], &fields[1], &fields[2], &fields[3], &fields[4])
		if err != nil {
			log.Fatal(err)
		}

		for x, num := range fields {
			cell := &cell{
				number: num,
				row:    board.rows[i],
				col:    board.cols[x],
			}
			board.rows[i][x] = cell
			board.cols[x][i] = cell
			board.num[num] = cell
		}
		scanner.Scan()
	}

	return board
}

type win struct {
	move  int
	score int
}

func plays(moves []int, boards []*board) []win {
	var wins []win
	for _, move := range moves {
		for bn, board := range boards {
			if board.won {
				continue
			}

			won := play(board, move)
			log.Debugln(board)

			if won {
				board.won = true
				s := score(board)
				wins = append(wins, win{move: move, score: s})
				log.Debugf("The move %d won on board %d\n\n%s\nwith score %d * %d = %d\n",
					move,
					bn,
					board,
					move,
					s,
					move*s)
			}
		}
	}
	return wins
}

func main() {
	const boardSize = 5

	//log.SetLevel(log.DebugLevel)
	input, err := os.Open(filepath.Join("data", "4", "input.txt"))

	if err != nil {
		log.Fatal(err)
	}

	scanner := bufio.NewScanner(input)

	// Read the moves from the first line
	scanner.Scan()
	var moves []int
	for _, move := range bytes.Split(scanner.Bytes(), []byte(",")) {
		m, err := strconv.Atoi(string(move))
		if err != nil {
			log.Fatal(err)
		}
		moves = append(moves, m)
	}

	// There's a blank line; skip it
	scanner.Scan()

	var boards []*board
	for scanner.Scan() {
		board := buildBoard(scanner, boardSize)
		boards = append(boards, board)
	}

	wins := plays(moves, boards)

	partone := wins[0]
	parttwo := wins[len(wins)-1]

	fmt.Printf("Part one: %d * %d = %d\n", partone.move, partone.score, partone.move*partone.score)
	fmt.Printf("Part two: %d * %d = %d\n", parttwo.move, parttwo.score, parttwo.move*parttwo.score)
}
