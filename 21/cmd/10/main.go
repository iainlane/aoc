package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"sort"

	log "github.com/sirupsen/logrus"
)

type intPair struct {
	a, b int
}

var closers = map[rune]intPair{
	')': {3, 1},
	']': {57, 2},
	'}': {1197, 3},
	'>': {25137, 4},
}

var openers = map[rune]rune{
	'(': ')',
	'[': ']',
	'{': '}',
	'<': '>',
}

// parseLine reads the characters in a line one by one. If an opener is seen,
// the matching closer is pushed to a stack. If a non-opener is seen, the stack
// is popped and compared with the character. If it doesn't match, the illegal
// character is returned. The stack is returned too. If this is non-empty, it
// means the string is incomplete and the characters on the stack were missing.
func parseLine(line string) ([]rune, rune) {
	var closerStack []rune
	for _, c := range line {
		// If we saw an opener, expect the matching closer
		if closer, ok := openers[c]; ok {
			closerStack = append(closerStack, closer)
			continue
		}
		// If we saw any other char, check it was the one we wanted
		expectedCloser := closerStack[len(closerStack)-1]
		if c != expectedCloser {
			log.Debugf("Expected %c, got %c", expectedCloser, c)
			return closerStack, c
		}
		closerStack = closerStack[:len(closerStack)-1]
	}
	return closerStack, 0
}

func partOne(line string) int {
	_, wrongChar := parseLine(line)

	if wrongChar == 0 {
		return 0
	}

	return closers[wrongChar].a
}

func partTwo(line string) int {
	closerStack, wrongChar := parseLine(line)

	if wrongChar != 0 {
		return 0
	}

	out := 0
	for i := len(closerStack) - 1; i >= 0; i-- {
		out = (out * 5) + closers[closerStack[i]].b
	}

	return out
}

func main() {
	//log.SetLevel(log.DebugLevel)
	input, err := os.Open(filepath.Join("data", "10", "input.txt"))

	if err != nil {
		log.Fatal(err)
	}

	reader := bufio.NewReader(input)
	scanner := bufio.NewScanner(reader)

	ans1 := 0
	var ans2 []int
	for scanner.Scan() {
		line := scanner.Text()
		ans1 += partOne(line)

		a2 := partTwo(line)
		if a2 != 0 {
			ans2 = append(ans2, a2)
		}
	}

	sort.Ints(ans2)

	fmt.Println(ans1)
	fmt.Println(ans2[len(ans2)/2])
}
