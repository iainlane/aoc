package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	log "github.com/sirupsen/logrus"
)

func step(s string, rules map[string]string, quantities map[rune]int) string {
	// slide over s, looking for a match in rules
	type insertion struct {
		index int
		value string
	}
	var insertions []insertion
	for i := 0; i < len(s)-1; i++ {
		window := s[i : i+2]
		if replacement, ok := rules[window]; ok {
			log.Debugf("Insert %s at %d", replacement, i+1)
			insertions = append(insertions, insertion{i + 1 + len(insertions), replacement})
			quantities[rune(replacement[0])]++
		}
	}

	for _, insertion := range insertions {
		s = s[:insertion.index] + insertion.value + s[insertion.index:]
	}

	return s
}

func findMaxMin(quantities map[rune]int) (int, int) {
	var max, min int
	// find maximum, minimum in quantities
	for _, v := range quantities {
		if v > max {
			max = v
		}
		if v < min || min == 0 {
			min = v
		}
	}
	return max, min
}

func main() {
	//log.SetLevel(log.DebugLevel)
	input, err := os.Open(filepath.Join("data", "14", "input.txt"))

	if err != nil {
		log.Fatal(err)
	}

	reader := bufio.NewReader(input)
	scanner := bufio.NewScanner(reader)

	insertionrules := make(map[string]string)
	quantities := make(map[rune]int)

	scanner.Scan()
	startString := scanner.Text()
	// populate quantities with the count of each character in startString
	for _, r := range startString {
		quantities[r]++
	}
	// Skip a blank line
	scanner.Scan()

	for scanner.Scan() {
		// NN -> C
		parts := strings.SplitN(scanner.Text(), " -> ", 2)

		log.Debugf("%s -> %s", parts[0], parts[1])
		insertionrules[parts[0]] = parts[1]
	}

	stepsPartOne := 10
	for i := 0; i < stepsPartOne; i++ {
		log.Infof("Iteration %d", i)
		startString = step(startString, insertionrules, quantities)
		log.Debugln(startString)
	}

	max, min := findMaxMin(quantities)
	fmt.Printf("Part 1: Max: %d, Min: %d. Max - min: %d\n", max, min, max-min)

	stepsPartTwo := 40
	for i := 0; i < stepsPartTwo-stepsPartOne; i++ {
		log.Infof("Iteration %d", i+stepsPartOne)
		startString = step(startString, insertionrules, quantities)
		log.Debugln(startString)
	}

	max, min = findMaxMin(quantities)
	fmt.Printf("Part 2: Max: %d, Min: %d. Max - min: %d\n", max, min, max-min)
}
