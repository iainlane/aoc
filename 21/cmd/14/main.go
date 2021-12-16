package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	log "github.com/sirupsen/logrus"
)

func makePairsMap(s string) map[string]int {
	pairs := make(map[string]int)
	for i := 0; i < len(s)-1; i++ {
		pairs[s[i:i+2]] += 1
	}
	return pairs
}

func step(pairsMap map[string]int, rules map[string]string, quantities map[byte]int) {
	updateMap := make(map[string]int)
	for k, v := range rules {
		if pv, ok := pairsMap[k]; ok {
			log.Debugf("%s -> %s. Setting %s to 0, setting %s to %d, setting %s to %d",
				k, v, k, string(k[0])+v, updateMap[string(k[0])+v]+pv, string(k[1])+v, updateMap[string(k[1])+v]+pv)
			if _, ok := updateMap[k]; !ok {
				updateMap[k] = 0
			}
			updateMap[string(k[0])+v] += pv
			updateMap[v+string(k[1])] += pv
			quantities[v[0]] += pv
		}
	}
	// Could this be done without the intermediate map?
	for k, v := range updateMap {
		if v == 0 {
			delete(pairsMap, k)
		} else {
			pairsMap[k] = v
		}
	}
}

func findMaxMin(quantities map[byte]int) (int, int) {
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

func iterate(times int, rules map[string]string, pairsMap map[string]int, quantities map[byte]int) (int, int) {
	for i := 0; i < times; i++ {
		step(pairsMap, rules, quantities)
	}

	max, min := findMaxMin(quantities)

	return max, min
}

func countCharacters(s string) map[byte]int {
	quantities := make(map[byte]int)
	for _, b := range []byte(s) {
		quantities[b]++
	}
	return quantities
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

	scanner.Scan()
	startString := scanner.Text()
	scanner.Scan()

	for scanner.Scan() {
		// NN -> C
		parts := strings.SplitN(scanner.Text(), " -> ", 2)

		insertionrules[parts[0]] = parts[1]
	}

	// Set up initial state
	quantities := countCharacters(startString)
	pairsMap := makePairsMap(startString)

	stepsPartOne := 10

	max, min := iterate(stepsPartOne, insertionrules, pairsMap, quantities)
	fmt.Printf("Part 1: Max: %d, Min: %d. Max - min: %d\n", max, min, max-min)

	stepsPartTwo := 40
	max, min = iterate(stepsPartTwo-stepsPartOne, insertionrules, pairsMap, quantities)
	fmt.Printf("Part 2: Max: %d, Min: %d. Max - min: %d\n", max, min, max-min)
}
