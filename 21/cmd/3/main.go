package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strconv"

	log "github.com/sirupsen/logrus"
)

type count struct {
	ones, zeroes []int64
}

func loop(input []int64,
		i int,
		eq func(int64, int64) bool,
	cmp func(int, int) bool) []int64 {
		var counts count
	if len(input) > 1 {
		for _, val := range input {
			if eq(val&(1<<i), 1<<i) {
				counts.ones = append(counts.ones, val)
			} else {
				counts.zeroes = append(counts.zeroes, val)
			}
		}
		if cmp(len(counts.zeroes), len(counts.ones)) {
			input = counts.zeroes
		} else {
			input = counts.ones
		}
	}

	return input
}

func main() {
	//log.SetLevel(log.DebugLevel)

	input, err := os.Open(filepath.Join("data", "3", "input.txt"))
	if err != nil {
		log.Fatal(err)
	}

	scanner := bufio.NewScanner(input)

	var counts []count
	var nbits int
	var vals []int64

	for scanner.Scan() {
		t := scanner.Text()
		val, err := strconv.ParseInt(t, 2, 64)

		if err != nil {
			log.Fatal(err)
		}

		if counts == nil {
			nbits = len(t)
			counts = make([]count, nbits)
		}

		vals = append(vals, val)
	}

	for i := nbits - 1; i >= 0; i-- {
		for _, val := range vals {
			if val&(1<<i) == 1<<i {
				counts[i].ones = append(counts[i].ones, val)
			} else {
				counts[i].zeroes = append(counts[i].zeroes, val)
			}
		}
	}

	var gamma, epsilon int
	for n, v := range counts {
		if len(v.zeroes) > len(v.ones) {
			epsilon |= (1 << n)
		} else {
			gamma |= (1 << n)
		}
	}

	fmt.Printf("ans 1: %d\n", gamma*epsilon)

	oxygen := vals
	co2 := vals
	for i := nbits - 1; i >= 0; i-- {
		log.Debugf("Bit %d", i)
		oxygen = loop(
			oxygen,
			i,
			func(a, b int64) bool { return a == b },
			func(a, b int) bool { return a > b },
		)
		co2 = loop(
			co2,
			i,
			func(a, b int64) bool { return a != b },
			func(a, b int) bool { return a < b },
		)
	}
	log.Debugf("oxygen: %b (%d), co2: %b (%d)", oxygen[0], oxygen[0], co2[0], co2[0])
	fmt.Printf("ans2: %d\n", oxygen[0]*co2[0])
}
