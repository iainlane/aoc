package main

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
)

const maxDays = 9

func step(buckets []int) {
	new := make([]int, maxDays)
	copy(new, buckets)
	copy(buckets, buckets[1:])
	copy(buckets[len(buckets)-1:], new)

	buckets[6] = buckets[6] + buckets[8]
}

func run(input []int, numDays int) int {
	for i := 1; i <= numDays; i++ {
		step(input)
	}

	tot := 0
	for _, v := range input {
		tot += v
	}

	return tot
}

func makeInitialBuckets(reader *bufio.Reader) []int {
	buckets := make([]int, maxDays)

	for {
		shouldBreak := false
		val, err := reader.ReadSlice(',')

		if err != nil {
			if errors.Is(err, io.EOF) {
				shouldBreak = true
			} else {
				log.Fatal(err)
			}
		}

		buckets[int(val[0]-'0')]++

		if shouldBreak {
			break
		}
	}

	return buckets
}

func main() {
	input, err := os.Open(filepath.Join("data", "6", "input.txt"))

	if err != nil {
		log.Fatal(err)
	}

	reader := bufio.NewReader(input)

	buckets := makeInitialBuckets(reader)

	// run() modifies the input
	part1 := run(buckets, 80)
	// we can continue iterating for the remaining number of days, no need to
	// recompute the first 80
	part2 := run(buckets, 256-80)

	fmt.Println(part1)
	fmt.Println(part2)
}
