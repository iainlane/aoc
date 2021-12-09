package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"

	log "github.com/sirupsen/logrus"
)

func parseInt(val []byte) int {
	var res int
	for _, v := range val[:len(val)-1] {
		res = res*10 + int(v-'0')
	}
	return res
}

func read(reader *bufio.Reader) []int {
	var out []int

	for {
		var val []byte
		var err error
		shouldBreak := false
		if val, err = reader.ReadSlice(','); err != nil {
			if err == io.EOF {
				shouldBreak = true
			} else {
				log.Fatal(err)
			}
		}
		out = append(out, parseInt(val))

		if shouldBreak {
			break
		}
	}

	sort.Ints(out)

	return out
}

// makeUpTo returns the amount of fuel required to get to the given value
func makeUpTo(vals []int, n int) (int, int) {
	var distance1, distance2 int
	for _, val := range vals {
		vn := val - n
		if vn < 0 {
			vn = -vn
		}

		// sum of one to vn, thanks Gauss
		vn2 := (vn * (vn + 1)) / 2

		log.Debugf("Move from %d to %d: %d fuel (part 1), %d fuel (part 2)", val, n, vn, vn2)
		distance1 += vn
		distance2 += vn2
	}
	return distance1, distance2
}

func solve(ints []int) (int, int) {
	max := ints[len(ints)-1]

	var currentBest, currentBest2, currentBestVal, currentBestVal2 int
	// brute force it
	for i := 0; i < max; i++ {
		currentVal, currentVal2 := makeUpTo(ints, i)
		log.Debugf("Move to %d: %d fuel (part 1), %d fuel (part 2)", i, currentVal, currentVal2)
		if currentBestVal == 0 || currentVal < currentBestVal {
			log.Debugln("New best:", currentVal)
			currentBest = i
			currentBestVal = currentVal
		}
		if currentBestVal2 == 0 || currentVal2 < currentBestVal2 {
			log.Debugln("New best (part 2):", currentVal2)
			currentBest2 = i
			currentBestVal2 = currentVal2
		}
	}
	log.Debugf("Best: Align to %d (part 1), %d (part 2) costing %d fuel (part 1), %d (part 2)",
		currentBest,
		currentBest2,
		currentBestVal,
		currentBestVal2)

	return currentBestVal, currentBestVal2
}

func main() {
	//log.SetLevel(log.DebugLevel)
	input, err := os.Open(filepath.Join("data", "7", "input.txt"))

	if err != nil {
		log.Fatal(err)
	}

	reader := bufio.NewReader(input)

	parsedValues := read(reader)

	part1, part2 := solve(parsedValues)

	fmt.Println(part1)
	fmt.Println(part2)
}
