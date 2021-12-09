package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"path/filepath"
	"strings"
)

// You've seen each number exactly once in the input. Turns out that this means
// that you can add up the number of times each letter appears to uniquely
// determine the number itself.
var totalToNumberMap = map[int]int{
	42: 0,
	17: 1,
	34: 2,
	39: 3,
	30: 4,
	37: 5,
	41: 6,
	25: 7,
	49: 8,
	45: 9,
}

// frequency returns a map of the frequency of each character in the input.
func frequency(input string) map[rune]int {
	var out = make(map[rune]int)
	for _, val := range input {
		out[val]++
	}
	return out
}

// figureOutNumbers works out the mapping from segment to number using the
// observed frequencies
func figureOutNumbers(observations string, output []string) []int {
	freqs := frequency(observations)
	out := make([]int, len(output))

	for n, val := range output {
		for _, char := range val {
			charFreq := freqs[char]
			out[n] += charFreq
		}
		out[n] = totalToNumberMap[out[n]]
	}

	return out
}

// partOne just counts the number of 1, 4, 7, 8 in the decoded (but you can do
// it without decoding, just count the length) output
func partOne(input []int) (out int) {
	for _, val := range input {
		switch val {
		case 1, 4, 7, 8:
			out++
		}
	}

	return out
}

// partTwo renders the decoded output as an int
func partTwo(segments []int) int {
	var out int

	mul := int(math.Pow(10, float64(len(segments)-1)))
	for _, val := range segments {
		out += val * mul
		mul /= 10
	}

	return out
}

func parseLine(line string) (string, []string) {
	parts := strings.SplitN(line, " | ", 2)

	obs := strings.Join(strings.Split(parts[0], " "), "")

	return obs, strings.Split(parts[1], " ")
}

func main() {
	input, err := os.Open(filepath.Join("data", "8", "input.txt"))

	if err != nil {
		log.Fatal(err)
	}

	reader := bufio.NewReader(input)

	scanner := bufio.NewScanner(reader)

	var ans1, ans2 int
	for scanner.Scan() {
		line := scanner.Text()

		observations, output := parseLine(line)

		nums := figureOutNumbers(observations, output)

		ans1 += partOne(nums)
		ans2 += partTwo(nums)
	}

	fmt.Println(ans1)
	fmt.Println(ans2)
}
