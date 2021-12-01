package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"

	log "github.com/sirupsen/logrus"
)

func main() {
	input, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer input.Close()

	scanner := bufio.NewScanner(input)

	var ans1, ans2, prev1, prev2, prev_window_value int
	for scanner.Scan() {
		item := scanner.Text()

		item_int, err := strconv.Atoi(item)
		if err != nil {
			log.Fatal(err)
		}

		if prev1 > 0 && item_int > prev1 {
			ans1++
		}

		if prev1 > 0 && prev2 > 0 {
			wv := prev1 + prev2 + item_int
			if prev_window_value > 0 && wv > prev_window_value {
				ans2++
			}
			prev_window_value = wv
		}

		prev2 = prev1
		prev1 = item_int
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	fmt.Printf("%d\n", ans1)
	fmt.Printf("%d\n", ans2)
}
