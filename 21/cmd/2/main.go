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

type direction int

const (
	forward direction = iota
	down
	up
)

func (d direction) String() string {
	switch d {
	case forward:
		return "forward"
	case down:
		return "down"
	case up:
		return "up"
	default:
		return fmt.Sprintf("unknown direction: %d", int(d))
	}
}

type instruction struct {
	direction direction
	steps     int
}

type position struct {
	x   int
	y   int
	aim int
}

func parseInstruction(item []byte) instruction {
	bytesamount := bytes.SplitN(item, []byte(" "), 2)

	var direction direction

	switch string(bytesamount[0]) {
	case "forward":
		direction = forward
	case "down":
		direction = down
	case "up":
		direction = up
	}

	steps, err := strconv.Atoi(string(bytesamount[1]))

	if err != nil {
		log.Fatal(err)
	}

	inst := instruction{
		direction: direction,
		steps:     steps,
	}

	log.Debugf("Parsed '%s' to 'direction: %v, steps: %d'", item, inst.direction, inst.steps)

	return inst
}

func step1(pos *position, inst instruction) {
	switch inst.direction {
	case forward:
		pos.x += inst.steps
	case down:
		pos.y += inst.steps
	case up:
		pos.y -= inst.steps
	}
	log.WithField("part", "1").Debugf("New position: (%d, %d)", pos.x, pos.y)
}

func step2(pos *position, inst instruction) {
	switch inst.direction {
	case forward:
		pos.x += inst.steps
		pos.y += inst.steps * pos.aim
	case down:
		pos.aim += inst.steps
	case up:
		pos.aim -= inst.steps
	}
	log.WithField("part", "2").Debugf("New position: (%d, %d)", pos.x, pos.y)
}

func main() {
	log.SetLevel(log.DebugLevel)
	input, err := os.Open(filepath.Join("data", "2", "input.txt"))
	if err != nil {
		log.Fatal(err)
	}
	defer input.Close()

	scanner := bufio.NewScanner(input)

	var pos1 position
	var pos2 position

	for scanner.Scan() {
		line := scanner.Bytes()
		instruction := parseInstruction(line)
		step1(&pos1, instruction)
		step2(&pos2, instruction)
	}

	ans1 := pos1.x * pos1.y
	ans2 := pos2.x * pos2.y

	fmt.Printf("answer part 1: %d\n", ans1)
	fmt.Printf("answer part 2: %d\n", ans2)
}
