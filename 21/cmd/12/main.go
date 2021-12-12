package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"unicode"

	log "github.com/sirupsen/logrus"
)

type vertex struct {
	value       string
	small       bool
	connections []*vertex
}

func (v vertex) String() string {
	var s string
	for n, c := range v.connections {
		s += fmt.Sprintf("%s -> %s", v.value, c.value)

		if n < len(v.connections)-1 {
			s += "\n"
		}
	}
	return s
}

type path struct {
	vertices        []*vertex
	visited         map[*vertex]bool
	maxSmallVisited int
}

func (p path) String() string {
	var s string
	for n, v := range p.vertices {
		s += v.value
		if n < len(p.vertices)-1 {
			s += "<->"
		}
	}
	return s
}

type graph struct {
	allVertices map[string]*vertex
}

func (g graph) String() string {
	var s string
	for _, v := range g.allVertices {
		s += v.String() + "\n"
	}
	return s
}

func makeVertex(value string) *vertex {
	small := !unicode.IsUpper(rune(value[0]))
	return &vertex{
		value: value,
		small: small,
	}
}

func (g graph) addEdge(from, to string) {
	fromV := g.allVertices[from]
	if fromV == nil {
		fromV = makeVertex(from)
	}
	g.allVertices[fromV.value] = fromV
	toV := g.allVertices[to]
	if toV == nil {
		toV = makeVertex(to)
	}
	g.allVertices[toV.value] = toV
	fromV.connections = append(fromV.connections, toV)
	toV.connections = append(toV.connections, fromV)
}

func (v *vertex) canVisit(allowedVisits int, path path) bool {
	log.Debugf("canVisit: %s, path: %s", v.value, path)
	if !v.small {
		log.Debugln("yes: not small")
		return true
	}

	if !path.visited[v] {
		log.Debugln("yes: not visited")
		return true
	}

	if path.maxSmallVisited < allowedVisits-1 {
		log.Debugf("yes: max small visited: %d < %d", path.maxSmallVisited, allowedVisits-1)
		return true
	}

	log.Debugln("no")
	return false
}

func findAllPaths(g *graph, start, end string, allowedVisits int) []path {
	var paths []path
	var curQueue []path

	s := g.allVertices[start]

	curQueue = append(curQueue, path{
		vertices: []*vertex{g.allVertices[start]},
	})

	for len(curQueue) > 0 {
		var curPath path
		curPath, curQueue = curQueue[0], curQueue[1:]
		log.Debugf("Considering path: %s, queue: %s", curPath, curQueue)

		for _, v := range curPath.vertices[len(curPath.vertices)-1].connections {
			if v == s {
				log.Debugln("Not going back to the start")
				continue
			}
			if !v.canVisit(allowedVisits, curPath) {
				log.Debugf("path: %s, NOT visiting: %s", curPath, v.value)
				continue
			}

			log.Debugf("path: %s, visiting: %s", curPath, v.value)

			newPath := path{
				vertices:        make([]*vertex, len(curPath.vertices)+1),
				visited:         make(map[*vertex]bool, len(curPath.visited)),
				maxSmallVisited: curPath.maxSmallVisited,
			}

			if v.value == end {
				log.Debugln("found end")
				paths = append(paths, newPath)
				continue
			}

			copy(newPath.vertices, append(curPath.vertices, v))

			for k, v := range curPath.visited {
				newPath.visited[k] = v
			}

			if v.small {
				if newPath.visited[v] {
					newPath.maxSmallVisited++
				} else {
					newPath.visited[v] = true
				}
			}

			log.Debugf("created new path: %s", newPath)

			curQueue = append(curQueue, newPath)
			log.Debugf("adding to queue: %s. new queue: %s", newPath, curQueue)
		}
	}

	return paths
}

func main() {
	//log.SetLevel(log.DebugLevel)
	input, err := os.Open(filepath.Join("data", "12", "input.txt"))

	if err != nil {
		log.Fatal(err)
	}

	reader := bufio.NewReader(input)
	scanner := bufio.NewScanner(reader)

	grid := &graph{
		allVertices: make(map[string]*vertex),
	}
	for scanner.Scan() {
		line := scanner.Text()
		fromTo := strings.SplitN(line, "-", 2)
		grid.addEdge(fromTo[0], fromTo[1])
	}

	fmt.Println(len(findAllPaths(grid, "start", "end", 1)))
	fmt.Println(len(findAllPaths(grid, "start", "end", 2)))
}
