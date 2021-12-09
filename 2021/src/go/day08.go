package main

import (
	"bufio"
	"fmt"
	"io"
	"net/http"
	"os"
	"strings"
)

type Point struct {
	x, y int
}

func ReadData(input io.Reader) ([]Point, []int) {

	points := make([]Point, 0, 256)
	route := make([]int, 0, 10000)

	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		if strings.HasPrefix(line, "(") {
			var p Point
			fmt.Sscanf(line, "(%d,%d)", &p.x, &p.y)
			points = append(points, p)
		} else {
			var i int
			fmt.Sscanf(line, "%d", &i)
			route = append(route, i)
		}
	}

	return points, route
}

func ThrowPackages(stops []Point, route []int) (Point, Point) {
	packets := make([][]int, 1000)
	for i := range packets {
		packets[i] = make([]int, 1000)
	}

	current := stops[route[0]]

	start := Point{999, 999}
	end := Point{0, 0}
	maxval := 0

	for i := 1; i < len(route); i++ {
		next := stops[route[i]]
		dx := 1
		dy := 1
		if current.x > next.x {
			dx = -1
		}
		if current.y > next.y {
			dy = -1
		}

		for x := current.x; x != next.x; x += dx {
			for y := current.y; y != next.y; y += dy {
				packets[x][y]++
				val := packets[x][y]
				if val > maxval {
					start.x = x
					start.y = y
					maxval = val
				}
				if val == maxval {
					end.x = x
					end.y = y
				}
			}
		}
		current = next
	}
	return start, end
}

func main() {
	var input io.Reader

	if len(os.Args) > 1 {
		response, err := http.Get(os.Args[1])
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}
		input = response.Body
		defer response.Body.Close()
	} else {
		input = bufio.NewReader(os.Stdin)
	}

	points, route := ReadData(input)
	p1, p2 := ThrowPackages(points, route)
	fmt.Printf("%d,%d %d,%d\n", p1.x, p1.y, p2.x, p2.y)
}
