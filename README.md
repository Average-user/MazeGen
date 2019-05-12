# Haskell Maze Generator

This software provides an implementation of several algorithms to generate
mazes. The mazes are shown with
[Gloss](https://hackage.haskell.org/package/gloss)
(2D vector graphics, package for Haskell). It also allows to draw paths from one
place to another, and is capable of generating perfect and non-perfect mazes.
This project is strongly inspired by
[this](http://weblog.jamisbuck.org/2011/2/7/maze-generation-algorithm-recap)
blog of Maze Generation algorithms

Algorithms implemented so far
- [Growing Tree](https://github.com/Average-user/MazeGen/tree/master/src/Algorithm/GrowingTree.hs)
- [Hunt And Kill](https://github.com/Average-user/MazeGen/tree/master/src/Algorithm/HuntKill.hs)
- [Randomized Prims](https://github.com/Average-user/MazeGen/tree/master/src/Algorithm/Prims.hs)
- [Recursive Backtracker](https://github.com/Average-user/MazeGen/tree/master/src/Algorithm/Backtracker.hs)
- [Sidewinder](https://github.com/Average-user/MazeGen/tree/master/src/Algorithm/Sidewinder.hs)
- [Randomized Kruskals](https://github.com/Average-user/MazeGen/tree/master/src/Algorithm/Kruskals.hs)

#### Usage

The parameters for building the maze, are specified in `config.json` file. Which
by default is:

``` json
{
    "algorithm": "Prims",
    "maze-size": "(30,30)",
    "walls-to-remove": "10",
    "draw-paths": "True",
    "path-starting-point": "(0,0)",
    "path-ending-point": "(29, 29)"
}
```
To know by what name algorithms are recognized checkout
[`ParseConfig.hs`](https://github.com/Average-user/MazeGen/blob/master/src/ParseConfig.hs).

`walls-to-remove` refers to the amount of walls to remove from the maze to make
a non-perfect maze. The default 0, generates perfect mazes (one and only one
path from any place to another).

When `draw-paths` is on, it I'll draw shortest paths in red, and the more large
ones in purple.

An example of default `config.json`:

``` text
stack build
stack exec MazeGen-exe
```

![](https://github.com/Average-user/MazeGen/blob/master/Pictures/example.png)

```
