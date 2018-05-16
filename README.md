# Haskell Maze Generator

This software provides an implementation of several algorithms to generate
mazes. The mazes are shown with
[Gloss](https://hackage.haskell.org/package/gloss)
(2D vector graphics, package for Haskell) and as an optional parameter,
the program can draw the path between the upper-left and bottom-right
corner, since this can help to visualize the difference of styles between
the algorithms. This project is strongly inspired by
[this](http://weblog.jamisbuck.org/2011/2/7/maze-generation-algorithm-recap)
blog of Maze Generation algorithms

Algorithms implemented so far
- [Growing Tree](https://github.com/Average-user/MazeGen/tree/master/src/GrowingTree.hs)
- [Hunt And Kill](https://github.com/Average-user/MazeGen/tree/master/src/HuntKill.hs)
- [Randomized Prims](https://github.com/Average-user/MazeGen/tree/master/src/Prims.hs)
- [Recursive Backtracker](https://github.com/Average-user/MazeGen/tree/master/src/Backtracker.hs)
- [Sidewinder](https://github.com/Average-user/MazeGen/tree/master/src/Sidewinder.hs)

An example of a 50x50 maze:

![](https://github.com/Average-user/MazeGen/blob/master/Pictures/example.png)
