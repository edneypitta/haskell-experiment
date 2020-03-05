# robot-cleaner
robot cleaner exercise written in haskell

## Background

When you have a lot of people working in an office it can get dirty quite quickly if you're not careful. However, cleaning staff are expensive. To save money on cleaning staff the best solution was deemed to be the creation of an automatic cleaning robot that cleans the office at night.

## Assignment

Your assignment is to build a prototype of this robot. The assignment is designed to be as simple as possible.

The robot will, once given some instructions (shown below as input), run on its own without any human interference. In the morning we can ask the robot to report how many unique places in the office it has cleaned.

## Input and Output Criteria

* All input will be given on standard in.
* All output is expected on standard out.
* First input line: a single integer that represents the number of commands the robot should expect to execute before it knows it is done. The number will be in the range **n** (**0** ≤ **n** ≤ **10, 000**).
* Second input line: consists of two integer numbers that represents the starting coordinates **x y**
of the robot. The value of each coordinate will be in the range **x** (**—100**, **000** ≤ **x** ≤ **100, 000**) and **y** (**—100, 000** ≤ **y** ≤ **100, 000**).
* The third, and any subsequent line, will consist of two pieces of data. The first will be a single uppercase character **c** ∈ {**E**, **W**, **S**, **N**}, that represents the direction on the compass the robot should head. The second will be an integer representing the number of steps **s** (**0** < **s** < **100,000**) that the robot should take in said direction.

## Special Notes

* The robot will never be sent outside the bounds of the plane.
* All input should be considered well formed and syntactically correct. There is no need, therefore, to implement elaborate input parsing.
* Do not output any error messages. See previous point. The only output should be the number of unique places that the robot cleaned. See below.
* There will no leading or trailing white space on any line of input.
* There should be no leading or trailing whitespace on any line of output.
* Any multi−valued line of input will have a single white space character between each value.
* You can assume, for the sake of simplicity, that the office can be viewed as a grid where the robot moves only on the vertices.
* The robot cleans at every vertex it touches not just where it stops.

## The Output

The output of your program should be a number **u**, which represents the number of *unique places* in the office that were cleaned. The output of the number **u** should be prefixed by `"=> Cleaned: "`, (excluding the quotes).

## Example input:

```
2
10 22
E 2
N 1
```

## Example output:

```
=> Cleaned: 4
```
