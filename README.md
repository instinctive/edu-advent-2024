# Advent of Code 2024

[`Advent`](src/Advent.md) (shared library)

[`Day 01`](src/day01.md)
[`Day 02`](src/day02.md)
[`Day 03`](src/day03.md)
[`Day 04`](src/day04.md)
[`Day 05`](src/day05.md)
plus bonus [`Day 03 regex`](src/day03-re.md)
<br/>
[`Day 06`](src/day06.md)
[`Day 07`](src/day07.md)
[`Day 08`](src/day08.md)
[`Day 09`](src/day09.md)
[`Day 10`](src/day10.md)
<br/>
[`Day 11`](src/day11.md)
[`Day 12`](src/day12.md)

I'm experimenting with a "literate haskell" style, using
[`markdown-unlit`](https://github.com/sol/markdown-unlit).  This is a very
simple literate programming environment, and it allows some reordering but
without "chunk" labels, so it will possibly be confusing.

For example, look at [Day 2](src/day02.md). The definitions of `isSafe` and
`dampen` occur within the definition of `main`. If you look at the [raw
text](https://raw.githubusercontent.com/instinctive/edu-advent-2024/refs/heads/main/src/day02.md)
(not the markdown), you will see that the code for `main` is labeled `top:2`,
which means the markdown processor brings them all together. But the markdown
itself does not show that.
