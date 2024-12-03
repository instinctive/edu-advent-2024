# Advent of Code 2024

I'm experimenting with a "literate haskell" style, using
[`markdown-unlit`](https://github.com/sol/markdown-unlit).  This is a very
simple literate programming environment, and it allows some reordering but
without "chunk" labels, so it will possibly be confusing.

For example, look at [Day 2](day02.md). The definitions of `isSafe` and
`dampen` occur within the definition of `main`. If you look at the [raw
text](https://raw.githubusercontent.com/instinctive/edu-advent-2024/refs/heads/main/day02.md)
(not the markdown), you will see that the code for `main` is labeled `top:2`,
which means the markdown processor brings them all together. But the markdown
itself does not show that.

Hey, it's an experiment!

## Solutions

[Day 1](day01.md),
[Day 2](day02.md)
