# fast-matrix

Consider we have 3 matrices A B and C of sizes (50 5) (5 100) and (100 10) respectively.
There are 2 ways to multiply them then:
(AB)C or A(BC)

First way gives 50*5*100 + 50*100*10 = 75000 multiplications.
Second way gives 5*100*10 + 50*5*10 = 7500. As you seem the difference is huge.

Let's optimize it!

We take a vector of dimensions, ((4 . 5) (5 . 10) (10 . 2) (2 . 8) (8 . 6) (6 .
5) (5 . 10) ...) (height width) and what we want is the best tree for the whole thing, that is range from 0 to n.
We solve it for each subrange (from i to j).
Ranges of length 1 and 2 are trivial.
For range from i to (+ i k) now -- there are k-1 options, of which we chose the best one.
Options are:
1: multiply i matrix by the best of range (+ i 1) to (+ i k)
2: multiply the best of range i, (+ i 1) to the best of range (+ i 2) (+ i k)
3: multiply the best of range i, (+ i 2) to the best of range (+ i 3) (+ i k)
...etc
The best one is the one with the lowest (total) amount of multiplications.

## License

MIT is just because. You can think it is CCA, I am just trying to say "Hey, I
did something"
