# Fast-matrix (tensor?)

This small library optimizes the associative order of matrix multiplication (in the future -- tensor contraction) using a dynamic programming algorithm.

# Abstract

Consider 3 matrices `A_(50 . 5), B_(5 . 100), C_(100 . 10)`. 
We can obtain matrix
```
D = A * B * C;
```
in two ways:
```math
D = (AB)C , D = A(BC)
```

Using the first way gives  `50*5*100 + 50*100*10 = 75000`$ scalar multiplications.
Using the second way gives$`5*100*10 + 50*5*10 = 7500`$, which is ten times less. Our goal is to minimize the number of multiplications.

Function `%build-tree` takes a vector of dimensions, `#((4 . 5) (5 . 10) (10 . 2) (2 . 8) (8 . 6) (6 . 5) (5 . 10) ...)` (every element corresponds to `(height . width)` of a matrix) and returns list of `(the best tree for the whole range from 0 to n, the resulting matrix size, the total number of scalar multiplications)`

# Algorithm

Dynamic programming - we solve it for each subrange (from i to j).

Ranges of length 1 and 2 are trivial.
For range from i to (+ i k) now -- there are k-1 options, of which we chose the best one.
Options are:

* multiply i matrix by the best of range (+ i 1) to (+ i k)

* multiply the best of range i, (+ i 1) to the best of range (+ i 2) (+ i k)

* multiply the best of range i, (+ i 2) to the best of range (+ i 3) (+ i k)

...etc

The best one is the one with the lowest (total) amount of multiplications.

## License

MIT. Contributions are welcome.
