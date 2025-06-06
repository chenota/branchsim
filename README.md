# Branch Predictor Simulator

This program simulates an n-bit branch predictor for a series of taken/not-taken ground truth values and reports the results.

## How to Use

This is a Common Lisp program that is designed to run on Linux. I provided a shebang so you can run it via `./` like so:

```
$ ./branch.lisp <pattern> [args]
```

## Arguments

I wrote my own modular argument parser as part of this program; it definetly needs some refinement but it gets the job done for now. The arguments are as follows:

| Argument | Description | Default Value |
|-|-|-|
| `pattern` | **(Positional)** Repeating taken/not-taken pattern as a string. Character 'T' denotes taken, while character 'N' denotes not taken. | |
| `bits` | **(Optional)** Number of bits for the branch predictor. | 2 |
| `iters` | **(Optional)** Number of individual prediction trials. | 10 |
| `alias` | **(Optional)** Second taken/not-taken pattern as a string. Will be interleaved with `pattern`. | |
| `verbose` | **(Optional, Flag)** Use verbose table output. I recommend piping verbose output into `column`. | False |

## Example Outputs

```
$ ./branch.lisp "TTTN" --iters 1000
Hits    748
Misses  252
Rate    75.%
```

```
$ ./branch.lisp "TT" --alias "NN" --iters 4 --verbose | column -t -s $'\t'
Predictor  Prediction  Ground Truth  Hit Rate
00         N           T             .0%
01         N           N             50.%
00         N           T             33.%
01         N           N             50.%
```