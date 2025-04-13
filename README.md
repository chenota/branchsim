# Branch Predictor Simulator

This program simulates an n-bit branch predictor for a series of taken/not-taken ground truth values. I wrote this simulator to assist with a homework where I had to figure out a series of aliased branch patterns that resulted in positive interference.

## How to Use

This is a Common Lisp program that is designed to run on Linux. I provided a shebang so you can run it via `./` like so:

```
$ ./branch.lisp <pattern> [args]
```

## Arguments

I wrote my own modular argument parser as part of this program; it definetly needs some refinement but it gets the job done for now. The arguments are as follows:

| Argument | Description | Default Value |
|-|-|-|
| `pattern` | **(Positional)** Repeating taken/not-taken pattern as a string. Character 'T' denotes taken, while character 'F' denotes not taken. | |
| `bits` | **(Optional)** Number of bits for the branch predictor. | 2 |
| `iters` | **(Optional)** Number of individual prediction trials. | 10 |
| `alias` | **(Optional)** Second taken/not-taken pattern as a string. Will be interleaved with `pattern`. | |
| `verbose` | **(Optional, Flag)** Use verbose table output. I recommend piping verbose output into `column`. | False |

## Example Outputs

