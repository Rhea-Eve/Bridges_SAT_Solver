# Bridges Puzzle Solver

This is an OCaml program to solve the Bridges Puzzle. The Bridges Puzzle is a logical puzzle that involves connecting islands with bridges under certain constraints.

## How to Use

### Running the Program

 `ocamlopt -o bridges_solver bridges_game.ml`
 `./bridges_solver `

 (This will allow you to make your own game in the program)

 OR

  `ocamlopt -o bridges_solver bridges_game.ml`
 `./bridges_solver filename`

 (This will allow you to read a game from a file)

### Creating Puzzles

The program starts in interactive puzzle creation mode. You can create your own puzzle by providing the puzzle dimensions, adding islands, and specifying the connections.

### Solving Puzzles

If you already have a puzzle defined in a file, you can solve it by running:

```bash
./bridges_solver your_puzzle_file.txt
```

Make sure your puzzle file is formatted correctly.

## Puzzle File Format

The puzzle file should contain the puzzle dimensions followed by the puzzle grid. Islands are represented by digits, and empty cells are represented by dots. For example:

```
5
1...1
.....
.....
.....
1...1
```

This represents a 5x5 puzzle with islands placed at specific positions. The numbers are the number of connections each island can have. 

## Code Overview

- `project1.ml`: The main OCaml source file containing the puzzle solver.
- `tokenizer`: Function to parse the input string and generate a list of islands.
- `make_possible_bridge_list`: Function to generate a list of possible bridges given a list of islands.
- `max_conect` and `min_conect`: Functions to generate CNF formulas to ensure the maximum and minimum number of connections for each island.
- `connected_sat_statments`: Function to generate CNF formulas to ensure the connectedness of the islands.
- `solve`: Function to solve the puzzle using a SAT solver (MiniSat).
- `print_solution`: Function to print the puzzle solution.
- `invoke_minisat`: Function to invoke the MiniSat solver.
- `turn_sat_readable`: Function to convert the output of the SAT solver to a readable format.
- `sat_out_put_to_bridge`: Function to convert SAT output to a list of bridges.

## Dependencies

This program relies on the MiniSat solver. Ensure it is installed on your system or provide the path to the executable in the code.

## License

This code is released under the MIT License. Feel free to use, modify, and distribute it as needed. Contributions are welcome!
