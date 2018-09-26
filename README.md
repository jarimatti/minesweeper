# A Minesweeper Game

This is a simple minesweeper game, done in Erlang. The aim of the game
is simple: open positions in the board without hitting mines. Flag all
mine positions. The game is over when all mines are marked and empty
positions revealed or a mine is revealed.

This is still work in progress. For the time being only a simple
console interface and initial web UI are done.

# Compile

Build using make:

    make

# Run EUnit Tests

Run tests:

    make tests

The EUnit tests are currently mixed with the application code and
conditionally compiled. The reason for the initial decision is based
on the method presented in course
["Introduction to Systematic Program Design - Part 1"](https://www.coursera.org/course/programdesign)
by Gregor Kiczales. The tests will probably be separated into own
files in the future.

# Run the application release Erlang shell

Simple:

    make run

# Usage

## Web UI

The game starts a web server in [http://localhost:8080](http://localhost:8080).
Open a browser there and start a new game by clickin the "New game" button.

Playing the game should feel familiar: left clicking on a gray square opens it
and hopefully does not reveal a mine. Right clicking on a gray square flags it
as a mine or removes the flag.

The only indication of a won game is that all the cells have been successfully
opened and mines flagged. This is still a work in progress.

The gray squares are unknown, white squares are opened and a possible number in
a white square tells how many mines are in adjacent squares.


## Console UI

Start the game with function `ms_console:start_game(Rows, Cols,
MineProb)`, where `Rows` is the row count, `Cols` is the column count
and `MineProb` is the probability of mines in range [0..100]. The last
argument will be changed to be actual mine count instead of just
probability of mines in the future.

The game starts and displays an undiscovered mine field.

Mine field legend:

 - `#` undiscovered location
 - `.` discovered empty location, no neighboring mines
 - `4` discovered empty location, 4 neighboring mines
 - `x` a flagged location
 - `*` a mine
 - `o` a false flag, shown after game over

Commands:

 - `p` show the mine field again
 - `o 4 5` open location row 4, column 5
 - `f 4 5` flag location row 4, column 5 as suspect mine
 - `u 4 5` remove flag from location row 4, column 5
 - `q` quit the game
 

### Example Session

Start a game with 10 rows, 20 columns and 10 percent probability for mines:

    $ make run
    ...
    Erlang/OTP 17 [erts-6.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

    Eshell V6.1  (abort with ^G)
    1> l(ms_server).
    {module,ms_server}
    2> l(ms_console).
    {module,ms_console}
    3> ms_console:start_game(10, 20, 10).
                11111111112
       12345678901234567890
      +--------------------+
     1|####################|
     2|####################|
     3|####################|
     4|####################|
     5|####################|
     6|####################|
     7|####################|
     8|####################|
     9|####################|
    10|####################|
      +--------------------+
    [u]nflag / [f]lag / [o]pen r c or [q]uit: o 5 5
                11111111112
       12345678901234567890
      +--------------------+
     1|#1.......2######1...|
     2|11111....3######1...|
     3|..1#1....2####211...|
     4|..111..11212#21.....|
     5|.......1#1.1#1......|
     6|.......111.111......|
     7|..........111..11211|
     8|111.......1#1..1####|
     9|##1.......111..113##|
    10|##1..............1##|
      +--------------------+
    [u]nflag / [f]lag / [o]pen r c or [q]uit: 


