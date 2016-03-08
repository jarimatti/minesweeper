PROJECT = minesweeper
PROJECT_DESCRIPTION = Minesweeper game.
PROJECT_VERSION = 0.0.1

DEPS = jsone cowboy cowlib ranch
dep_jsone = hex 1.2.0
dep_cowboy = hex 1.0.4
dep_cowlib = hex 1.0.2
dep_ranch = hex 1.2.1

COVER = 1
SP = 4

include erlang.mk
