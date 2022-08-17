
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chesstools

<!-- badges: start -->
<!-- badges: end -->

Chesstools provides the user with functions to convert any pgn database
into the standardised pgn dataframe used by otbchess, and fuctions to
download all rated games of one or more players from Lichess and
Chess.com, returned in a dataframe.

## Installation

You can install the development version of chesstools from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lukesingleton20/chesstools")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(chesstools)
pgn_to_dataframe("filepath/database.pgn")
retrieve_lichess_games(c("Lichess Username 1", "Lichess Username 2"))
retrieve_chesscom_games(c("Chess.com Username 1", "Chess.com Username 2"))
```
