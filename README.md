
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chesstools

<!-- badges: start -->
<!-- badges: end -->

Chesstools provides the user with functions: (1) Convert any pgn
database into the standardised pgn dataframe used by otbchess (2)
Download all rated games of one or more players from Lichess or
Chess.com returned in an otbchess standardised dataframe (3) Download
all historical ECF ratings of an individual player by using their ECF
Rating Code

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
retrieve_ecf_ratings("341984")
retrieve_lichess_games(c("Lichess Username 1", "Lichess Username 2"))
retrieve_chesscom_games(c("Chess.com Username 1", "Chess.com Username 2"))
```
