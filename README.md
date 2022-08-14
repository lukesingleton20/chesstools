
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chesstools

<!-- badges: start -->
<!-- badges: end -->

Chesstools provides the user with functions to download games of one or
more players from Lichess and Chess.com. All rated games will be
retrieved in a standard dataframe. Note that all data will be in the
character format.

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
retrieve_lichess_games(c("Lichess Username 1", "Lichess Username 2"))
retrieve_chesscom_games(c("Chess.com Username 1", "Chess.com Username 2"))
```
