#' Retrieve Lichess Games
#'
#' This function retrieves rated games for one or more players from Lichess.
#'
#' @param player One or more Lichess usernames.
#'
#' @return An otbchess standardised dataframe.
#' @import httr, curl
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
retrieve_lichess_games <- function(players){

  retrieve_player_games <- function(player){

    # first, we do a cursory check that player is a valid Lichess username
    if(httr::GET(paste0("https://lichess.org/@/",player))$status_code == 404){
      return("This Lichess username does not exist.")
    }

    # create temporary file
    tmp <- tempfile()

    # download from Lichess via API using curl to our temporary file
    # nb: surprisingly doesn't need OAuth2 login or token
    curl::curl_download(paste0("https://lichess.org/api/games/user/",player,"?rated=true"), tmp)

    # readLines converts each line of the tmp file into a character object
    pgn_database <- readLines(tmp)

    # to handle those cases where a player has played no games on Lichess,
    # return an error message instead of a null dataframe
    if(length(pgn_database) == 0){
      return("Player has played no games on Lichess.")
    }

    # first we need to collapse the raw file to get something closer to a
    # standard pgn database
    pgn_database <- paste(pgn_database,collapse="\n")

    # now we convert the pgn database into the otbchess standard pgn dataframe
    pgn_dataframe <- pgn_to_dataframe(pgn_database)

    return(pgn_dataframe)
  }

  # create the overall dataframe to which we'll append each players' dataframe
  lichess_dataframe <- data.frame(stringsAsFactors = FALSE)

  # for loop that calls the retrieve_player_games() function
  # for each_player found within the input players
  # the if function checks that retrieve_player_games() has returned a dataframe
  # (i.e. did not stop early and returned an error message)
  # then uses rbind() to append the dataframe returned from retrieve_player_games()
  # to the dataframe lichess_dataframe that contains all previously retrieved dataframes

  for(each_player in players){
    player_dataframe <- retrieve_player_games(each_player)
    if(is.data.frame(player_dataframe) == TRUE){
      lichess_dataframe <- rbind(lichess_dataframe,player_dataframe)
    }
  }

  return(lichess_dataframe)
}

#' Retrieve Chess.com Games
#'
#' This function retrieves rated games for one or more players from chess.com.
#'
#' @param players One or more Chess.com usernames
#'
#' @return An otbchess standardised dataframe.
#' @import jsonlite, httr, curl, stringr
#' @export
#'
#' @examples
retrieve_chesscom_games <- function(players){

  retrieve_player_games <- function(player){

    # first, we do a cursory check that player is a valid Chess.com username
    if(httr::GET(paste0("https://www.chess.com/member/",player))$status_code == 404){
      return("This Chess.com username does not exist.")
    }

    # the chess.com API requires that you pull games on a per month basis
    # so first we need to establish every year/month that has games for this player
    player_history <- jsonlite::fromJSON(paste0("https://api.chess.com/pub/player/",player,"/games/archives"))$archives

    # to handle those cases where a player has played no games on chess.com,
    # return an error message instead of a null dataframe

    if(length(player_history) == 0){
      return("Player has played no games on Chess.com.")
    }

    # next, we need to download each monthly archive so we set up a character
    # vector and assign each vector as this is much quicker than
    # using a for loop with the append() function
    pgn_database <- character(0)

    # this for loop downloads each monthly archive into a temporary file,
    # then copies it into the raw_games vector
    # we then collapse the vector so that there is one vector with a single element
    # that contains every game, rather than each element being a set of monthly games

    count <- 0
    for(month in player_history){
      tmp <- tempfile()
      curl::curl_download(paste0(month,"/pgn"),tmp)
      pgn_monthly_database <- readLines(tmp) %>% paste(., collapse = "\n")
      pgn_database[count] <- paste0(pgn_monthly_database,"\n\n")
      count <- count + 1
    }

    # unfortunately, as of August 2022, rather than returning the tournament name
    # within "Events" as is best practice, chess.com adds tournament information
    # under an unusual "Tournament" heading, which is not a practice seen anywhere else,
    # so we must first make a list of games so that we can isolate this information
    # and move it under "Events"
    pgn_database <- strsplit(pgn_database, "\n\n\n") %>% unlist(.)

    # build an index of which games have the "Tournament" heading present
    tournament_index <- which(grepl("[Tournament",pgn_database, fixed = TRUE))

    # next we replace the information contained in "Event" with the information
    # contained within "Tournament"
    for(game in tournament_index){
      tournament <- str_extract(pgn_database[game],"(?<=www\\.chess\\.com\\/tournament\\/).*?(?=\"\\])")
      pgn_database[game] <- str_replace(pgn_database[game],"(?<=\\[Event \").*?(?=\"\\])",tournament)
    }

    # we then collapse the raw file to get something closer to a standard pgn database
    pgn_database <- paste(pgn_database,collapse="\n")

    # now we convert the pgn database into the otbchess standard pgn dataframe
    pgn_dataframe <- pgn_to_dataframe(pgn_database)

    return(pgn_dataframe)

  }

  # create the overall dataframe to which we'll append each players' dataframe
  chesscom_dataframe <- data.frame(stringsAsFactors = FALSE)

  # for loop that calls the retrieve_player_games() function
  # for each_player found within the input players
  # the if function checks that retrieve_player_games() has returned a dataframe
  # (i.e. did not stop early and returned an error message)
  # then uses rbind() to append the dataframe returned from retrieve_player_games()
  # to the dataframe chesscom_dataframe that contains all previously retrieved dataframes

  for(each_player in players){
    player_dataframe <- retrieve_player_games(each_player)
    if(is.data.frame(player_dataframe) == TRUE){
      chesscom_dataframe <- rbind(chesscom_dataframe,player_dataframe)
    }
  }

  return(chesscom_dataframe)

}
