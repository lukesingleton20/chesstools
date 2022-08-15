#' Retrieve Lichess Games
#'
#' This function retrieves rated games for one or more players from Lichess.
#'
#' @param player One or more Lichess usernames
#'
#' @return A dataframe where all elements are characters
#' @import httr, curl
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

    # the download from Lichess is in the form of a standardised PGN database
    # which is essentially a text file where each game is a block of text
    # starting with game information, i.e. "event", "date" etc, denoted by []
    # each [] of game information is separated by \n (a new line)
    # within each [] we want to convert the first word, i.e. Event into a dataframe column header
    # and the second string, enclosed by "", into the column entry for this row
    # after the game information follows a blank line, then the game moves on its own line
    # the game moves always start with 1. and have an indeterminate number of characters
    # there follows two blank lines before the next game entry

    # readLines converts each line of the tmp file into a character object
    # the \n character will be knocked off the end of each line and each blank line
    # will contain an empty ""

    # nb: we must first use readLines() as tmp only contains the location path of tmp in memory
    # and not the contents of tmp itself

    read_raw_games <- readLines(tmp)

    # to handle those cases where a player has played no games on Lichess,
    # return an error message instead of a null dataframe

    if(length(read_raw_games) == 0){
      return("Player has played no games on Lichess.")
    }

    # the next stage is to convert to a character vector that contains a game per element
    # rather than a line per element (i.e. each game within read_raw_games takes up 20 elements)

    # paste coverts vector to character (superfluous in this instance)
    # and then concatenates, seperating objects with \n (a new line)
    # we now have a character vector with one element [1] that contains the entire database

    collapsed_raw_games <- paste(read_raw_games, collapse = "\n")

    # three \n characters denote the end of each game and can be used to then split
    # each game into an individual element, however, the resulting format is a list within a list

    split_raw_games <- strsplit(collapsed_raw_games, "\n\n\n")

    # we can now convert, or 'flatten', the list into a character vector

    flattened_raw_games <- unlist(split_raw_games)

    # after extensive testing, there are a number of edge cases which result in a game
    # having a non-standard number of game information elements;
    # easiest to remove an entire game whilst it is in vector form here

    # "FEN" and "SetUp" only appear in games that were abandoned at the end arena tournaments
    # The absence of WhiteRatingDiff and BlackRatingDiff is an extremely rare case
    # both cases indicate a rated game was not completed or officially recorded thus
    # we can remove as we do not want it in our dataset

    cleaned_games <- flattened_raw_games[grepl("FEN",flattened_raw_games) == FALSE &
                                     grepl("SetUp",flattened_raw_games) == FALSE &
                                     grepl("WhiteRatingDiff",flattened_raw_games) == TRUE &
                                     grepl("BlackRatingDiff",flattened_raw_games) == TRUE]

    # the next stage is to create a list of games, where each element of the top-level list
    # contains a sub-list, with an element for each line of game information

    raw_games_list <- strsplit(cleaned_games, "\n")

    # now we must remove the empty elements within the sublist and those
    # elements that appear only rarely (such as the title of a FIDE titled player)
    # which cause a nonstandard number of elements

    # lapply() applies a function to every element within a list and its sublists
    # this custom function only keeps elements that are not "" (!="") and
    # those elements that do not contain either "BlackTitle" or "WhiteTitle"

    cleaned_games_list <- lapply(raw_games_list,function(remove_elements){
      remove_elements[remove_elements != "" &
                        grepl("BlackTitle",remove_elements) == FALSE &
                        grepl("WhiteTitle",remove_elements) == FALSE]
    })

    # now we must ensure the game moves element matches the rest of the elements
    # for ease of manipulation when turning it into a dataframe

    # note that ifelse() is used as it is the vectorised equivalent of if()
    # this custom function checks the first letter of each list element using substr()
    # and if the element starts with "1" (indicating it is the game move element)
    # then we paste the relevant text to standardise this element with all the others
    # note here the use of the \ which is the "escape" character that indicates a special
    # character

    final_games_list <- lapply(cleaned_games_list,function(standardised_list){
      ifelse(substr(standardised_list,1,1) == "1", paste0("[Moves \"",standardised_list,"\"]"), standardised_list)
    })

    # now we must covert final_games_list into a dataframe

    # first, we pull the column headings out from final_games_list
    # note we use sapply() here as it outputs directly into a vector, whereas
    # lapply() outputs a list
    # note that we only perform this list on the first instance of a game [1]
    # as we only require one edition of the column headings

    column_headings <- sapply(final_games_list[1],function(pull_column_headings){
      pull_column_headings <- sub("\\[", "", pull_column_headings)
      pull_column_headings <- sub(" .*", "", pull_column_headings)
      return(pull_column_headings)
    })

    # second, we need to pull the row data out from final_games_list

    # using a similar function as above, we remove the extraneous
    # characters, only leaving the characters we require for our row data

    row_data <- lapply(final_games_list, function(pull_row_data){
      pull_row_data <- sub(".* \"", "", pull_row_data)
      pull_row_data <- sub("\".*", "", pull_row_data)
    })

    # lastly, we turn row_data into the final dataframe

    games_dataframe <- data.frame(matrix(unlist(row_data),
                                         nrow = length(row_data),
                                         byrow=TRUE),
                                  stringsAsFactors=FALSE)

    # we give the dataframe the column_headings extracted previously

    colnames(games_dataframe) <- column_headings

    # we clear any automatically given row names from the data.frame function

    rownames(games_dataframe) <- c()

    return(games_dataframe)
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
#' @return A dataframe where all elements are characters.
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

    raw_games <- character(0)

    # this for loop downloads each monthly archive into a temporary file,
    # then copies it into the raw_games vector
    # nb: two extra new line characters are required after the last game as these
    # are present between games but not for the last game in a monthly archive

    # we then collapse the vector so that there is one vector with a single element
    # that contains every game, rather than each element being a set of monthly games

    count <- 0
    for(month in player_history){
      tmp <- tempfile()
      curl::curl_download(paste0(month,"/pgn"),tmp)
      read_raw_monthly_games <- readLines(tmp)
      collapsed_raw_monthly_games <- paste(read_raw_monthly_games, collapse = "\n")
      raw_games[count] <- paste0(collapsed_raw_monthly_games,"\n\n")
      count <- count + 1
    }

    # it is easiest to remove the engine evaluations here, so remove everything
    # from between the curly brackets and then restore the black move notation from ...

    removed_evalutaion <- gsub("\\{[^\\}]*\\}\\s", "", raw_games)
    hundred_postfix <- gsub("\\d\\d\\d\\.{3} ","",removed_evalutaion)
    ten_postfix <- gsub("\\d\\d\\.{3} ","",hundred_postfix)
    cleaned_raw_games <- gsub("\\d\\.{3} ","",ten_postfix)

    # three \n characters denote the end of each game and can be used to then split
    # each game into an individual element, however, the resulting format is a list within a list

    split_raw_games <- strsplit(cleaned_raw_games, "\n\n\n")

    # we can now convert, or 'flatten', the list into a character vector

    flattened_raw_games <- unlist(split_raw_games)

    # for some reason, chess.com does not store the name of the tournament in the
    # "event" game information so we'll rectify that here

    tournament_games_index <- which(grepl("[Tournament",flattened_raw_games, fixed = TRUE))

    for(game in tournament_games_index){
      tournament <- sub(".*https://www.chess.com/tournament/","",flattened_raw_games[game])
      tournament <- sub("\".*", "",tournament)
      replacement <- sub(" .*?\"\\]",paste0(" \"",tournament,"\"\\]"),flattened_raw_games[game])
      flattened_raw_games[game] <- replacement
    }

    # the next stage is to create a list of games, where each element of the top-level list
    # contains a sub-list, with an element for each line of game information

    # remove those games that were abandoned before a result was reached, removing strange Chess.com
    # variants, ensuring a first move was played and that it had a categorized opening (this avoids
    # niche cases of puzzle battles)

    cleaned_raw_games <- flattened_raw_games[grepl("abandoned",flattened_raw_games) == FALSE &
                                               grepl("[Variant",flattened_raw_games, fixed = TRUE) == FALSE &
                                               grepl("1. ",flattened_raw_games, fixed = TRUE) == TRUE &
                                               grepl("ECO", flattened_raw_games) == TRUE]

    raw_games_list <- strsplit(cleaned_raw_games, "\n")

    # now we must remove the empty elements within the sublist & extraneous game information
    # lapply() applies a function to every element within a list and its sublists
    # tournaments have been dealt with previously
    # note that "Round" always returns "-", even for tournament games, so can remove

    cleaned_games_list <- lapply(raw_games_list,function(remove_elements){
                        remove_elements[remove_elements != "" &
                        grepl("[FEN",remove_elements, fixed = TRUE) == FALSE &
                        grepl("[SetUp",remove_elements, fixed = TRUE) == FALSE &
                        grepl("[Tournament",remove_elements, fixed = TRUE) == FALSE &
                        grepl("[CurrentPosition",remove_elements, fixed = TRUE) == FALSE &
                        grepl("[Timezone",remove_elements, fixed = TRUE) == FALSE &
                        grepl("[ECOUrl",remove_elements, fixed = TRUE) == FALSE &
                        grepl("[Link",remove_elements, fixed = TRUE) == FALSE &
                        grepl("[Round",remove_elements, fixed = TRUE) == FALSE &
                        grepl("[Match",remove_elements, fixed = TRUE) == FALSE]
    })

    # now we must ensure the game moves element matches the rest of the elements
    # for ease of manipulation when turning it into a dataframe

    # this  sub-function checks the first letter of each list element using substr()
    # and if the element starts with "1" (indicating it is the game move element)
    # then we paste the relevant text to standardise this element with all the others

    final_games_list <- lapply(cleaned_games_list,function(standardised_list){
      ifelse(substr(standardised_list,1,1) == "1", paste0("[Moves \"",standardised_list,"\"]"), standardised_list)
    })

    # now we must covert final_games_list into a dataframe

    # first, we pull the column headings out from final_games_list
    # note we use sapply() here as it outputs directly into a vector, whereas
    # lapply() outputs a list
    # note that we only perform this list on the first instance of a game [1]
    # as we only require one edition of the column headings

    column_headings <- sapply(final_games_list[1],function(pull_column_headings){
      pull_column_headings <- sub("\\[", "", pull_column_headings)
      pull_column_headings <- sub(" .*", "", pull_column_headings)
      return(pull_column_headings)
    })

    # second, we need to pull the row data out from final_games_list

    # using a similar function as above, we remove the extraneous
    # characters, only leaving the characters we require for our row data

    row_data <- lapply(final_games_list, function(pull_row_data){
      pull_row_data <- sub(".* \"", "", pull_row_data)
      pull_row_data <- sub("\".*", "", pull_row_data)
    })

    # lastly, we turn row_data into the final dataframe

    games_dataframe <- data.frame(matrix(unlist(row_data),
                                         nrow = length(row_data),
                                         byrow=TRUE),
                                  stringsAsFactors=FALSE)

    # we give the dataframe the column_headings extracted previously

    colnames(games_dataframe) <- column_headings

    # we clear any automatically given row names from the data.frame function

    rownames(games_dataframe) <- c()

    return(games_dataframe)

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
