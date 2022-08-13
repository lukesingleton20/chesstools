#' Retrieve Lichess Games
#'
#' This function retrieves the bullet, blitz, rapid and classical rated games for a single player
#'
#' @param player A Lichess username
#'
#' @return A dataframe where all elements are characters
#' @import httr
#' @export
#'
#' @examples
retrieve_lichess_games <- function(player){

  # first, we do a cursory check that player is a valid Lichess username

  if(httr::GET(paste0("https://lichess.org/@/",player))$status_code == 404){
    stop("This Lichess username does not exist.")
  }

  # create temporary file

  tmp <- tempfile()

  # download from Lichess via API using curl to our temporary file
  # nb: surprisingly doesn't need OAuth2 login or token

  curl::curl_download(paste0("https://lichess.org/api/games/user/",player,"?rated=true&perfType=bullet,blitz,rapid,classical"), tmp)

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
  # stop the function and return an error message

  if(length(read_raw_games) == 0){
    stop("Player has played no games on Lichess.")
  }

  # the next stage is to convert to a character vector that contains a game per element
  # rather than a line per element (i.e. each game within read_raw_games takes up 20 elements)

  # paste coverts vector to character (superfluous in this instance)
  # and then concatenates, seperating objects with \n (a new line)
  # we now have a character vector with one element [1] that contains the entire database
  # nb: we could use the dplyr %>% here, but for ease of understanding have left each operation seperate

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

  # this was also a difficult function to get to work as intended;
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

  # this feels a little 'brute force' but, given that Lichess returns games in
  # a standard PGN format with headings in a standarised format,
  # this solution SHOULD work 100% of the time

  column_headings <- sapply(final_games_list[1],function(pull_column_headings){
    pull_column_headings <- sub("\\[", "", pull_column_headings)
    pull_column_headings <- sub(" .*", "", pull_column_headings)
    return(pull_column_headings)
  })

  # second, we need to pull the row data out from final_games_list

  # using a similar function as above, we 'brute force' remove the extraneous
  # characters, only leaving the characters we require for our row data

  row_data <- lapply(final_games_list, function(pull_row_data){
    pull_row_data <- sub(".* \"", "", pull_row_data)
    pull_row_data <- sub("\".*", "", pull_row_data)
  })

  # lastly, we turn row_data and column_headings into the final dataframe

  games_dataframe <- data.frame(matrix(unlist(row_data),
                                       nrow = length(row_data),
                                       byrow=TRUE),
                                stringsAsFactors=FALSE)

  # we give the dataframe the column_headings extracted previously

  colnames(games_dataframe) <- column_headings

  # we clear any automatically given row names from the data.frame function

  rownames(games_dataframe) <- c()

  # I'm aware of the debate around whether or not you should explicitly return output in R,
  # given that the output is implicitly returned but, given I'm probably a little old school,
  # I opted for the explicit return

  return(games_dataframe)
}
