#' PGN to Dataframe
#'
#'Converts a PGN to a standardised dataframe.
#'
#' @param input_pgn
#'
#' @return dataframe
#' @import stringr, stringi, readr
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
pgn_to_dataframe <- function(input_pgn){

  # first, we want to standardise the pgn database by removing all carriage returns
  # and new lines; then, we want to split the database into individual games, using
  # the fact that pgns always start with "[Event "; we remove the first instance of
  # "[Event " as we're otherwise left with an empty first element after strsplit();
  # we then 'flatten' the list into a vector so that we can perform a further
  # strsplit() to create a list per game, with a sublist for each element of game
  # information; whilst the pgn database is flattened, however, we want to restore
  # any whitespace to a single space.
  pgn_list <- readr::read_file(input_pgn) %>%
              stringr::str_replace_all(.,"\\r"," ") %>%
              stringr::str_replace_all(.,"\\n"," ") %>%
              stringr::str_replace(.,"\\[Event \"","") %>%
              strsplit(.,"\\[Event \"") %>%
              unlist(.) %>%
              stringr::str_replace_all(.,"\\s+"," ") %>%
              strsplit(., "\"\\]")

  # we now ensure that we standardise all the elements, i.e. add the 'Moves'
  # prefix to the move element (which is always the last element of a pgn) and
  # restore the 'Event' prefix
  count = 1
  while(count <= length(pgn_list)){
    pgn_list[[count]][[1]] <- paste0("[Event \"",pgn_list[[count]][[1]])
    moves_element <- lengths(pgn_list[count])
    pgn_list[[count]][[moves_element]] <- paste0("[Moves \"",pgn_list[[count]][[moves_element]])
    count <- count + 1
  }

  # create the empty dataframe and populate the standardised headings
  lookup_table <- c("Event","Site","Date","Time","EventType","EventRounds","TimeControl",
                    "WhiteTeam","WhiteTeamCountry","BlackTeam","BlackTeamCountry",
                    "Section","Stage","Round","Board","White","WhiteTitle", "WhiteElo",
                    "Black","BlackTitle","BlackElo","ECO","PlyCount","Result","Termination","Moves")
  pgn_dataframe <- data.frame(matrix(ncol = length(lookup_table), nrow = length(pgn_list)),stringsAsFactors = FALSE)
  colnames(pgn_dataframe) <- lookup_table

  # as the format of pgns are not universal, we need to turn them into the
  # standardised format used by otbchess.
  game_index <- 1
  for(game in pgn_list){

    # we declare utc_time here, so it can be used in the nested while loop below
    utc_time <- character()

    game_info_index <- 1
    while(game_info_index <= lengths(pgn_list[game_index])){
      # extract the entire string from the current element
      pgn_game_info <- pgn_list[[game_index]][[game_info_index]]
      # pull the game information type
      column_lookup <- stringr::str_extract(pgn_game_info,"\\w+")
      # and match it against the dataframe column headings, to get the number
      # of the column we want to copy this string to; it returns 0 if it cannot
      # find an equivalent column heading in the dataframe
      column_number <- match(column_lookup,lookup_table,0)

      # if this element has an equivalent heading in the dataframe
      if(column_number != 0){

        # often the 'moves' element of a pgn can contain a lot of extraneous
        # information put there by software (i.e. variations, comments, arrows etc.)
        # we have to loop over the element until we have removed all brackets from
        # comments or variations in order to remove pesky nested brackets
        if(column_number == match("Moves",lookup_table)){

          while(grepl("\\(|\\)|\\{|\\}",pgn_game_info) == TRUE){
            pgn_game_info <- stringr::str_replace_all(pgn_game_info,"\\([^\\(]*?\\)"," ") %>%
                             stringr::str_replace_all(.,"\\{[^\\{]*?\\}"," ")
          }
          # this removes information such as arrows
          pgn_game_info <- stringr::str_replace_all(pgn_game_info,"\\$\\d{1,2}"," ") %>%
          # this removes extended black notation (i.e. 7...) that is added around
          # comments and variations
                           stringr::str_replace_all(.,"\\d{1,3}\\.{3}"," ") %>%
          # we then restore any extraneous whitespace to a single space
                           stringr::str_replace_all(.,"\\s+"," ")
        }

        # for the final input into the dataframe element, we remove the game
        # information prefix (i.e. "[Event \")
        final_input <- sub(".* \"","",pgn_game_info)

        # trimming the whitespace from both the front and back ensures a tidy entry
        final_input <- trimws(final_input)

        # copy the final input to the relevent element of the dataframe
        pgn_dataframe[game_index,column_number] <- final_input
      }

      # if there is "UTCTime" game information, we extract it here in case we are
      # unable to find the more standard "Time" game information
      if(grepl("[UTCTime \"",pgn_game_info,fixed=TRUE) == TRUE){
        utc_time <- stringr::str_extract(pgn_game_info,"(?<=\\[UTCTime \").*")
      }

      game_info_index <- game_info_index + 1
    }

    # now we carry out some final checks on each row of data

    # if we have been unable to find "Time" game information but have been able to find "UTCTime"
    # information, we use that instead
    if(length(utc_time) > 0 & is.na(pgn_dataframe[game_index,"Time"]) == TRUE){
      pgn_dataframe[game_index,"Time"] <- utc_time
    }

    # if a game within the PGN has no "PlyCount" information listed, we can pull
    # this from the "Moves" element
    if(is.na(pgn_dataframe[game_index, "PlyCount"]) == TRUE){
      ply_count <- as.integer(stringi::stri_extract_last_regex(pgn_dataframe[game_index,"Moves"],"(?<=\\s)\\d{1,}(?=\\.)"))

      # if there is no black move then there should be an uneven number for PlyCount
      # which is the number of "half moves"
      if(grepl(paste0(ply_count,"\\.\\s\\S*\\s\\S*\\s"),pgn_dataframe[game_index,"Moves"])){
        ply_count <- ply_count*2
      } else {
        ply_count <- (ply_count*2)-1
      }
      pgn_dataframe[game_index,"PlyCount"] <- ply_count
    }

    # standardise input for "Termination", first by checking for keywords already provided
    if(grepl("Abandoned|abandoned", pgn_dataframe[game_index, "Termination"]) == TRUE){
      pgn_dataframe[game_index, "Termination"] <- "Abandoned"
    } else if(grepl("time",pgn_dataframe[game_index, "Termination"],ignore.case=T) == TRUE){
      pgn_dataframe[game_index, "Termination"] <- "Won on time"
    } else if(grepl("insufficient",pgn_dataframe[game_index, "Termination"],ignore.case=T) == TRUE){
      pgn_dataframe[game_index, "Termination"] <- "Drawn due to insufficient material"
    } else if(grepl("repetition",pgn_dataframe[game_index, "Termination"],ignore.case=T) == TRUE){
      pgn_dataframe[game_index, "Termination"] <- "Drawn by repetition"
    } else if(grepl("agreement",pgn_dataframe[game_index, "Termination"],ignore.case=T) == TRUE){
      pgn_dataframe[game_index, "Termination"] <- "Drawn by agreement"
    } else if(grepl("50|fifty",pgn_dataframe[game_index, "Termination"]) == TRUE){
      pgn_dataframe[game_index, "Termination"] <- "Drawn by 50-move rule"
    } else if(grepl("stalemate",pgn_dataframe[game_index, "Termination"],ignore.case=T) == TRUE){
      pgn_dataframe[game_index, "Termination"] <- "Drawn by stalemate"
    } else if(grepl("resignation",pgn_dataframe[game_index, "Termination"],ignore.case=T) == TRUE){
      pgn_dataframe[game_index, "Termination"] <- "Won by resignation"
    } else if(grepl("checkmate",pgn_dataframe[game_index, "Termination"],ignore.case=T) == TRUE){
      pgn_dataframe[game_index, "Termination"] <- "Won by checkmate"
    } else {

      #if there are no keywords present, determine from "Moves" how the game was terminated

      # if there is less than seven characters after the first move, the game was abandoned
      # i.e. if "Moves" contains "1-0" or "1. e4 1-0"
      if(grepl("1.{7}.*",pgn_dataframe[game_index, "Moves"]) == FALSE){
        pgn_dataframe[game_index, "Termination"] <- "Abandoned"

        # if a decisive result is present (i.e. 1-0 or 0-1)
      } else if(grepl("1-0|0-1",pgn_dataframe[game_index, "Moves"]) == TRUE){

        # check for the symbol denoting checkmate
        if(grepl("#",pgn_dataframe[game_index, "Moves"]) == TRUE){
          pgn_dataframe[game_index, "Termination"] <- "Won by checkmate"
        } else {

          # if the game was decisive but did not end by checkmate, then it either
          # ended due to timeout or resignation - most chess software and websites
          # indicate a timeout as a special case and mention the keyword "time"
          # so are captured above, which means we are left with resignation as
          # the only remaining type of termination result here
          pgn_dataframe[game_index, "Termination"] <- "Won by resignation"
        }
      } else {

        # if none of the above are true, then the game was drawn - unfortunately,
        # it is impossible to determine whether the game was drawn by agreement,
        # three-fold repetition, the fifty-move rule, insufficient material or stalemate -
        # the best we can do is declare "(reason unspecified)"
        pgn_dataframe[game_index, "Termination"] <- "Drawn (reason unspecified)"
      }
    }

    game_index <- game_index + 1
  }

  # finally we ensure that each column is converted to the correct data type, we
  # suppress warnings, otherwise a warning message will appear when there is an NA
  # present within the column

  # note that dates here are optimised for windows, especially excel
  pgn_dataframe$Date <- suppressWarnings(as.Date(pgn_dataframe$Date,tryFormats=c("%d/%m/%y","%d/%m/%Y",
                                                                                 "%d.%m.%y","%d.%m.%Y",
                                                                                 "%y/%m/%d","%Y/%m/%d",
                                                                                 "%y.%m.%d","%Y.%m.%d",
                                                                                 "%m/%d/%y","%m/%d/%Y",
                                                                                 "%m.%d.%y","%m.%d.%Y"),
                                                 origin="1899-12-30"))
  pgn_dataframe$EventRounds <- suppressWarnings(as.integer(pgn_dataframe$EventRound))
  pgn_dataframe$Round <- suppressWarnings(as.integer(pgn_dataframe$Round))
  pgn_dataframe$Board <- suppressWarnings(as.integer(pgn_dataframe$Board))
  pgn_dataframe$WhiteElo <- suppressWarnings(as.integer(pgn_dataframe$WhiteElo))
  pgn_dataframe$BlackElo <- suppressWarnings(as.integer(pgn_dataframe$BlackElo))
  pgn_dataframe$PlyCount <- suppressWarnings(as.integer(pgn_dataframe$PlyCount))

  return(pgn_dataframe)
}
