#' PGN to Dataframe
#'
#'Converts a PGN to a standardised dataframe.
#'
#' @param input_pgn
#'
#' @return dataframe
#' @import stringr
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
  removed_carriage_pgn <- stringr::str_replace_all(input_pgn,"\\r"," ")
  removed_lines_pgn <- stringr::str_replace_all(removed_carriage_pgn,"\\n"," ")
  removed_first_event_pgn <- stringr::str_replace(removed_lines_pgn,"\\[Event \"","")
<<<<<<< HEAD
  split_pgn <- strsplit(removed_first_event_pgn,"\\[Event \"")
=======
  split_pgn <- stringr::strsplit(removed_first_event_pgn,"\\[Event \"")
>>>>>>> 1fe2a81f30e495fb59aa728351bf105bf0301922
  flattened_pgn <- unlist(split_pgn)
  restored_whitespace_pgn <- stringr::str_replace_all(flattened_pgn,"\\s+"," ")
  pgn_list <- strsplit(restored_whitespace_pgn, "\"\\]")

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
  lookup_table <- c("Event","EventType","EventRounds","Site","Date","Time",
                    "TimeControl","WhiteTeam","BlackTeam","Section","Round","Board","WhiteTitle",
                    "White", "WhiteElo","BlackTitle","Black","BlackElo",
                    "ECO","PlyCount","Result","Termination","Moves")
  pgn_dataframe <- data.frame(matrix(ncol = length(lookup_table), nrow = length(pgn_list)),stringsAsFactors = FALSE)
  colnames(pgn_dataframe) <- lookup_table

  # as the format of pgns are not universal, we need to turn them into the
  # standardised format used by otbchess.
  game_index <- 1
  for(game in pgn_list){

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
        # comments or variations as to remove nested brackets

        if(column_number == match("Moves",lookup_table)){

          while(grepl("\\(|\\)|\\{|\\}",pgn_game_info) == TRUE){
            pgn_game_info <- stringr::str_replace_all(pgn_game_info,"\\([^\\(]*?\\)"," ")
            pgn_game_info <- stringr::str_replace_all(pgn_game_info,"\\{[^\\{]*?\\}"," ")
          }
          # this removes information such as arrows
          pgn_game_info <- stringr::str_replace_all(pgn_game_info,"\\$\\d{1,2}"," ")
          # this removes extended black notation (i.e. 7...) that is added around
          # comments and variations
          pgn_game_info <- stringr::str_replace_all(pgn_game_info,"\\d{1,3}\\.{3}"," ")
          # we then restore any extraneous whitespace to a single space
          pgn_game_info <- stringr::str_replace_all(pgn_game_info,"\\s+"," ")
        }

        # for the final input into the dataframe element, we remove the game
        # information prefix (i.e. "[Event \")
        final_input <- sub(".* \"","",pgn_game_info)
        # trimming the whitespace from both the front and back ensures a tidy entry
        final_input <- trimws(final_input)
        # copy the final input to the relevent element of the dataframe
        pgn_dataframe[game_index,column_number] <- final_input
      }

      game_info_index <- game_info_index + 1
    }

    game_index <- game_index + 1
  }
}
