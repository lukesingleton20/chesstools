#' Retrieve ECF Rating
#'
#' Retrieve the all historical monthly ECF ratings of a player by using their ECF Rating Code.
#' Returned as a dataframe with two columns: Date and Rating.
#'
#' @param player_id
#'
#' @return player_rating
#' @import curl, stringr, httr
#' @export
#'
#' @examples
retrieve_ecf_ratings <- function(player_id){

  # get current month and year
  month <- as.integer(format(Sys.Date(),"%m"))
  year <- as.integer(format(Sys.Date(),"%Y"))

  # due to the requirements of the url path, if month is less than 10, we need
  # to add the front zero, i.e. turn 9 into 09
  if(month < 10){
    adj_month <- (paste0("0",month))
  } else {
    adj_month <- month
  }

  # create temporary file for use with curl_download
  tmp <- tempfile()

  # create the dataframe
  rating_history <- data.frame(matrix(ncol = 2))
  colnames(rating_history) <- c("Date", "Rating")

  # set "current row" so we can append multiple rows
  current_row <- 1

  # while the API does not return a 404 error message (i.e. that player has a
  # rating for the specific month and year)
  while(httr::GET(paste0("https://www.ecfrating.org.uk/v2/new/api.php?v2/ratings/S/",player_id,"/",year,"-",adj_month,"-01"))$status_code != 404){

    # download player rating via the ECF API
    curl::curl_download(paste0("https://www.ecfrating.org.uk/v2/new/api.php?v2/ratings/S/",player_id,"/",year,"-",adj_month,"-01"),tmp)
    player_rating <- suppressWarnings(readLines(tmp))

    # extract player rating information as integer
    player_rating <- as.integer(str_extract(player_rating,"(?<=original_rating\":).*?(?=,\")"))

    # if the rating is in the old style, convert to new style
    if(year < 2020 | (year == 2020 & month < 8)){
      player_rating <- as.integer(round((player_rating * 7.5) + 700),digits=0)
    }

    # add the relevant information to our dataframe
    rating_history[current_row,1] <- paste0("01/",adj_month,"/",year)
    rating_history[current_row,2] <- player_rating

    # cycle backwards through the months, returning to December of the previous
    # year once we hit January
    if(month == 1){
      month <- 12
      year <- year-1
    } else {
      month <- month-1
    }

    # again, to ensure the url path is correct we need to manually add a zero
    # for those months less than 10
    if(month < 10){
      adj_month <- (paste0("0",month))
    } else {
      adj_month <- month
    }

    # cycle through to the next row of the dataframe
    current_row = current_row+1
  }

  # convert the "Date" column to the date data type
  rating_history$Date <- as.Date(rating_history$Date,format="%d/%m/%Y",origin="1899-12-30")

  return(rating_history)
}
