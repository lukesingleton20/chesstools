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

  month <- as.integer(format(Sys.Date(),"%m"))
  year <- as.integer(format(Sys.Date(),"%Y"))
  if(month < 10){
    adj_month <- (paste0("0",month))
  } else {
    adj_month <- month
  }

  tmp <- tempfile()
  rating_history <- data.frame(matrix(ncol = 2))
  colnames(rating_history) <- c("Date", "Rating")
  current_row <- 1

  while(httr::GET(paste0("https://www.ecfrating.org.uk/v2/new/api.php?v2/ratings/S/",player_id,"/",year,"-",adj_month,"-01"))$status_code != 404){
    curl::curl_download(paste0("https://www.ecfrating.org.uk/v2/new/api.php?v2/ratings/S/",player_id,"/",year,"-",adj_month,"-01"),tmp)
    player_rating <- suppressWarnings(readLines(tmp))

    # extract player rating information as integer
    player_rating <- as.integer(str_extract(player_rating,"(?<=original_rating\":).*?(?=,\")"))

    # if the rating is in the old style, convert to new style
    if(year < 2020 | (year == 2020 & month < 8)){
      player_rating <- as.integer(round((player_rating * 7.5) + 700),digits=0)
    }

    rating_history[current_row,1] <- paste0("01/",adj_month,"/",year)
    rating_history[current_row,2] <- player_rating

    if(month == 1){
      month <- 12
      year <- year-1
    } else {
      month <- month-1
    }
    if(month < 10){
      adj_month <- (paste0("0",month))
    } else {
      adj_month <- month
    }

    current_row = current_row+1
  }

  rating_history$Date <- as.Date(rating_history$Date,format="%d/%m/%Y",origin="1899-12-30")

  return(rating_history)
}
