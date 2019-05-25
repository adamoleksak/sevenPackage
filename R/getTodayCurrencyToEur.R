#' @title  Get Daily Euro Exchange Rate
#' @description getTodayCurrencyToEur is used to get today's exchange rate for Euro
#' @param result specifies output type, table or status
#' @return returns table or status, depending on given parameter
#' @export
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom data.table as.data.table
#' @examples
#' getTodayCurrencyToEur()
#'
getTodayCurrencyToEur <- function(result = "table"){

  if (!result %in% c("table", "status")) stop ("result must be string of values table or status")

  vUrl  <- "https://api.exchangeratesapi.io/latest"
  vRawResult <- GET(url = vUrl)
  if (vRawResult$status_code != 200) stop("server did not recieved request")

  vRawResultAsChar <- rawToChar(vRawResult$content)
  vDateList <- fromJSON(vRawResultAsChar)
  vCurrencyTable <- as.data.table(vDateList$rates)

  vReturn <- if(result == 'table'){
    vCurrencyTable
  } else {
    vRawResult$status_code
  }
  return(vReturn)

}

