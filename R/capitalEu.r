#' @title  Get European Country Capial City
#' @description capitalEu provides functionality which enables to get capital city of some european countries
#' @param country country
#' @param displayWay way of displaying results, possible options: lower, upper, mix
#' @return capital city of given country
#' @export
#' @importFrom data.table data.table
#' @examples
#' capitalEu('Spain', 'mix')
#'
capitalEu <- function(country, displayWay){

  vData <- data.table(COUNTRY = c('TURKEY',	'GREECE',	'BULGARIA',	'MACEDONIA',	'SERBIA',	'ROMANIA',	'MOLDOVA',	'MONTENEGRO',	'ALBANIA',	'BOSNIA & HERZEGOVINA',	'CROATIA',	'SLOVENIA',	'ITALY',	'AUSTRIA',	'HUNGARY',	'SLOVAKIA',	'POLAND',	'UKRAINE',	'BELARUS',	'RUSSIA',	'GEORGIA',	'AZERBAIJAN',	'ARMENIA',	'ESTONIA',	'LITHUANIA',	'LATVIA',	'FINLAND',	'SWEDEN',	'NORWAY',	'DENMARK',	'GERMANY',	'SWITZERLAND',	'CZECH Rep.',	'LIECHTENSTEIN',	'SAN MARINO',	'NETHERLANDS',	'BELGIUM',	'LUXEMBOURG',	'FRANCE',	'SPAIN',	'PORTUGAL',	'UNITED KINGDOM',	'IRELAND',	'MALTA',	'ICELAND',	'VATICAN',	'MONACO'))
  vData[, CAPITAL:= c('ANKARA',	'ATHENS',	'SOFIA',	'SKOPJE',	'BELGRADE',	'BUCHAREST',	'CHISINAU',	'PODGORICA',	'TIRANE',	'SARAJEVO',	'ZAGREB',	'LJUBLJANA',	'ROME',	'VIENNA',	'BUDAPEST',	'BRATISLAVA',	'WARSAW',	'KIEV',	'MINSK',	'MOSCOW',	'TBILISI',	'BAKU',	'YEREVAN',	'TALLINN',	'VILNIUS',	'RIGA',	'HELSINKI',	'STOCKHOLM',	'OSLO',	'COPENHAGEN',	'BERLIN',	'BERN',	'PRAGUE',	'VADUZ',	'SAN MARINO',	'AMSTERDAM',	'BRUSSELS',	'LUXEMBOURG',	'PARIS',	'MADRID',	'LISBON',	'LONDON',	'DUBLIN',	'VALLETTA',	'REYKJAVIK',	'VATICAN CITY',	'MONACO')]

  if (length(country) > 1) stop("There is a possibility to check for only one country")
  countryUp <- toupper(country)
  vRawCapital <- vData[COUNTRY %in% countryUp, CAPITAL]

  if (identical(vRawCapital, character(0))) stop((paste0("There is no country like ", country)))
  vCapital <- if(displayWay == "lower"){
    tolower(vRawCapital)
  } else if(displayWay == "upper"){
    toupper(vRawCapital)
  } else if (displayWay == "mix"){
    vSmall <- seq(1, nchar(vRawCapital), 2)
    vRawCapital <- unlist(base::strsplit(vRawCapital, split = ""))
    vRawCapital[vSmall] <- tolower(vRawCapital[vSmall])
    vRawCapital
  } else stop("Three possible arguments are strings: lower, upper, mix")

  vCapital
}

