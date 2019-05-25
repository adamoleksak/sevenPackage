#' @title  Drop All Csv From Given Directory
#' @description deletes all csv files from given directory. Use with caution.
#' @param path directory where csv to delete are stored, as a string.
#'     Path should be ended without slash (/).
#' @return directory without csv files, console output presents names of dropped csv's.
#' @export
#' @importFrom assertthat assert_that

cleanDirectory <- function(path) {

  vCsvFiles = list.files(path = path, pattern = "*.csv")
  assert_that(length(vCsvFiles) > 0, msg = "There is no csv files in directory")
  dropFile <- function(x){
    file.remove(paste0(path,"/", x))
  }
  sapply(vCsvFiles, dropFile)

}

