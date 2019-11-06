#' this function takes a data.frame column with proportions (possibly from prop.table) and returns a formatted  column with percentages, including the % sign
#' 
#' @param accepts a data.frame column or a vector, whether to include the % symbol and whether there should be a space between the number and the % symbol
#' @return returns a formatted string column or a string vector with the % sign
#' @export
procentuj <- function(column, percent_symbol=TRUE, round_to=2, sep=" "){
  return(paste(round(column, round_to),c("%"), sep=sep))
}