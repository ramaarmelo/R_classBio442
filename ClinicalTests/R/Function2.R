
#' @title Project_function2
#' @description Data visualization providing a scatter plots in on panel with two rows
#' @param drugline data set encompassing variables medicine types viral load and CD4 count
#' @keywords viral load CD count data visualization sacatter plot box plot
#' @export
#' @examples 
#' MedSample()


MedSample <- function(data = drugLine){
  PlotART <- ggplot(data = drugLine, aes(x = CD_count, y = Viral_load, colour = ART_type)) + geom_point() +
    facet_grid(~ART_type)
  return(PlotART)
  
}













