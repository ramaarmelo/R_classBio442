#' @title Project_function3
#' @description Data visualization smultaneously as scatter plots and boxplots
#' @param drugline data set encompassing variables medicine types viral load and CD4 count
#' @keywords viral load CD count data visualization sacatter plot box plot
#' @export
#' @examples 
#' MedSample()

DrugsVisual <- function(data = drugLine){
  P1<- ggplot(data = drugLine, aes(x = CD_count, y = Viral_load, colour = ART_type)) + geom_point() +
    facet_wrap(~ART_type, nrow = 1, scales = "free")
  P2<- ggplot(data = drugLine, aes(y = Viral_load, colour = ART_type)) + geom_boxplot() +
    facet_wrap(~ART_type, nrow = 1)
  P3<- ggarrange(P1, P2, nrow = 2)
  return(P3)
}


