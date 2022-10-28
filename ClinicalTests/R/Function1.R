
#' @title Project_function1
#' @description Simultaneously calculates incidence prevalence sensitivity specificity positive and negative predictive values 
#' @param data data used to apply the function
#' @param Sc the school's name where the outbreak is going on
#' @param N population symptomatic people detected through a syndromic algorithm
#' @param Ti time frame between the onset of the out break to the time when the diagnostic is made
#' @param TP true positive 
#' @param FP False positive
#' @param TN true negative
#' @param FN false negative
#' @param P general population in the school
#' @param PPV positive predictive value
#' @param NPV negative predictive value
#' @keywords viral load CD count data visualization sacatter plot box plot
#' @export
#' @examples 
#' Disea_description()

Disea_description <- function(data = ProjectData, Sc = "School", N = "Symptomatic", Ti = "Time_frame",
                              TP = "True_positive", FP = "False_positive",
                              TN = "True_negative", FN = "False_negative", P = "Population",
                              PPV = "PositivePV",
                              NPV = "NegatPV"){
  School <- data[,Sc]
  Incidence <- data[,N]/data[,P]*data[,Ti]
  Prevalence <- data[,N]/data[,P]
  Sensitivity <- data[,TP]/data[,FN]+data[,TP]
  Specificity <- data[,TN]/data[,TN]+data[,FP]
  PositivePV <- data[,TP]/data[,TP]+data[,FP]
  NegativePV <- data[,TN]/data[,TN]+data[,FN]
  return(data.frame( School = School, Incidence = Incidence, Prevalence = Prevalence,
                     Sensitivity = Sensitivity, Specificity = Specificity, PositivePV = PositivePV,
                     NegativePV = NegativePV))
  
}


