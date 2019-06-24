oddsTable <- function(model.x){
  modelSummary <- summary(model.x)
  alpha = 0.05
  modelConfint <- confint(model.x, level=1-alpha)
  Odds_Ratio <- exp(modelSummary$coefficients[,1])
  LowerCL <- exp(modelConfint[,1])
  UpperCL <- exp(modelConfint[,2])
  P_value <- modelSummary$coefficients[,"Pr(>|z|)"]
  modelTable <- cbind(Odds_Ratio, LowerCL, UpperCL, P_value)
  modelTable <- round(modelTable, 3)
  modelTable
  return(modelTable)
}
