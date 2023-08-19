# function to calculate the IC50 of the input data frame.
#
# input df must have columns named dose and response.
# drc.upper input options are NA (4 parameter) or a number to lock the upper limit of the drc (3 parameter)
#

library(dplyr)
library(drc)

ic50return<-function(drdf, ic = 50, drc.upper = NA) {
  ic <- 100-ic
  doseResponse <- tryCatch(expr = drm(data = drdf, response ~ dose, fct = LL.4(fixed = c(NA, NA, drc.upper, NA), names=c("H","E0","top", "EC50"))), 
           error = function(e) {return(NA)})
  if (is.na(doseResponse[1]) == FALSE) {
    E0 <- doseResponse$coefficients[2]
    top <- ifelse(is.na(drc.upper), doseResponse$coefficients[3], drc.upper)
    EC50 <- ifelse(is.na(drc.upper), doseResponse$coefficients[4], doseResponse$coefficients[3])
    H <-  -doseResponse$coefficients[1]
    IC50 <- exp(log((((top - E0) / (ic - E0) ) - 1) * EC50**(-H))/ (-H)) %>% 
      as.numeric()
  }
  if (IC50 == Inf || is.na(IC50)) {
    IC50 <- NA
  }
  return(IC50)
}
