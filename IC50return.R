ic50return<-function(df, ic = 50) {
  doseResponse <- drm(data = df, pctnorm ~ dose, fct = LL.4(fixed = c(NA, NA, NA, NA), names=c("H","E0","top", "EC50")))
  E0 <- doseResponse$coefficients[2]
  top <- doseResponse$coefficients[3]
  EC50 <- doseResponse$coefficients[4]
  H <-  -doseResponse$coefficients[1]
  IC50 <- exp(log((((top - E0) / (ic - E0) ) - 1) * EC50**(-H))/ (-H)) %>% 
    as.numeric()
  if (IC50 == Inf) {
    IC50 <- NA
  }
  return(IC50)
}

ic50return.3param<-function(df, ic = 50) {
  doseResponse <- drm(data = df, pctnorm ~ dose, fct = LL.4(fixed = c(NA, NA, 100, NA), names=c("H","E0","top", "EC50")))
  E0 <- doseResponse$coefficients[2]
  top <- 100
  EC50 <- doseResponse$coefficients[3]
  H <-  -doseResponse$coefficients[1]
  IC50 <- exp(log((((top - E0) / (ic - E0) ) - 1) * EC50**(-H))/ (-H)) %>% 
    as.numeric()
  if (IC50 == Inf) {
    IC50 <- NA
  }
  return(IC50)
}


for(i in drugs) {
  for (j in cells) {
    tempData <- normDF %>% 
      filter(drug == i, cell_id == j)
    ic50 <- tryCatch(expr = ic50return.3param(tempData), 
                     error = function(e) {return(NA)})
    ic90 <- tryCatch(expr = ic50return.3param(tempData, ic = 10), 
                     error = function(e) {return(NA)})
    tempDF <- data.frame(drug = i, cell_id = j, IC50 = ic50, IC90 = ic90)
    ic50df <- rbind(ic50df, tempDF)
  }
}


  ggplot(aes(x = dose, y = pctnorm, color = NRAS_status, group = as.factor(cell_id))) +
  geom_smooth(method = "drm",
              method.args = list(fct = LL.4(fixed = c(NA, NA, 100, NA), names=c("H","E0","top", "EC50"))), 
              se = FALSE, linewidth = 0.5) + 
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10), 
                     breaks = c(0,10,100,1000,10000, 100000)) + 
  #stat_summary(geom = "errorbar", width = 0.1, fun.data = "mean_cl_boot") + 
  facet_wrap(~origin) + 
  coord_cartesian(ylim = c(0,150), xlim = c(5000, 100000)) + 
  labs(y = "Cell Percent Vehicle") + 
  geom_vline(xintercept = 20000, linetype = "dotted", color = "black") + 
  geom_vline(xintercept = 35000, linetype = "dotted", color = "black") + 
  theme_bw()
