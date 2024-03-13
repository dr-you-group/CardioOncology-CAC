library(dplyr)
results <- read.csv("./")

#### Stroke CAC1 #### 
stroke_CAC1 <- results %>% filter(subgroup == "CAC1" & outcomes == "stroke")

stroke_CAC1_results <- meta::metagen(log(stroke_CAC1$hr),
                                     lower=log(stroke_CAC1$lb), 
                                     upper = log(stroke_CAC1$ub), 
                                     sm="HR", 
                                     hakn = FALSE, 
                                     comb.fixed = TRUE, 
                                     comb.random = FALSE,
                                     studlab = stroke_CAC1$analysis)


tiff("stroke_cac1.tiff", units = "cm", width = 20, height = 10, res = 1000)
meta::settings.meta("jama")
meta::forest.meta(stroke_CAC1_results, 
                  fontsize = 11,
                  text.common = paste0(sprintf("Overall: I\u00B2 = %.1f", stroke_CAC1_results$I2*100), "%"),
                  commmon = T,
                  random = F,
                  rightcols = F,
                  plotwitdh = "8cm",
                  spacing = 1,
                  print.pval.I2=F,
                  print.tau2 = F,
                  # print.Q	=F,
                  print.pval.Q = F,
                  hetstat = F,
                  test.overall = F,
                  just.studlab = "left",
                  just = "center",
                  #addrow.overall=TRUE,
                  xlim = c(0.1,5),
                  ref = 1,
                  #colgap.forest.left = "1cm",
                  col.square = "#000000",
                  col.diamond.common = "#6F99ADFF",
                  xlab = "            SHR (95% CI)",
                  leftlabs  = c("Source","SHR (95% CI)"),
                  at = c(0.2,1,5))
dev.off()

#### Stroke CAC2 #### 
stroke_CAC2 <- results %>% filter(subgroup == "CAC2" & outcomes == "stroke")

stroke_CAC2_results <- meta::metagen(log(stroke_CAC2$hr),
                                     lower=log(stroke_CAC2$lb), 
                                     upper = log(stroke_CAC2$ub), 
                                     sm="HR", 
                                     hakn = FALSE, 
                                     comb.fixed = T, 
                                     comb.random = F,
                                     studlab = stroke_CAC2$analysis)



tiff("stroke_CAC2.tiff", units = "cm", width = 20, height = 10, res = 1000)
meta::settings.meta("JAMA")
meta::forest.meta(stroke_CAC2_results, 
                  fontsize = 11,
                  text.common = paste0(sprintf("Overall: I\u00B2 = %.1f", stroke_CAC2_results$I2*100), "%"),
                  commmon = T,
                  random = F,
                  rightcols = F,
                  plotwitdh = "8cm",
                  spacing = 1,
                  print.pval.I2=F,
                  print.tau2 = F,
                  # print.Q	=F,
                  print.pval.Q = F,
                  hetstat = F,
                  test.overall = F,
                  just.studlab = "left",
                  just = "center",
                  #addrow.overall=TRUE,
                  xlim = c(0.1,5),
                  ref = 1,
                  #colgap.forest.left = "1cm",
                  col.square = "#000000",
                  col.diamond.common = "#6F99ADFF",
                  xlab = "            SHR (95% CI)",
                  leftlabs  = c("Source","SHR (95% CI)"),
                  at = c(0.2,1,5))
dev.off()

#### Stroke CAC3 #### 
stroke_CAC3 <- results %>% filter(subgroup == "CAC3" & outcomes == "stroke")

stroke_CAC3_results <- meta::metagen(log(stroke_CAC3$hr),
                                     lower=log(stroke_CAC3$lb), 
                                     upper = log(stroke_CAC3$ub), 
                                     sm="HR", 
                                     hakn = FALSE, 
                                     comb.fixed = T, 
                                     comb.random = F,
                                     studlab = stroke_CAC3$analysis)



tiff("stroke_CAC3.tiff", units = "cm", width = 20, height = 10, res = 1000)
meta::settings.meta("JAMA")
meta::forest.meta(stroke_CAC3_results, 
                  fontsize = 11,
                  text.common = paste0(sprintf("Overall: I\u00B2 = %.1f", stroke_CAC3_results$I2*100), "%"),
                  commmon = T,
                  random = F,
                  rightcols = F,
                  plotwitdh = "8cm",
                  spacing = 1,
                  print.pval.I2=F,
                  print.tau2 = F,
                  # print.Q	=F,
                  print.pval.Q = F,
                  hetstat = F,
                  test.overall = F,
                  just.studlab = "left",
                  just = "center",
                  #addrow.overall=TRUE,
                  xlim = c(0.1,5),
                  ref = 1,
                  #colgap.forest.left = "1cm",
                  col.square = "#000000",
                  col.diamond.common = "#6F99ADFF",
                  xlab = "            SHR (95% CI)",
                  leftlabs  = c("Source","SHR (95% CI)"),
                  at = c(0.2,1,5))
dev.off()

#### mace CAC1 #### 
mace_CAC1 <- results %>% filter(subgroup == "CAC1" & outcomes == "mace")

mace_CAC1_results <- meta::metagen(log(mace_CAC1$hr),
                                   lower=log(mace_CAC1$lb), 
                                   upper = log(mace_CAC1$ub), 
                                   sm="HR", 
                                   hakn = FALSE, 
                                   comb.fixed = TRUE, 
                                   comb.random = FALSE,
                                   studlab = mace_CAC1$analysis)



tiff("mace_cac1.tiff", units = "cm", width = 20, height = 10, res = 1000)
meta::settings.meta("jama")
meta::forest.meta(mace_CAC1_results, 
                  fontsize = 11,
                  text.common = paste0(sprintf("Overall: I\u00B2 = %.1f", mace_CAC1_results$I2*100), "%"),
                  commmon = T,
                  random = F,
                  rightcols = F,
                  plotwitdh = "8cm",
                  spacing = 1,
                  print.pval.I2=F,
                  print.tau2 = F,
                  # print.Q	=F,
                  print.pval.Q = F,
                  hetstat = F,
                  test.overall = F,
                  just.studlab = "left",
                  just = "center",
                  #addrow.overall=TRUE,
                  xlim = c(0.1,5),
                  ref = 1,
                  #colgap.forest.left = "1cm",
                  col.square = "#000000",
                  col.diamond.common = "#6F99ADFF",
                  xlab = "            SHR (95% CI)",
                  leftlabs  = c("Source","SHR (95% CI)"),
                  at = c(0.2,1,5))
dev.off()

#### mace CAC2 #### 
mace_CAC2 <- results %>% filter(subgroup == "CAC2" & outcomes == "mace")

mace_CAC2_results <- meta::metagen(log(mace_CAC2$hr),
                                   lower=log(mace_CAC2$lb), 
                                   upper = log(mace_CAC2$ub), 
                                   sm="HR", 
                                   hakn = FALSE, 
                                   comb.fixed = T, 
                                   comb.random = F,
                                   studlab = mace_CAC2$analysis)



tiff("mace_CAC2.tiff", units = "cm", width = 20, height = 10, res = 1000)
meta::settings.meta("JAMA")
meta::forest.meta(mace_CAC2_results, 
                  fontsize = 11,
                  text.common = paste0(sprintf("Overall: I\u00B2 = %.1f", mace_CAC2_results$I2*100), "%"),
                  commmon = T,
                  random = F,
                  rightcols = F,
                  plotwitdh = "8cm",
                  spacing = 1,
                  print.pval.I2=F,
                  print.tau2 = F,
                  # print.Q	=F,
                  print.pval.Q = F,
                  hetstat = F,
                  test.overall = F,
                  just.studlab = "left",
                  just = "center",
                  #addrow.overall=TRUE,
                  xlim = c(0.1,5),
                  ref = 1,
                  #colgap.forest.left = "1cm",
                  col.square = "#000000",
                  col.diamond.common = "#6F99ADFF",
                  xlab = "            SHR (95% CI)",
                  leftlabs  = c("Source","SHR (95% CI)"),
                  at = c(0.2,1,5))
dev.off()

#### mace CAC3 #### 
mace_CAC3 <- results %>% filter(subgroup == "CAC3" & outcomes == "mace")

mace_CAC3_results <- meta::metagen(log(mace_CAC3$hr),
                                   lower=log(mace_CAC3$lb), 
                                   upper = log(mace_CAC3$ub), 
                                   sm="HR", 
                                   hakn = FALSE, 
                                   comb.fixed = T, 
                                   comb.random = F,
                                   studlab = mace_CAC3$analysis)



tiff("mace_CAC3.tiff", units = "cm", width = 20, height = 10, res = 1000)
meta::settings.meta("JAMA")
meta::forest.meta(mace_CAC3_results, 
                  fontsize = 11,
                  text.common = paste0(sprintf("Overall: I\u00B2 = %.1f", mace_CAC3_results$I2*100), "%"),
                  commmon = T,
                  random = F,
                  rightcols = F,
                  plotwitdh = "8cm",
                  spacing = 1,
                  print.pval.I2=F,
                  print.tau2 = F,
                  # print.Q	=F,
                  print.pval.Q = F,
                  hetstat = F,
                  test.overall = F,
                  just.studlab = "left",
                  just = "center",
                  #addrow.overall=TRUE,
                  xlim = c(0.1,5),
                  ref = 1,
                  #colgap.forest.left = "1cm",
                  col.square = "#000000",
                  col.diamond.common = "#6F99ADFF",
                  xlab = "            SHR (95% CI)",
                  leftlabs  = c("Source","SHR (95% CI)"),
                  at = c(0.2,1,5))
dev.off()


#### CVDeath CAC1 #### 
CVDeath_CAC1 <- results %>% filter(subgroup == "CAC1" & outcomes == "CVDeath")

CVDeath_CAC1_results <- meta::metagen(log(CVDeath_CAC1$hr),
                                      lower=log(CVDeath_CAC1$lb), 
                                      upper = log(CVDeath_CAC1$ub), 
                                      sm="HR", 
                                      hakn = FALSE, 
                                      comb.fixed = TRUE, 
                                      comb.random = FALSE,
                                      studlab = CVDeath_CAC1$analysis)



tiff("CVDeath_cac1.tiff", units = "cm", width = 20, height = 10, res = 1000)
meta::settings.meta("jama")
meta::forest.meta(CVDeath_CAC1_results, 
                  fontsize = 11,
                  text.common = paste0(sprintf("Overall: I\u00B2 = %.1f", CVDeath_CAC1_results$I2*100), "%"),
                  commmon = T,
                  random = F,
                  rightcols = F,
                  plotwitdh = "8cm",
                  spacing = 1,
                  print.pval.I2=F,
                  print.tau2 = F,
                  # print.Q	=F,
                  print.pval.Q = F,
                  hetstat = F,
                  test.overall = F,
                  just.studlab = "left",
                  just = "center",
                  #addrow.overall=TRUE,
                  xlim = c(0.1,5),
                  ref = 1,
                  #colgap.forest.left = "1cm",
                  col.square = "#000000",
                  col.diamond.common = "#6F99ADFF",
                  xlab = "            SHR (95% CI)",
                  leftlabs  = c("Source","SHR (95% CI)"),
                  at = c(0.2,1,5))
dev.off()

#### CVDeath CAC2 #### 
CVDeath_CAC2 <- results %>% filter(subgroup == "CAC2" & outcomes == "CVDeath")

CVDeath_CAC2_results <- meta::metagen(log(CVDeath_CAC2$hr),
                                      lower=log(CVDeath_CAC2$lb), 
                                      upper = log(CVDeath_CAC2$ub), 
                                      sm="HR", 
                                      hakn = FALSE, 
                                      comb.fixed = T, 
                                      comb.random = F,
                                      studlab = CVDeath_CAC2$analysis)



tiff("CVDeath_CAC2.tiff", units = "cm", width = 20, height = 10, res = 1000)
meta::settings.meta("JAMA")
meta::forest.meta(CVDeath_CAC2_results, 
                  fontsize = 11,
                  text.common = paste0(sprintf("Overall: I\u00B2 = %.1f", CVDeath_CAC2_results$I2*100), "%"),
                  commmon = T,
                  random = F,
                  rightcols = F,
                  plotwitdh = "8cm",
                  spacing = 1,
                  print.pval.I2=F,
                  print.tau2 = F,
                  # print.Q	=F,
                  print.pval.Q = F,
                  hetstat = F,
                  test.overall = F,
                  just.studlab = "left",
                  just = "center",
                  #addrow.overall=TRUE,
                  xlim = c(0.1,5),
                  ref = 1,
                  #colgap.forest.left = "1cm",
                  col.square = "#000000",
                  col.diamond.common = "#6F99ADFF",
                  xlab = "            SHR (95% CI)",
                  leftlabs  = c("Source","SHR (95% CI)"),
                  at = c(0.2,1,5))
dev.off()

#### CVDeath CAC3 #### 
CVDeath_CAC3 <- results %>% filter(subgroup == "CAC3" & outcomes == "CVDeath")

CVDeath_CAC3_results <- meta::metagen(log(CVDeath_CAC3$hr),
                                      lower=log(CVDeath_CAC3$lb), 
                                      upper = log(CVDeath_CAC3$ub), 
                                      sm="HR", 
                                      hakn = FALSE, 
                                      comb.fixed = T, 
                                      comb.random = F,
                                      studlab = CVDeath_CAC3$analysis)



tiff("CVDeath_CAC3.tiff", units = "cm", width = 20, height = 10, res = 1000)
meta::settings.meta("JAMA")
meta::forest.meta(CVDeath_CAC3_results, 
                  fontsize = 11,
                  text.common = paste0(sprintf("Overall: I\u00B2 = %.1f", CVDeath_CAC3_results$I2*100), "%"),
                  commmon = T,
                  random = F,
                  rightcols = F,
                  plotwitdh = "8cm",
                  spacing = 1,
                  print.pval.I2=F,
                  print.tau2 = F,
                  # print.Q	=F,
                  print.pval.Q = F,
                  hetstat = F,
                  test.overall = F,
                  just.studlab = "left",
                  just = "center",
                  #addrow.overall=TRUE,
                  xlim = c(0.1,5),
                  ref = 1,
                  #colgap.forest.left = "1cm",
                  col.square = "#000000",
                  col.diamond.common = "#6F99ADFF",
                  xlab = "            SHR (95% CI)",
                  leftlabs  = c("Source","SHR (95% CI)"),
                  at = c(0.2,1,5))
dev.off()

#### mi CAC1 #### 
mi_CAC1 <- results %>% filter(subgroup == "CAC1" & outcomes == "mi")

mi_CAC1_results <- meta::metagen(log(mi_CAC1$hr),
                                 lower=log(mi_CAC1$lb), 
                                 upper = log(mi_CAC1$ub), 
                                 sm="HR", 
                                 hakn = FALSE, 
                                 comb.fixed = TRUE, 
                                 comb.random = FALSE,
                                 studlab = mi_CAC1$analysis)



tiff("mi_cac1.tiff", units = "cm", width = 20, height = 10, res = 1000)
meta::settings.meta("jama")
meta::forest.meta(mi_CAC1_results, 
                  fontsize = 11,
                  text.common = paste0(sprintf("Overall")),
                  commmon = T,
                  random = F,
                  rightcols = F,
                  plotwitdh = "8cm",
                  spacing = 1,
                  print.pval.I2=F,
                  print.tau2 = F,
                  # print.Q	=F,
                  print.pval.Q = F,
                  hetstat = F,
                  test.overall = F,
                  just.studlab = "left",
                  just = "center",
                  #addrow.overall=TRUE,
                  xlim = c(0.01,30),
                  ref = 1,
                  #colgap.forest.left = "1cm",
                  col.square = "#000000",
                  col.diamond.common = "#6F99ADFF",
                  xlab = "            SHR (95% CI)",
                  leftlabs  = c("Source","SHR (95% CI)"),
                  at = c(0.03,1,30),
                  lab.NA.effect= "")
dev.off()

#### mi CAC2 #### 
mi_CAC2 <- results %>% filter(subgroup == "CAC2" & outcomes == "mi")

mi_CAC2_results <- meta::metagen(log(mi_CAC2$hr),
                                 lower=log(mi_CAC2$lb), 
                                 upper = log(mi_CAC2$ub), 
                                 sm="HR", 
                                 hakn = FALSE, 
                                 comb.fixed = T, 
                                 comb.random = F,
                                 studlab = mi_CAC2$analysis)



tiff("mi_CAC2.tiff", units = "cm", width = 20, height = 10, res = 1000)
meta::settings.meta("JAMA")
meta::forest.meta(mi_CAC2_results, 
                  fontsize = 11,
                  text.common = "Overall",
                  commmon = T,
                  random = F,
                  rightcols = F,
                  plotwitdh = "8cm",
                  spacing = 1,
                  print.pval.I2=F,
                  print.tau2 = F,
                  # print.Q	=F,
                  print.pval.Q = F,
                  hetstat = F,
                  test.overall = F,
                  just.studlab = "left",
                  just = "center",
                  #addrow.overall=TRUE,
                  xlim = c(0.01,30),
                  ref = 1,
                  #colgap.forest.left = "1cm",
                  col.square = "#000000",
                  col.diamond.common = "#6F99ADFF",
                  xlab = "            SHR (95% CI)",
                  leftlabs  = c("Source","SHR (95% CI)"),
                  at = c(0.03,1,30))
dev.off()

#### mi CAC3 #### 
mi_CAC3 <- results %>% filter(subgroup == "CAC3" & outcomes == "mi")

mi_CAC3_results <- meta::metagen(log(mi_CAC3$hr),
                                 lower=log(mi_CAC3$lb), 
                                 upper = log(mi_CAC3$ub), 
                                 sm="HR", 
                                 hakn = FALSE, 
                                 comb.fixed = T, 
                                 comb.random = F,
                                 studlab = mi_CAC3$analysis)



tiff("mi_CAC3.tiff", units = "cm", width = 20, height = 10, res = 1000)
meta::settings.meta("JAMA")
meta::forest.meta(mi_CAC3_results, 
                  fontsize = 11,
                  text.common = "Overall",
                  commmon = T,
                  random = F,
                  rightcols = F,
                  plotwitdh = "8cm",
                  spacing = 1,
                  print.pval.I2=F,
                  print.tau2 = F,
                  # print.Q	=F,
                  print.pval.Q = F,
                  hetstat = F,
                  test.overall = F,
                  just.studlab = "left",
                  just = "center",
                  #addrow.overall=TRUE,
                  xlim = c(0.01,30),
                  ref = 1,
                  #colgap.forest.left = "1cm",
                  col.square = "#000000",
                  col.diamond.common = "#6F99ADFF",
                  xlab = "            SHR (95% CI)",
                  leftlabs  = c("Source","SHR (95% CI)"),
                  at = c(0.03,1,30))
dev.off()
