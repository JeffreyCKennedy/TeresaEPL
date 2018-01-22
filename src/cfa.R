
# Confirmatory Factor Analysis --------------------------------------------

# Use cat(keys.list_5[["MOT_P"]], sep = " + ") to get RHS of model statement
# Then use notepad to edit the text document to create the lavaan model syntax
# sink("./output/CFA Model statements.txt")
# cat("\n\n\nAll items for E, P, & L:\n")
# cat(keys.list_5[["MOT_E"]], sep = " + ", fill = TRUE)
# cat(keys.list_5[["MOT_P"]], sep = " + ", fill = TRUE)
# cat(keys.list_5[["MOT_L"]], sep = " + ", fill = TRUE)
# cat("\n\n\nItems for 9 facets (AI, CL, SN):\n")
# cat(keys.list_5[["MOT_EAI"]], sep = " + ", fill = TRUE)
# cat(keys.list_5[["MOT_ESN"]], sep = " + ", fill = TRUE)
# cat(keys.list_5[["MOT_ECL"]], sep = " + ", fill = TRUE)
# cat(keys.list_5[["MOT_PAI"]], sep = " + ", fill = TRUE)
# cat(keys.list_5[["MOT_PSN"]], sep = " + ", fill = TRUE)
# cat(keys.list_5[["MOT_PCL"]], sep = " + ", fill = TRUE)
# cat(keys.list_5[["MOT_LAI"]], sep = " + ", fill = TRUE)
# cat(keys.list_5[["MOT_LSN"]], sep = " + ", fill = TRUE)
# cat(keys.list_5[["MOT_LNCL"]], sep = " + ")
# sink()


EPL.1f <- ' EPL =~ MOT6_ESN + MOT9_ECL + MOT12_ECL + MOT15_EAI + 
                   MOT18_ESN + MOT24_EAI + MOT28_ESN + MOT29_ECL + 
                   MOT30_EAI + MOT1_PAI + MOT4_PCL + MOT7_PSN + 
                   MOT10_PSN + MOT16_PCL + MOT19_PAI + MOT22_PSN + 
                   MOT27_PCL + MOT_PNEW + MOT2_LSN + MOT5_LAI + 
                   MOT8_LNCL + MOT11_LNCL + MOT14_LSN + MOT17_LNCL + 
                   MOT20_LAI + MOT23_LAI + MOT26_LSN '

EPL.3f <- ' E =~ MOT6_ESN + MOT9_ECL + MOT12_ECL + MOT15_EAI + 
                 MOT18_ESN + MOT24_EAI + MOT28_ESN + MOT29_ECL + MOT30_EAI
            P =~ MOT1_PAI + MOT4_PCL + MOT7_PSN + MOT10_PSN + MOT16_PCL + 
                 MOT19_PAI + MOT22_PSN + MOT27_PCL + MOT_PNEW
            L =~ MOT2_LSN + MOT5_LAI + MOT8_LNCL + MOT11_LNCL + 
                 MOT14_LSN + MOT17_LNCL + MOT20_LAI + MOT23_LAI + MOT26_LSN '

EPL.9f <- ' EAI =~ MOT15_EAI + MOT24_EAI + MOT30_EAI
            ESN =~ MOT6_ESN + MOT18_ESN + MOT28_ESN
            ECL =~ MOT9_ECL + MOT12_ECL + MOT29_ECL 
            PAI =~ MOT1_PAI + MOT19_PAI + MOT_PNEW
            PSN =~ MOT10_PSN + MOT7_PSN + MOT22_PSN
            PCL =~ MOT4_PCL + MOT16_PCL + MOT27_PCL
            LAI =~ MOT5_LAI + MOT20_LAI + MOT23_LAI
            LSN =~ MOT2_LSN + MOT14_LSN + MOT26_LSN
            LNC =~ MOT8_LNCL + MOT11_LNCL + MOT17_LNCL
            E   =~ EAI + ESN + ECL
            P   =~ PAI + PSN + PCL
            L   =~ LAI + LSN + LNC '


# Allow errors to covary for negative worded items ------------------------

EPL.9f.neg <- ' EAI =~ MOT15_EAI + MOT24_EAI + MOT30_EAI
            ESN =~ MOT6_ESN + MOT18_ESN + MOT28_ESN
            ECL =~ MOT9_ECL + MOT12_ECL + MOT29_ECL 
            PAI =~ MOT1_PAI + MOT19_PAI + MOT_PNEW
            PSN =~ MOT10_PSN + MOT7_PSN + MOT22_PSN
            PCL =~ MOT4_PCL + MOT16_PCL + MOT27_PCL
            LAI =~ MOT5_LAI + MOT20_LAI + MOT23_LAI
            LSN =~ MOT2_LSN + MOT14_LSN + MOT26_LSN
            LNC =~ MOT8_LNCL + MOT11_LNCL + MOT17_LNCL
            E   =~ EAI + ESN + ECL
            P   =~ PAI + PSN + PCL
            L   =~ LAI + LSN + LNC
            MOT5_LAI   ~~ MOT26_LSN + MOT11_LNCL
            MOT11_LNCL ~~ MOT26_LSN '
            

# Need to find out what estimator Ringo used.
# MLR according to Frontiers paper, though in MPlus
fit_EPL.1f <- cfa(EPL.1f, data = CarPref, missing = "listwise", estimator = "MLR")
fit_EPL.3f <- cfa(EPL.3f, data = CarPref, missing = "listwise", estimator = "MLR")
fit_EPL.9f <- cfa(EPL.9f, data = CarPref, missing = "listwise", estimator = "MLR")
fit_EPL.9f.neg <- cfa(EPL.9f.neg, data = CarPref, missing = "listwise", estimator = "MLR")

# Ringo: sorry for late reply on this. I think I took the numbers from the WLSMV
# estimation output in the slides instead of MLR…sorry as the analyses were done
# at last minute..we ran a few different estimation method to check the results.
# This explains the differences.
#
# I have rerun using MLR, the goodness of fit values are almost the same as
# yours from R. Sorry for the confusion. I will be careful in reporting the
# findings in the book chapter.


summary(fit_EPL.1f)

sink("./output/CFA Fit Measures.txt")
cat("\n\n1 factor model:\n")
fitMeasures(fit_EPL.1f, c("chisq", "chisq.scaled", "df", "cfi", "rmsea", "srmr"))
cat("\n\n3 factor model:\n")
fitMeasures(fit_EPL.3f, c("chisq", "chisq.scaled", "df", "cfi", "rmsea", "srmr"))
cat("\n\n9 1st order + 3 2nd order factors model:\n")
fitMeasures(fit_EPL.9f, c("chisq", "chisq.scaled", "df", "cfi", "rmsea", "srmr"))
cat("\n\n9 1st order + 3 2nd order factors model with errors of neg worded items covarying:\n")
fitMeasures(fit_EPL.9f.neg, c("chisq", "chisq.scaled", "df", "cfi", "rmsea", "srmr"))
sink()

# Can plot the models
library(semPlot)
semPaths(fit_EPL.1f, intercepts = FALSE, whatLabels = "std", residuals = FALSE, 
         layout = "circle")
semPaths(fit_EPL.3f, intercepts = FALSE, whatLabels = "std", residuals = FALSE, 
         layout = "circle")
semPaths(fit_EPL.9f, intercepts = FALSE, whatLabels = "std", residuals = FALSE, 
         layout = "tree2")
 
 
 
 

 
 
 



