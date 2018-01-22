# Print out reliabilities and descriptives

sink("./output/Scale psychometrics.txt")
signif(scores_7$alpha, 3) #round to 3 significant digits
cat("\n\n")
signif(scores_5$alpha, 3)
cat("\n\n")
describe(CarPref[c("WE", "JS", "TO", "LS")])
cat("\n\n")
describe(CarPref[c("MOT_P","MOT_L","MOT_E")])
cat("\n\n")
describe(CarPref[c("PEFIT", "FIT_E","FIT_P", "FIT_L", "FIT_EPL" )])
cat("\n\n")
describe(CarPref[c("PEFIT_VC","PEFIT_NSPJ", "PEFIT_DAPJ")])
sink()


# Correlations ------------------------------------------------------------

correlations <- CarPref %>% 
    select(starts_with("MOT_"), starts_with("FIT_"), starts_with("PEFIT_"), 
           one_of(c("PEFIT","WE","JS","TO","LS"))) %>% 
    corr.test()
sink("./output/Scale correlations.txt")
print(correlations)
sink()


