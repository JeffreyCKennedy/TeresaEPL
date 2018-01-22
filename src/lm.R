# Linear Regression -------------------------------------------------------

# using EPL motivation:
fit_EPL <- lm(WE ~ Age + tenure_yrs + MOT_E + MOT_P + MOT_L, data=CarPref)
fit_EPL_comp <- lm(WE ~ Age + tenure_yrs + MOT_PAI + MOT_PSN + MOT_PCL + 
                       MOT_LAI + MOT_LSN + MOT_LNCL + MOT_ESN + MOT_ECL + 
                       MOT_EAI, data = CarPref)

# fit_c1 <- lm(formula = WE ~ tenure_yrs, data = CarPref)
# fit_c2 <- lm(formula = WE ~ Age, data = CarPref)
fit_c12 <- lm(formula = WE ~ Age + tenure_yrs, data = CarPref)
#fit0 <- lm(formula = WE ~ PEFIT, data = CarPref)
fit1 <- lm(formula = WE ~ Age + tenure_yrs + PEFIT, data = CarPref)
fit2 <- lm(formula = WE ~ Age + tenure_yrs + PEFIT + FIT_EPL, data = CarPref)

fit3 <- lm(formula = WE ~ Age + tenure_yrs + PEFIT_VC +PEFIT_NSPJ + PEFIT_DAPJ, data = CarPref)
fit4 <- lm(formula = WE ~ Age + tenure_yrs + PEFIT_VC +PEFIT_NSPJ + PEFIT_DAPJ + FIT_E + FIT_P + 
               FIT_L, data = CarPref)


fit_js1 <- lm(formula = JS ~ PEFIT + FIT_EPL, data = CarPref)
fit_js2 <- lm(formula = JS ~ PEFIT_VC +PEFIT_NSPJ + PEFIT_DAPJ + FIT_E + FIT_P + 
                  FIT_L, data = CarPref)

fit_to_c12 <- lm(formula = TO ~ Age + tenure_yrs, data = CarPref)
fit_to1 <- lm(formula = TO ~ Age + tenure_yrs + PEFIT, data = CarPref)
fit_to2 <- lm(formula = TO ~ Age + tenure_yrs + PEFIT + FIT_EPL, data = CarPref)

fit_to3 <- lm(formula = TO ~ Age + tenure_yrs + PEFIT_VC +PEFIT_NSPJ + PEFIT_DAPJ, data = CarPref)
fit_to4 <- lm(formula = TO ~ Age + tenure_yrs + PEFIT_VC +PEFIT_NSPJ + PEFIT_DAPJ + FIT_E + FIT_P + 
                  FIT_L, data = CarPref)

# Use sjPlot to produce nice output
sjt.lm(fit_c12, fit1, fit2, 
       pred.labels = "", 
       separate.ci.col = FALSE,
       show.std = "std",
       show.fstat = TRUE,
       file = "./output/Regression Table.html")

sink(file.path("./output/Regression Summaries.txt"))
cat("\n\n\nEngagement on Age & Tenure (Yrs):\n")
summary(fit_c12)
lm.beta(fit_c12)
cat("\n\n\nEngagement on Age, Tenure & PEFIT:\n")
summary(fit1)
lm.beta(fit1)
cat("\n\n\nEngagement on Age, Tenure, PEFIT & EPL Fit:\n")
summary(fit2)
lm.beta(fit2)
cat("\n\n\nTest of improvement in model at each stage:\n")
anova(fit_c12, fit1, fit2)

cat("\n\n\nEngagement on Age, Tenure, & E, P, & L Motivation:\n")
summary(fit_EPL)
lm.beta(fit_EPL)
cat("\n\n\nEngagement on Age, Tenure, & E, P, & L Mot components:\n")
summary(fit_EPL_comp)
lm.beta(fit_EPL_comp)

cat("\n\n\nEngagement on controls and PEFit components:\n")
summary(fit3)
lm.beta(fit3)
cat("\n\n\nEngagement on controls, PEFit components, & EPL Fit components:\n")
summary(fit4)
lm.beta(fit4)
cat("\n\n\nTest of improvement in model at each stage:\n")
anova(fit_c12, fit3, fit4)

cat("\n\n\nTurnover on Age & Tenure (Yrs):\n")
summary(fit_to_c12)
lm.beta(fit_to_c12)
cat("\n\n\nTurnover on Controls & PEFIT:\n")
summary(fit_to1)
lm.beta(fit_to1)
cat("\n\n\nTurnover on Controls, PEFIT and EPL Fit:\n")
summary(fit_to2)
lm.beta(fit_to2)
cat("\n\n\nTest of improvement in model at each stage:\n")
anova(fit_to_c12, fit_to1, fit_to2)

cat("\n\n\nTurnover on controls and PEFit components:\n")
summary(fit_to3)
lm.beta(fit_to3)
cat("\n\n\nTurnover on controls, PEFit components, & EPL Fit components:\n")
summary(fit_to4)
lm.beta(fit_to4)
cat("\n\n\nTest of improvement in model at each stage:\n")
anova(fit_to_c12, fit_to3, fit_to4)

cat("\n\n\nJob Sat on PEFIT and EPL Fit:\n")
summary(fit_js1)
lm.beta(fit_js1)
cat("\n\n\nJob Sat on Fit components:\n")
summary(fit_js2)
lm.beta(fit_js2)

sink()

# To print regression diagnostic plots, as per p. 184 Kabacoff 2nd Ed.
# And using ideas from https://www.r-bloggers.com/automatically-save-your-plots-to-a-folder/

plot_names <- c("fit2", "fit4", "fit_to2", "fit_to4")
plot_list <- list(fit2, fit4, fit_to2, fit_to4)

for(i in 1:length(plot_list)){
    plot_path <- file.path("./output/",paste("RegressionDiaganostic_",plot_names[i], ".jpg", sep = ""))
    jpeg(file=plot_path)
    plot_title = paste("Regression Diagnostic Plot for ", plot_names[i])
    par(mfrow=c(2,2))
    plot(plot_list[[i]], main = plot_title)
    dev.off()
}
par(mfrow=c(1,1))



