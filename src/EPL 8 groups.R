# creating 8 groups based on hi/low EPL responses.

# Can use scale() to center variables (or create z scores, or subtract a 
# specified value from them)
# http://stat.ethz.ch/R-manual/R-patched/library/base/html/scale.html

# Here, using dplyr mutate to create the new z scores.

CarPref <- CarPref %>% 
    mutate(MOT_Ez = (MOT_E - mean(MOT_E)) / sd(MOT_E)) %>% 
    mutate(MOT_Pz = (MOT_P - mean(MOT_P)) / sd(MOT_P)) %>% 
    mutate(MOT_Lz = (MOT_L - mean(MOT_L)) / sd(MOT_L))


CarPref$EPL_cat <- 10 
CarPref <- within(CarPref, EPL_cat[CarPref$MOT_Ez > 0 & CarPref$MOT_Pz > 0 & CarPref$MOT_Lz > 0] <- 8)
CarPref <- within(CarPref, EPL_cat[CarPref$MOT_Ez > 0 & CarPref$MOT_Pz <= 0 & CarPref$MOT_Lz > 0] <- 7)
CarPref <- within(CarPref, EPL_cat[CarPref$MOT_Ez <= 0 & CarPref$MOT_Pz > 0 & CarPref$MOT_Lz > 0] <- 6)
CarPref <- within(CarPref, EPL_cat[CarPref$MOT_Ez > 0 & CarPref$MOT_Pz > 0 & CarPref$MOT_Lz <= 0] <- 5)
CarPref <- within(CarPref, EPL_cat[CarPref$MOT_Ez <= 0 & CarPref$MOT_Pz <= 0 & CarPref$MOT_Lz > 0] <- 4)
CarPref <- within(CarPref, EPL_cat[CarPref$MOT_Ez > 0 & CarPref$MOT_Pz <= 0 & CarPref$MOT_Lz <= 0] <- 3)
CarPref <- within(CarPref, EPL_cat[CarPref$MOT_Ez <= 0 & CarPref$MOT_Pz > 0 & CarPref$MOT_Lz <= 0] <- 2)
CarPref <- within(CarPref, EPL_cat[CarPref$MOT_Ez <= 0 & CarPref$MOT_Pz <= 0 & CarPref$MOT_Lz <= 0] <- 1)

CarPref$EPL_cat <- ordered(CarPref$EPL_cat, levels=c(1,2,3,4,5,6,7,8), 
                           labels=c("EPL Lo","P Hi","E Hi","L Hi","EP Hi","LP Hi","EL Hi","EPL Hi"))

# CarPref$EPL_cat <- "BLANK" 
# CarPref <- within(CarPref, EPL_cat[CarPref$MOT_Ez > 0 & CarPref$MOT_Pz > 0 & CarPref$MOT_Lz > 0] <- "EPL Hi")
# CarPref <- within(CarPref, EPL_cat[CarPref$MOT_Ez > 0 & CarPref$MOT_Pz <= 0 & CarPref$MOT_Lz > 0] <- "EL Hi")
# CarPref <- within(CarPref, EPL_cat[CarPref$MOT_Ez <= 0 & CarPref$MOT_Pz > 0 & CarPref$MOT_Lz > 0] <- "LP Hi")
# CarPref <- within(CarPref, EPL_cat[CarPref$MOT_Ez > 0 & CarPref$MOT_Pz > 0 & CarPref$MOT_Lz <= 0] <- "EP Hi")
# CarPref <- within(CarPref, EPL_cat[CarPref$MOT_Ez <= 0 & CarPref$MOT_Pz <= 0 & CarPref$MOT_Lz > 0] <- "L Hi")
# CarPref <- within(CarPref, EPL_cat[CarPref$MOT_Ez > 0 & CarPref$MOT_Pz <= 0 & CarPref$MOT_Lz <= 0] <- "E Hi")
# CarPref <- within(CarPref, EPL_cat[CarPref$MOT_Ez <= 0 & CarPref$MOT_Pz > 0 & CarPref$MOT_Lz <= 0] <- "P Hi")
# CarPref <- within(CarPref, EPL_cat[CarPref$MOT_Ez <= 0 & CarPref$MOT_Pz <= 0 & CarPref$MOT_Lz <= 0] <- "EPL Lo")



CarPrefz <- CarPref %>% 
    group_by(EPL_cat) %>%
    summarise(WE_mean = mean(WE, na.rm = TRUE),
    WE_sd = sd(WE, na.rm = TRUE),
    TO_mean = mean(TO, na.rm = TRUE), 
    JS_mean = mean(JS, na.rm = TRUE))

head(CarPrefz[c("EPL_cat", "WE_mean")], 10)
head(CarPrefz[c("EPL_cat", "TO_mean")], 10)
head(CarPrefz[c("EPL_cat", "JS_mean")], 10)
plot(CarPrefz[c("EPL_cat", "WE_mean")], ylim=c(3, 5))
plot(CarPrefz[c("EPL_cat", "TO_mean")], ylim=c(1.5, 3.5))
plot(CarPrefz[c("EPL_cat", "JS_mean")], ylim=c(4, 6.5))

# Cat 1 = all low       Cat 2 = P high      Cat 3 = E high      Cat 4 = L high
# Cat 5 = EP high       Cat 6 = LP high     Cat 7 = EL high     Cat 8 = EPL high

# ANOVA
sink(file.path("./output/ANOVA of 8 EPL groups.txt"))
fit_av <- aov(WE ~ EPL_cat, data=CarPref)
cat("\n\n\nOne-Way Analysis of Variance:\n")
summary(fit_av)
cat("\n\n\nTukey HSD post hoc comparisons:\n")
# Tukey Honestly Significant Differences
TukeyHSD(fit_av)
sink()


# Assessing Test Assumptions ----------------------------------------------
# p. 222 Kabacoff
qqPlot(lm(WE ~ EPL_cat, data=CarPref), simulate = TRUE, main="Q-Q Plot", labels=FALSE)
bartlett.test(WE ~ EPL_cat, data=CarPref)
# Results  indicate sig difference in variances across the groups - bad.
outlierTest(fit_av, cutoff=0.05, n.max = 10)
# Case 26 (Qstnr_number = 27) is biggest outlier.

opar <- par(no.readonly = TRUE) # Use for resetting par to defaults
# See p. 220 Kabacokff. Plot sig levels of Tukey HSD
par(las=2) # Orient labels on Y axis horizontally
par(mar=c(5,8,4,2)) #Change plot margins as needed to fit info Bot, L, T, R
plot(TukeyHSD(fit_av))
# Alternative plot from Kabacoff p. 221
library(multcomp)
par(mar=c(5,4,4,2)) #Change plot margins as needed to fit info
tuk <-  glht(fit_av, linfct=mcp(EPL_cat = "Tukey"))
plot(cld(tuk, level = .05), col="lightgrey")
par(opar) # return par values to defaults
