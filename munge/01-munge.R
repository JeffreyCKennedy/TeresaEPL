# Teresa Winstone's MBS research into EPL Career Fit and Engagement
# Data manipulations
# Highlight and run command below to load project without munging.
# This is useful when debugging data manipulation code.
# rm(list = ls()); library(ProjectTemplate); load.project(list(munging=FALSE)) 

# Create the scale scores

keys.list_5 <- list(ID=c("ID"),
                    MOT_P = c("MOT1_PAI", "MOT4_PCL", "MOT7_PSN", "MOT10_PSN", 
                              "MOT16_PCL", "MOT19_PAI", "MOT22_PSN", "MOT27_PCL",
                              "MOT_PNEW"),
                    MOT_L = c("MOT2_LSN", "MOT5_LAI", "MOT8_LNCL", "MOT11_LNCL", 
                              "MOT14_LSN", "MOT17_LNCL", "MOT20_LAI", "MOT23_LAI", 
                              "MOT26_LSN"),
                    MOT_E = c("MOT6_ESN", "MOT9_ECL", "MOT12_ECL", "MOT15_EAI", 
                              "MOT18_ESN", "MOT24_EAI", "MOT28_ESN", "MOT29_ECL", 
                              "MOT30_EAI"),
                    MOT_PAI = c("MOT1_PAI", "MOT19_PAI", "MOT_PNEW"),
                    MOT_PSN = c("MOT7_PSN", "MOT10_PSN",  "MOT22_PSN"),
                    MOT_PCL = c("MOT4_PCL", "MOT16_PCL", "MOT27_PCL"),
                    MOT_LAI = c("MOT5_LAI", "MOT20_LAI", "MOT23_LAI"),
                    MOT_LSN = c("MOT2_LSN", "MOT14_LSN", "MOT26_LSN"),
                    MOT_LNCL = c("MOT8_LNCL", "MOT11_LNCL", "MOT17_LNCL"),
                    MOT_ESN = c("MOT6_ESN", "MOT18_ESN", "MOT28_ESN"),
                    MOT_ECL = c("MOT9_ECL", "MOT12_ECL", "MOT29_ECL"),
                    MOT_EAI = c("MOT15_EAI", "MOT24_EAI", "MOT30_EAI"),
                    FIT_EPL = c( "FIT1_E", "FIT2_E", "FIT3_E", "FIT4_P", 
                                 "FIT5_P", "FIT6_P", "FIT7_L", "FIT8_L", "FIT9_L"),
                    FIT_E = c("FIT1_E", "FIT2_E", "FIT3_E"),
                    FIT_P = c("FIT4_P", "FIT5_P", "FIT6_P"),
                    FIT_L = c("FIT7_L", "FIT8_L", "FIT9_L"),
                    PEFIT = c("PEFIT1_VC", "PEFIT2_VC", "PEFIT3_VC", "PEFIT4_NSPJ",
                              "PEFIT5_NSPJ", "PEFIT6_NSPJ", "PEFIT7_DAPJ", 
                              "PEFIT8_DAPJ", "PEFIT9_DAPJ"),
                    PEFIT_VC = c("PEFIT1_VC", "PEFIT2_VC", "PEFIT3_VC"),
                    PEFIT_NSPJ = c("PEFIT4_NSPJ", "PEFIT5_NSPJ", "PEFIT6_NSPJ"),
                    PEFIT_DAPJ = c("PEFIT7_DAPJ", "PEFIT8_DAPJ", "PEFIT9_DAPJ"),
                    WE = c("WE1_PHY", "WE2_PHY", "WE3_PHY", "WE4_PHY", "WE5_PHY",
                           "WE6_PHY", "WE7_EMO", "WE8_EMO", "WE9_EMO", "WE10_EMO",
                           "WE11_EMO", "WE12_EMO", "WE13_COG", "WE14_COG",
                           "WE15_COG", "WE16_COG", "WE17_COG", "WE18_COG"),
                    WE_PHY = c("WE1_PHY", "WE2_PHY", "WE3_PHY", "WE4_PHY", 
                               "WE5_PHY", "WE6_PHY"),
                    WE_EMO = c("WE7_EMO", "WE8_EMO", "WE9_EMO", "WE10_EMO", 
                               "WE11_EMO", "WE12_EMO"),
                    WE_COG = c("WE13_COG", "WE14_COG", "WE15_COG", "WE16_COG", 
                               "WE17_COG", "WE18_COG"),
                    TO = c("TI_1", "TI_2", "TI_3", "TI_4")
                    )

itemsused_5 <- c("ID", "MOT1_PAI", "MOT4_PCL", "MOT7_PSN", "MOT10_PSN",
                 "MOT16_PCL", "MOT19_PAI", "MOT22_PSN", "MOT27_PCL", "MOT_PNEW",
                 "MOT2_LSN", "MOT5_LAI", "MOT8_LNCL", "MOT11_LNCL",
                 "MOT14_LSN", "MOT17_LNCL", "MOT20_LAI", "MOT23_LAI", "MOT26_LSN",
                 "MOT6_ESN", "MOT9_ECL", "MOT12_ECL", "MOT15_EAI",
                 "MOT18_ESN", "MOT24_EAI", "MOT28_ESN", "MOT29_ECL", "MOT30_EAI",
                 "FIT1_E", "FIT2_E", "FIT3_E", "FIT4_P", 
                 "FIT5_P", "FIT6_P", "FIT7_L", "FIT8_L", "FIT9_L",
                 "PEFIT1_VC", "PEFIT2_VC", "PEFIT3_VC", "PEFIT4_NSPJ",
                 "PEFIT5_NSPJ", "PEFIT6_NSPJ", "PEFIT7_DAPJ", 
                 "PEFIT8_DAPJ", "PEFIT9_DAPJ", "WE1_PHY", "WE2_PHY", "WE3_PHY", 
                 "WE4_PHY", "WE5_PHY", "WE6_PHY", "WE7_EMO", "WE8_EMO", "WE9_EMO",
                 "WE10_EMO", "WE11_EMO", "WE12_EMO", "WE13_COG", "WE14_COG",
                 "WE15_COG", "WE16_COG", "WE17_COG", "WE18_COG",
                 "TI_1", "TI_2", "TI_3", "TI_4")

subset_CarPref_5 <- CarPref %>% dplyr::select(one_of(itemsused_5))
scores_5 <- scoreItems(keys.list_5, subset_CarPref_5, impute="none", min=1, max=5, digits = 2)
# add the new scale scores to CarPref using dplyr::left_join, matching by ID.
CarPref <- dplyr::left_join(CarPref, as.data.frame(scores_5$scores), by = "ID")

keys.list_7 <- list(ID=c("ID"), 
                    JS = c("JS_1", "JS_2", "JS_3"),
                    LS = c("LS_1", "LS_2", "LS_3", "LS_4", "LS_5"))

itemsused_7 <- c("ID", "JS_1", "JS_2", "JS_3", "LS_1", "LS_2", "LS_3", "LS_4", "LS_5")

subset_CarPref_7 <- CarPref %>% dplyr::select(one_of(itemsused_7))
scores_7 <- scoreItems(keys.list_7, subset_CarPref_7, impute="none", min=1, max=7, digits = 2)
# add the new scale scores to responses using dplyr::left_join, matching by ID.
CarPref <- dplyr::left_join(CarPref, as.data.frame(scores_7$scores), by = "ID")




# Still to do -------------------------------------------------------------
# Get values of LS_2 which are missing from this dataset. Can sort SPSS file
# according to Qstnr_number then copy this number across to the ResponseID 
# column. Could then use ResponseID to match up cases? Or there's probably 
# a join function in dplyr that will do it.