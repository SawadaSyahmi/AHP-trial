
library(ahpsurvey)
library(readxl)
library(magrittr)
library(dplyr)
data(city200)



ahpscale_lvl1 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level1"))
ahpscale_lvl2_1 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_Econ"))
ahpscale_lvl2_2 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_Infrastructure"))
ahpscale_lvl2_3 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_serviceStandard"))
ahpscale_lvl2_4 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_Technical"))
ahpscale_lvl3_1 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_Technical"))
ahpscale_lvl3_2 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_Technical"))
ahpscale_lvl3_3 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_Technical"))
ahpscale_lvl3_4 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_Technical"))
ahpscale_lvl3_5 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_Technical"))
ahpscale_lvl3_6 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_Technical"))
ahpscale_lvl3_7 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_Technical"))
ahpscale_lvl3_8 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_Technical"))
ahpscale_lvl3_9 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_Technical"))
ahpscale_lvl3_10 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_Technical"))
ahpscale_lvl3_11 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_Technical"))
ahpscale_lvl3_12 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_Technical"))

#################################################################################################################################################
#################################################################################################################################################

#########################            TESTING AREA      #####################################################################################


#Uncomment to use
#problem 1. Cannot work on less than 3 criteria


ahpscale_lvl2_1 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_Econ"))

MatrixChck01<-ahp.mat(ahpscale_lvl2_1, atts = c("Capex", "Opex"), negconvert = TRUE)
eigenmean_lvl2_1 <- ahp(df = ahpscale_lvl2_1,
                      atts = c("Capex", "Opex","empty"), ##test only ## need changes as R AHP lib need more than 3 or more criteria
                      negconvert = TRUE,
                      reciprocal = TRUE,
                      method = "eigen",
                      aggmethod = "arithmetic",
                      qt = 0.2,
                      censorcr = 0.37,
                      agg = TRUE)

head(eigenmean_lvl2_1$indpref)



#################################################################################################################################################
#################################################################################################################################################


#########################            Eigenvalue calculation AREA      ###########################################################################



eigenmean_lvl1 <- ahp(df = ahpscale_lvl1,
                      atts = c("Technical", "Economical", "Infrastructure", "Service Standard"),
                      negconvert = TRUE,
                      reciprocal = TRUE,
                      method = "eigen",
                      aggmethod = "arithmetic",
                      qt = 0.2,
                      censorcr = 0.37,
                      agg = TRUE)

head(eigenmean_lvl1$indpref)


eigenmean_lvl2_1 <- ahp(df = ahpscale_lvl2_1,
                        atts = c("Capex", "Opex"), ##test only ## need changes as R AHP lib need more than 3 or more criteria
                        negconvert = TRUE,
                        reciprocal = TRUE,
                        method = "eigen",
                        aggmethod = "arithmetic",
                        qt = 0.2,
                        censorcr = 0.37,
                        agg = TRUE)

head(eigenmean_lvl2_1$indpref)





MatrixChck<-ahp.mat(ahpscale_lvl2_2, atts = c("Existing Infrastructure", "Spectrum Availability"), negconvert = TRUE)
MatrixChck
eigenmean_lvl2_2 <- ahp(df = ahpscale_lvl2_2,
                       atts =  c("Existing Infrastructure", "Spectrum Availability"),
                       negconvert = TRUE,
                       reciprocal = TRUE,
                       method = "eigen",
                       aggmethod = "arithmetic",
                       qt = 0.2,
                       censorcr = 0.37,
                       agg = TRUE)

head(eigenmean_lvl2_2$indpref)

eigenmean_lvl2_3 <- ahp(df = ahpscale_lvl2_3,
                        atts =  c("Installation Complexity", "Maintenance ", "Security"),
                        negconvert = TRUE,
                        reciprocal = TRUE,
                        method = "eigen",
                        aggmethod = "arithmetic",
                        qt = 0.2,
                        censorcr = 0.37,
                        agg = TRUE)

head(eigenmean_lvl2_3$indpref)



eigenmean_lvl2_4 <- ahp(df = ahpscale_lvl2_4,
                        atts =  c("Frequency", "Scalibility", "DataRate", "Reliability"),
                        negconvert = TRUE,
                        reciprocal = TRUE,
                        method = "eigen",
                        aggmethod = "arithmetic",
                        qt = 0.2,
                        censorcr = 0.37,
                        agg = TRUE)

head(eigenmean_lvl2_4$indpref)

eigenmean_lvl3_1 <- ahp(df = ahpscale_lvl3_1,
                        atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
                        negconvert = TRUE,
                        reciprocal = TRUE,
                        method = "eigen",
                        aggmethod = "arithmetic",
                        qt = 0.2,
                        censorcr = 0.1,
                        agg = TRUE)

head(eigenmean_lvl3_1$indpref)


eigenmean_lvl3_2 <- ahp(df = ahpscale_lvl3_2,
                        atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
                        negconvert = TRUE,
                        reciprocal = TRUE,
                        method = "eigen",
                        aggmethod = "arithmetic",
                        qt = 0.2,
                        censorcr = 0.1,
                        agg = TRUE)

head(eigenmean_lvl3_2$indpref)

eigenmean_lvl3_3 <- ahp(df = ahpscale_lvl3_3,
                        atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
                        negconvert = TRUE,
                        reciprocal = TRUE,
                        method = "eigen",
                        aggmethod = "arithmetic",
                        qt = 0.2,
                        censorcr = 0.1,
                        agg = TRUE)

head(eigenmean_lvl3_3$indpref)

eigenmean_lvl3_4 <- ahp(df = ahpscale_lvl3_4,
                        atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
                        negconvert = TRUE,
                        reciprocal = TRUE,
                        method = "eigen",
                        aggmethod = "arithmetic",
                        qt = 0.2,
                        censorcr = 0.1,
                        agg = TRUE)

head(eigenmean_lvl3_4$indpref)

eigenmean_lvl3_5 <- ahp(df = ahpscale_lvl3_5,
                        atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
                        negconvert = TRUE,
                        reciprocal = TRUE,
                        method = "eigen",
                        aggmethod = "arithmetic",
                        qt = 0.2,
                        censorcr = 0.1,
                        agg = TRUE)

head(eigenmean_lvl3_5$indpref)

eigenmean_lvl3_6 <- ahp(df = ahpscale_lvl3_6,
                        atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
                        negconvert = TRUE,
                        reciprocal = TRUE,
                        method = "eigen",
                        aggmethod = "arithmetic",
                        qt = 0.2,
                        censorcr = 0.1,
                        agg = TRUE)

head(eigenmean_lvl3_6$indpref)

eigenmean_lvl3_7 <- ahp(df = ahpscale_lvl3_7,
                        atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
                        negconvert = TRUE,
                        reciprocal = TRUE,
                        method = "eigen",
                        aggmethod = "arithmetic",
                        qt = 0.2,
                        censorcr = 0.1,
                        agg = TRUE)

head(eigenmean_lvl3_7$indpref)

eigenmean_lvl3_8 <- ahp(df = ahpscale_lvl3_8,
                        atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
                        negconvert = TRUE,
                        reciprocal = TRUE,
                        method = "eigen",
                        aggmethod = "arithmetic",
                        qt = 0.2,
                        censorcr = 0.1,
                        agg = TRUE)

head(eigenmean_lvl3_8$indpref)

eigenmean_lvl3_9 <- ahp(df = ahpscale_lvl3_9,
                        atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
                        negconvert = TRUE,
                        reciprocal = TRUE,
                        method = "eigen",
                        aggmethod = "arithmetic",
                        qt = 0.2,
                        censorcr = 0.1,
                        agg = TRUE)

head(eigenmean_lvl3_9$indpref)

eigenmean_lvl3_10 <- ahp(df = ahpscale_lvl3_10,
                        atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
                        negconvert = TRUE,
                        reciprocal = TRUE,
                        method = "eigen",
                        aggmethod = "arithmetic",
                        qt = 0.2,
                        censorcr = 0.1,
                        agg = TRUE)

head(eigenmean_lvl3_10$indpref)

eigenmean_lvl3_11 <- ahp(df = ahpscale_lvl3_11,
                        atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
                        negconvert = TRUE,
                        reciprocal = TRUE,
                        method = "eigen",
                        aggmethod = "arithmetic",
                        qt = 0.2,
                        censorcr = 0.1,
                        agg = TRUE)

head(eigenmean_lvl3_11$indpref)

eigenmean_lvl3_12 <- ahp(df = ahpscale_lvl3_12,
                        atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
                        negconvert = TRUE,
                        reciprocal = TRUE,
                        method = "eigen",
                        aggmethod = "arithmetic",
                        qt = 0.2,
                        censorcr = 0.1,
                        agg = TRUE)

head(eigenmean_lvl3_12$indpref)












