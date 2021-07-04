
library(ahpsurvey)
library(readxl)
library(magrittr)
library(dplyr)
data(city200)
#options(error=recover)


ahpscale_lvl1 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level1"))
ahpscale_lvl2_1 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_Econ"))
ahpscale_lvl2_2 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_Infrastructure"))
ahpscale_lvl2_3 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_serviceStandard"))
ahpscale_lvl2_4 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_Technical"))
ahpscale_lvl3_1 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level3_Freq"))
ahpscale_lvl3_2 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level3_Distance"))
ahpscale_lvl3_3 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level3_Scalability"))
ahpscale_lvl3_4 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level3_DataRate"))
ahpscale_lvl3_5 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level3_Reliability"))
ahpscale_lvl3_6 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level3_Capex"))
ahpscale_lvl3_7 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level3_Opex"))
ahpscale_lvl3_8 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level3_ExtInfra"))
ahpscale_lvl3_9 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level3_Spectrum"))
ahpscale_lvl3_10 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level3_Security"))
ahpscale_lvl3_11 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level3_Install"))
ahpscale_lvl3_12 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level3_Maintain"))


#################################################################################################################################################
#################################################################################################################################################

#########################            TESTING AREA      #####################################################################################


#Uncomment to use
#problem 1. Cannot work on less than 3 criteria

#
# ahpscale_lvl2_1Test <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_EconTest"))
#
# MatrixChck01<-ahp.mat(ahpscale_lvl2_1Test, atts = c("Capex", "Opex","empty"), negconvert = TRUE)
# MatrixChck01
# eigenmean_lvl2_1 <- ahp(df = ahpscale_lvl2_1Test,
#                       atts = c("Capex", "Opex","empty"), ##test only ## need changes as R AHP lib need more than 3 or more criteria
#                       negconvert = TRUE,
#                       reciprocal = TRUE,
#                       method = "eigen",
#                       aggmethod = "arithmetic",
#                       qt = 0.2,
#                       censorcr = 0.37,
#                       agg = TRUE)
#
# head(eigenmean_lvl2_1$indpref)




# ahpscale_lvl2_1 <- data.frame(read_excel("SummaryAhpScale.xlsx", sheet = "Level2_EconTest"))
# MatrixChck01<-ahp.mat(ahpscale_lvl2_1, atts = c("Capex", "Opex"), negconvert = TRUE)
# B=(matrix(unlist(MatrixChck01), nrow=2, ncol=2,  byrow=TRUE))
# A= eigen(B)
# eB = matrix((A$vectors[,1]),nrow=2, ncol=1, byrow= TRUE)
# eigenmean_lvl2_1= eB/sum(eB)


# eigenmean_lvl2_1 <- ahp(df = ahpscale_lvl2_1Test,
#                         atts = c("Capex", "Opex",), ##test only ## need changes as R AHP lib need more than 3 or more criteria
#                         negconvert = TRUE,
#                         reciprocal = TRUE,
#                         method = "eigen",
#                         aggmethod = "arithmetic",
#                         qt = 0.2,
#                         censorcr = 0.37,
#                         agg = TRUE)
#
# head(eigenmean_lvl2_1$indpref)



#################################################################################################################################################
#################################################################################################################################################

#########################            Eigenvalue calculation AREA      ###########################################################################

#################################################################################################################################################
#################################################################################################################################################




MatrixChck01<-ahp.mat(ahpscale_lvl1, atts = c("Technical", "Economical", "Infrastructure", "Service Standard"), negconvert = TRUE)
B=(matrix(unlist(MatrixChck01), nrow=4, ncol=4,  byrow=TRUE))
A= eigen(B)
eB = matrix((A$vectors[,1]),nrow=4, ncol=1, byrow= TRUE)
eigenmean_lvl1= Re(eB/sum(eB))
eigenmean_lvl1

MatrixChck01<-ahp.mat(ahpscale_lvl2_1, atts = c("Capex", "Opex"), negconvert = TRUE)
B=(matrix(unlist(MatrixChck01), nrow=2, ncol=2,  byrow=TRUE))
A= eigen(B)
eB = matrix((A$vectors[,1]),nrow=2, ncol=1, byrow= TRUE)
eigenmean_lvl2_1= Re(eB/sum(eB))
eigenmean_lvl2_1

MatrixChck02<-ahp.mat(ahpscale_lvl2_2, atts = c("Existing Infrastructure", "Spectrum Availability"), negconvert = TRUE)
B=(matrix(unlist(MatrixChck02), nrow=2, ncol=2,  byrow=TRUE))
A= eigen(B)
eB = matrix((A$vectors[,1]),nrow=2, ncol=1, byrow= TRUE)
eigenmean_lvl2_2= Re(eB/sum(eB))
eigenmean_lvl2_2

MatrixChck01<-ahp.mat(ahpscale_lvl2_3, atts = c("Installation Complexity", "Maintenance ", "Security"), negconvert = TRUE)
B=(matrix(unlist(MatrixChck01), nrow=3, ncol=3,  byrow=TRUE))
A= eigen(B)
eB = matrix((A$vectors[,1]),nrow=3, ncol=1, byrow= TRUE)
eigenmean_lvl2_3= Re(eB/sum(eB))
eigenmean_lvl2_3

MatrixChck01<-ahp.mat(ahpscale_lvl2_4, atts = c("Frequency","Distance", "Scalibility", "DataRate", "Reliability"), negconvert = TRUE)
B=(matrix(unlist(MatrixChck01), nrow=5, ncol=5,  byrow=TRUE))
A= eigen(B)
eB = matrix((A$vectors[,1]),nrow=5, ncol=1, byrow= TRUE)
eigenmean_lvl2_4= Re(eB/sum(eB))
eigenmean_lvl2_4

MatrixChck01<-ahp.mat(ahpscale_lvl3_1, atts = c("NBPLC","Fiber", "Zigbee", "Wifi", "4G", "5G", "PrivateLte", "Lora"), negconvert = TRUE)
B=(matrix(unlist(MatrixChck01), nrow=8, ncol=8,  byrow=TRUE))
A= eigen(B)
eB = matrix((A$vectors[,1]),nrow=8, ncol=1, byrow= TRUE)
eigenmean_lvl3_1= Re(eB/sum(eB))
eigenmean_lvl3_1


MatrixChck01<-ahp.mat(ahpscale_lvl3_2, atts = c("NBPLC","Fiber", "Zigbee", "Wifi", "4G", "5G", "PrivateLte", "Lora"), negconvert = TRUE)
B=(matrix(unlist(MatrixChck01), nrow=8, ncol=8,  byrow=TRUE))
A= eigen(B)
eB = matrix((A$vectors[,1]),nrow=8, ncol=1, byrow= TRUE)
eigenmean_lvl3_2= Re(eB/sum(eB))
eigenmean_lvl3_2

MatrixChck01<-ahp.mat(ahpscale_lvl3_3, atts = c("NBPLC","Fiber", "Zigbee", "Wifi", "4G", "5G", "PrivateLte", "Lora"), negconvert = TRUE)
B=(matrix(unlist(MatrixChck01), nrow=8, ncol=8,  byrow=TRUE))
A= eigen(B)
eB = matrix((A$vectors[,1]),nrow=8, ncol=1, byrow= TRUE)
eigenmean_lvl3_3= Re(eB/sum(eB))
eigenmean_lvl3_3

MatrixChck01<-ahp.mat(ahpscale_lvl3_4, atts = c("NBPLC","Fiber", "Zigbee", "Wifi", "4G", "5G", "PrivateLte", "Lora"), negconvert = TRUE)
B=(matrix(unlist(MatrixChck01), nrow=8, ncol=8,  byrow=TRUE))
A= eigen(B)
eB = matrix((A$vectors[,1]),nrow=8, ncol=1, byrow= TRUE)
eigenmean_lvl3_4= Re(eB/sum(eB))
eigenmean_lvl3_4

MatrixChck01<-ahp.mat(ahpscale_lvl3_5, atts = c("NBPLC","Fiber", "Zigbee", "Wifi", "4G", "5G", "PrivateLte", "Lora"), negconvert = TRUE)
B=(matrix(unlist(MatrixChck01), nrow=8, ncol=8,  byrow=TRUE))
A= eigen(B)
eB = matrix((A$vectors[,1]),nrow=8, ncol=1, byrow= TRUE)
eigenmean_lvl3_5= Re(eB/sum(eB))
eigenmean_lvl3_5

MatrixChck01<-ahp.mat(ahpscale_lvl3_6, atts = c("NBPLC","Fiber", "Zigbee", "Wifi", "4G", "5G", "PrivateLte", "Lora"), negconvert = TRUE)
B=(matrix(unlist(MatrixChck01), nrow=8, ncol=8,  byrow=TRUE))
A= eigen(B)
eB = matrix((A$vectors[,1]),nrow=8, ncol=1, byrow= TRUE)
eigenmean_lvl3_6= Re(eB/sum(eB))
eigenmean_lvl3_6

MatrixChck01<-ahp.mat(ahpscale_lvl3_7, atts = c("NBPLC","Fiber", "Zigbee", "Wifi", "4G", "5G", "PrivateLte", "Lora"), negconvert = TRUE)
B=(matrix(unlist(MatrixChck01), nrow=8, ncol=8,  byrow=TRUE))
A= eigen(B)
eB = matrix((A$vectors[,1]),nrow=8, ncol=1, byrow= TRUE)
eigenmean_lvl3_7= Re(eB/sum(eB))
eigenmean_lvl3_7

MatrixChck01<-ahp.mat(ahpscale_lvl3_8, atts = c("NBPLC","Fiber", "Zigbee", "Wifi", "4G", "5G", "PrivateLte", "Lora"), negconvert = TRUE)
B=(matrix(unlist(MatrixChck01), nrow=8, ncol=8,  byrow=TRUE))
A= eigen(B)
eB = matrix((A$vectors[,1]),nrow=8, ncol=1, byrow= TRUE)
eigenmean_lvl3_8= Re(eB/sum(eB))
eigenmean_lvl3_8

MatrixChck01<-ahp.mat(ahpscale_lvl3_9, atts = c("NBPLC","Fiber", "Zigbee", "Wifi", "4G", "5G", "PrivateLte", "Lora"), negconvert = TRUE)
B=(matrix(unlist(MatrixChck01), nrow=8, ncol=8,  byrow=TRUE))
A= eigen(B)
eB = matrix((A$vectors[,1]),nrow=8, ncol=1, byrow= TRUE)
eigenmean_lvl3_9= Re(eB/sum(eB))
eigenmean_lvl3_9

MatrixChck01<-ahp.mat(ahpscale_lvl3_10, atts = c("NBPLC","Fiber", "Zigbee", "Wifi", "4G", "5G", "PrivateLte", "Lora"), negconvert = TRUE)
B=(matrix(unlist(MatrixChck01), nrow=8, ncol=8,  byrow=TRUE))
A= eigen(B)
eB = matrix((A$vectors[,1]),nrow=8, ncol=1, byrow= TRUE)
eigenmean_lvl3_10= Re(eB/sum(eB))
eigenmean_lvl3_10

MatrixChck01<-ahp.mat(ahpscale_lvl3_11, atts = c("NBPLC","Fiber", "Zigbee", "Wifi", "4G", "5G", "PrivateLte", "Lora"), negconvert = TRUE)
B=(matrix(unlist(MatrixChck01), nrow=8, ncol=8,  byrow=TRUE))
A= eigen(B)
eB = matrix((A$vectors[,1]),nrow=8, ncol=1, byrow= TRUE)
eigenmean_lvl3_11= Re(eB/sum(eB))
eigenmean_lvl3_11

MatrixChck01<-ahp.mat(ahpscale_lvl3_12, atts = c("NBPLC","Fiber", "Zigbee", "Wifi", "4G", "5G", "PrivateLte", "Lora"), negconvert = TRUE)
B=(matrix(unlist(MatrixChck01), nrow=8, ncol=8,  byrow=TRUE))
A= eigen(B)
eB = matrix((A$vectors[,1]),nrow=8, ncol=1, byrow= TRUE)
eigenmean_lvl3_12= Re(eB/sum(eB))
eigenmean_lvl3_12


#################################################################################################################################################
#################################################################################################################################################

#########################            Global Priorities calculation AREA      ####################################################################

#################################################################################################################################################
#################################################################################################################################################
globalPriorTechnical=c()

for (n in 1:length(eigenmean_lvl2_4)) {
  globalPriorTechnical[n] <- eigenmean_lvl1[1,]*eigenmean_lvl2_4[n,]

}

globalPriorEcon=c()

for (n in 1:length(eigenmean_lvl2_1)) {
  globalPriorEcon[n] <- eigenmean_lvl1[2,]*eigenmean_lvl2_1[n,]

}

globalPriorInfra=c()

for (n in 1:length(eigenmean_lvl2_2)) {
  globalPriorInfra[n] <- eigenmean_lvl1[3,]*eigenmean_lvl2_2[n,]
}

globalPriorServiceStandard=c()

for (n in 1:length(eigenmean_lvl2_3)) {
  globalPriorServiceStandard[n] <- eigenmean_lvl1[4,]*eigenmean_lvl2_3[n,]

}

TotalGlobalPrio<- sum(sum(globalPriorEcon),sum(globalPriorInfra),sum(globalPriorTechnical),sum(globalPriorServiceStandard))


#################################################################################################################################################
#################################################################################################################################################

#########################               Ranking calculation AREA             ####################################################################

#################################################################################################################################################
#################################################################################################################################################

# for NBPLC

listlevel3_Technical<-c("eigenmean_lvl3_1","eigenmean_lvl3_2","eigenmean_lvl3_3","eigenmean_lvl3_4","eigenmean_lvl3_5")
listlevel3_Econ<-c("eigenmean_lvl3_6","eigenmean_lvl3_7")
listlevel3_Infra<-c("eigenmean_lvl3_8","eigenmean_lvl3_9")
listlevel3_Service<-c("eigenmean_lvl3_10", "eigenmean_lvl3_11","eigenmean_lvl3_12")
Tech<-c("NBPLC","Fiber", "Zigbee", "Wifi", "4G", "5G", "PrivateLte", "Lora")

SumEVTotal=c()

for(n in 1:8){
    EV=0
    SumEV=0
    m=0
    for(i in listlevel3_Technical){
      m=m+1
      EV = globalPriorTechnical[m]*get(i)[n] #loop the number [1] and loop the sumEV
      SumEV = SumEV +  EV
    }


    m=0
    for(i in listlevel3_Econ){
      m=m+1
      EV = globalPriorEcon[m]*get(i)[n]
      SumEV = SumEV + EV
    }

    m=0
    for(i in listlevel3_Infra){
      m=m+1
      EV = globalPriorInfra[m]*get(i)[n]
      SumEV = SumEV + EV
    }

    m=0
    for(i in listlevel3_Service){
      m=m+1
      EV = globalPriorServiceStandard[m]*get(i)[n]
      SumEV = SumEV + EV
    }

SumEVTotal[n]<- SumEV
}

EVscore<- data.frame(Tech, SumEVTotal)
SortedEvscore <- arrange(EVscore,-SumEVTotal)
SortedEvscore



#################################################################################################################################################
#################################################################################################################################################

#########################                             Trash AREA      ###########################################################################

#################################################################################################################################################
#################################################################################################################################################

# eigenmean_lvl1 <- ahp(df = ahpscale_lvl1,
#                       atts = c("Technical", "Economical", "Infrastructure", "Service Standard"),
#                       negconvert = TRUE,
#                       reciprocal = TRUE,
#                       method = "eigen",
#                       aggmethod = "arithmetic",
#                       qt = 0.1,
#                       censorcr = 0.37,
#                       agg = TRUE)
#
# head(eigenmean_lvl1$indpref)
#
# eigenmean_lvl2_3 <- ahp(df = ahpscale_lvl2_3,
#                         atts =  c("Installation Complexity", "Maintenance ", "Security"),
#                         negconvert = TRUE,
#                         reciprocal = TRUE,
#                         method = "eigen",
#                         aggmethod = "arithmetic",
#                         qt = 0.2,
#                         censorcr = 0.37,
#                         agg = TRUE)
#
# head(eigenmean_lvl2_3$indpref)
#
#
#
# eigenmean_lvl2_4 <- ahp(df = ahpscale_lvl2_4,
#                         atts =  c("Frequency", "Scalibility", "DataRate", "Reliability"),
#                         negconvert = TRUE,
#                         reciprocal = TRUE,
#                         method = "eigen",
#                         aggmethod = "arithmetic",
#                         qt = 0.2,
#                         censorcr = 0.37,
#                         agg = TRUE)
#
# head(eigenmean_lvl2_4$indpref)
#
# eigenmean_lvl3_1 <- ahp(df = ahpscale_lvl3_1,
#                         atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
#                         negconvert = TRUE,
#                         reciprocal = TRUE,
#                         method = "eigen",
#                         aggmethod = "arithmetic",
#                         qt = 0.2,
#                         censorcr = 0.1,
#                         agg = TRUE)
#
# head(eigenmean_lvl3_1$indpref)
#
#
# eigenmean_lvl3_2 <- ahp(df = ahpscale_lvl3_2,
#                         atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
#                         negconvert = TRUE,
#                         reciprocal = TRUE,
#                         method = "eigen",
#                         aggmethod = "arithmetic",
#                         qt = 0.2,
#                         censorcr = 0.1,
#                         agg = TRUE)
#
# head(eigenmean_lvl3_2$indpref)
#
# eigenmean_lvl3_3 <- ahp(df = ahpscale_lvl3_3,
#                         atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
#                         negconvert = TRUE,
#                         reciprocal = TRUE,
#                         method = "eigen",
#                         aggmethod = "arithmetic",
#                         qt = 0.2,
#                         censorcr = 0.1,
#                         agg = TRUE)
#
# head(eigenmean_lvl3_3$indpref)
#
# eigenmean_lvl3_4 <- ahp(df = ahpscale_lvl3_4,
#                         atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
#                         negconvert = TRUE,
#                         reciprocal = TRUE,
#                         method = "eigen",
#                         aggmethod = "arithmetic",
#                         qt = 0.2,
#                         censorcr = 0.1,
#                         agg = TRUE)
#
# head(eigenmean_lvl3_4$indpref)
#
# eigenmean_lvl3_5 <- ahp(df = ahpscale_lvl3_5,
#                         atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
#                         negconvert = TRUE,
#                         reciprocal = TRUE,
#                         method = "eigen",
#                         aggmethod = "arithmetic",
#                         qt = 0.2,
#                         censorcr = 0.1,
#                         agg = TRUE)
#
# head(eigenmean_lvl3_5$indpref)
#
# eigenmean_lvl3_6 <- ahp(df = ahpscale_lvl3_6,
#                         atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
#                         negconvert = TRUE,
#                         reciprocal = TRUE,
#                         method = "eigen",
#                         aggmethod = "arithmetic",
#                         qt = 0.2,
#                         censorcr = 0.1,
#                         agg = TRUE)
#
# head(eigenmean_lvl3_6$indpref)
#
# eigenmean_lvl3_7 <- ahp(df = ahpscale_lvl3_7,
#                         atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
#                         negconvert = TRUE,
#                         reciprocal = TRUE,
#                         method = "eigen",
#                         aggmethod = "arithmetic",
#                         qt = 0.2,
#                         censorcr = 0.1,
#                         agg = TRUE)
#
# head(eigenmean_lvl3_7$indpref)
#
# eigenmean_lvl3_8 <- ahp(df = ahpscale_lvl3_8,
#                         atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
#                         negconvert = TRUE,
#                         reciprocal = TRUE,
#                         method = "eigen",
#                         aggmethod = "arithmetic",
#                         qt = 0.2,
#                         censorcr = 0.1,
#                         agg = TRUE)
#
# head(eigenmean_lvl3_8$indpref)
#
# eigenmean_lvl3_9 <- ahp(df = ahpscale_lvl3_9,
#                         atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
#                         negconvert = TRUE,
#                         reciprocal = TRUE,
#                         method = "eigen",
#                         aggmethod = "arithmetic",
#                         qt = 0.2,
#                         censorcr = 0.1,
#                         agg = TRUE)
#
# head(eigenmean_lvl3_9$indpref)
#
# eigenmean_lvl3_10 <- ahp(df = ahpscale_lvl3_10,
#                          atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
#                          negconvert = TRUE,
#                          reciprocal = TRUE,
#                          method = "eigen",
#                          aggmethod = "arithmetic",
#                          qt = 0.2,
#                          censorcr = 0.1,
#                          agg = TRUE)
#
# head(eigenmean_lvl3_10$indpref)
#
# eigenmean_lvl3_11 <- ahp(df = ahpscale_lvl3_11,
#                          atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
#                          negconvert = TRUE,
#                          reciprocal = TRUE,
#                          method = "eigen",
#                          aggmethod = "arithmetic",
#                          qt = 0.2,
#                          censorcr = 0.1,
#                          agg = TRUE)
#
# head(eigenmean_lvl3_11$indpref)
#
# eigenmean_lvl3_12 <- ahp(df = ahpscale_lvl3_12,
#                          atts =  c("NBPLC", "UHF", "Fiber Optics", "Zigbee",  "WIFI/RF Mesh", "4G", "	Private LTE / NB- IoT",  "LoRA", "SATCOM"),
#                          negconvert = TRUE,
#                          reciprocal = TRUE,
#                          method = "eigen",
#                          aggmethod = "arithmetic",
#                          qt = 0.2,
#                          censorcr = 0.1,
#                          agg = TRUE)
#
# head(eigenmean_lvl3_12$indpref)





