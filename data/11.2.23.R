#10.31.23
#Starting Indivudial Project
install.packages("xcmsOnline")
library(xcmsOnline)
install.packages("MALDIquant")
library(MALDIquant)
install.packages("pRoloc")
library(pRoloc)
install.packages("MassSpecWavelet")
library(MassSpecWavelet)
install.packages("MetaboAnalystR")
library(MetaboAnalystR)
install.packages("MSnbase")
library(MSnbase)
install.packages("CAMERA")
library(CAMERA)
install.packages("xcms")
library(xcms)
install.packages("ggplot2")
library(ggplot2)
install.packages("pheatmap")
library(pheatmap)
install.packages("VennDiagram")
library(VennDiagram)
install.packages("circlize")
library(circlize)
install.packages("ComplexHeatmap")
library(ComplexHeatmap)
install.packages("volcano3D")
library(volcano3D)


head(Brown_adipose_2022)
#install.packages("openxlsx")
library(openxlsx)
?read.xlsx
Brown_adipose_2022 <- read.xlsx("data/Brown adipose 2022.xlsx",rowNames = T)
Brown_adipose_2022[1:3,]
Brown_adipose_2022
