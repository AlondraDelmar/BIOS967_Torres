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
#install.packages("ggplot2")
library(openxlsx)
library(ggplot2)
####visualize PCA results
#install.packages("ggpubr")
#install.packages("ggthemes")
library(ggpubr)
library(ggthemes)
#install.packages("gmodels")
library(gmodels)
library(export)
#Calculate the Principal Components (PC) using fast.prcomp included in gmodels.
pca.info <- fast.prcomp(Brown_adipose_2022)
#check PCA results.
head(pca.info)
summary(pca.info) #You can see the specific explained variance for each PC.
head(pca.info$rotation) #regression coefficients
head(pca.info$sdev) #square root of eigenvalue
head(pca.info$x) #sample score
pca.data <- data.frame(sample = rownames(pca.info$rotation),Type=c(rep("4_IBA",8),rep("5_MAR",8),rep("2_OCT",9), rep("1_SEP",9),rep("3_TOR",14)), pca.info$rotation)
#according to the name you geenrate when read your excel
m<-colnames(Brown_adipose_2022)[1:48]
ggscatter(pca.data,x="PC1", y="PC2", color="Type",
          size=1, ellipse.border.remove=TRUE,
          label=c(m), repel=TRUE, main="PCA plot"+theme_base())
#next objectives is too: customize plot and create heatmmap
