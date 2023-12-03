#10.31.23
#Starting Individual Project


head(Brown_adipose_2022)
#install.packages("openxlsx")
library(openxlsx)
?read.xlsx
Brown_adipose_2022 = read.xlsx("data/Brown adipose 2022.xlsx",rowNames = F) #DS: keep the row names
Brown_adipose_2022[1:3,]
Brown_adipose_2022
#install.packages("ggplot2")
library(ggplot2)
####visualize PCA results
#install.packages("ggpubr")
#install.packages("ggthemes")
library(ggpubr)
library(ggthemes)
#install.packages("gmodels")
library(gmodels)
library(export)
library(tidyverse)
#Calculate the Principal Components (PC) using fast.prcomp included in gmodels.
pca.info <- fast.prcomp(Brown_adipose_2022)
#check PCA results.
head(pca.info)
summary(pca.info) #You can see the specific explained variance for each PC.
head(pca.info$rotation) #regression coefficients
head(pca.info$sdev) #square root of eigenvalue
head(pca.info$x) #sample score
pca.data <- data.frame(sample = rownames(pca.info$rotation),Type=c(rep("4_IBA",8),rep("5_MAR",8),rep("2_OCT",9), rep("1_SEP",9),rep("3_TOR",14)), pca.info$rotation)
#according to the name you generate when read your excel
m<-colnames(Brown_adipose_2022)[1:48]
ggscatter(pca.data,x="PC1", y="PC2", color="Type",
          size=1, ellipse.border.remove=TRUE,
          label=c(m), repel=TRUE, main="PCA plot"+theme_base())
#next objectives is too: customize plot and create heatmmap
#install.packages("ggplot2")
library(ggplot2)
# Sample data
#3data_matrix <- matrix(data = c((Brown_adipose_2022)[1:48]), nrow = 5, byrow = TRUE)
#rownames(data_matrix) <- c(rep("4_IBA",8),rep("5_MAR",8),rep("2_OCT",9), rep("1_SEP",9),rep("3_TOR",14))
#colnames(data_matrix) <- Brown_adipose_2022[1:48]

# Create a data frame from the matrix
#data_df <- as.data.frame(data_matrix)

# Load the scales package for better control of color scales
#install.packages("scales")
#library(scales)
#install.packages("reshape2")
library(reshape2)
# Create the heatmap using ggplot2
#ggplot(data = data_df, aes(x = , y = , fill = value)) +
  #geom_tile() +
  #scale_fill_gradient(low = "white", high = "red") +
  #theme_minimal() +
  #labs(title = "Heatmap") +
  #theme(axis.text.x = element_text(angle = 48, hjust = 1))

####

a=Brown_adipose_2022 %>%
  mutate(feature=seq(1:nrow(Brown_adipose_2022))) %>%
  select(-Feature) %>%
  pivot_longer(-feature)

#whole data heatmap plot
dat=Brown_adipose_2022[,2:49]
dat1=apply(dat,1,function(x) scale(x))
t(dat1)
dat2=data.frame(Feature=Brown_adipose_2022[,1])
dat3=cbind(dat2, t(dat1))

library(ggplot2)
ggplot(data = pivot_longer(dat3, -Feature), aes(x = name, y = Feature, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Heatmap") +
  theme(axis.text.x = element_text(angle = 48, hjust = 1)) +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank())

#variable data heatmap
#The data should be seperate according to the variable
Brown_adipose_2022[1,] #check the variable id and get the column of order of each variable
#According to the results shown in the console, the variables are in the order:
  #1 columns 2-9 are IBA.
  #2 columns 10-17 are MAR.
  #3 columns 18-26 are OCT.
  #4 columns 27-35 are SEPT.
  #5 columns 36-49 are TOR.
#IBA data heatmap
dat=Brown_adipose_2022[,2:9]
dat1=apply(dat,1,function(x) scale(x))
t(dat1)
dat2=data.frame(Feature=Brown_adipose_2022[,1])
dat3=cbind(dat2, t(dat1))

#library(ggplot2)
ggplot(data = pivot_longer(dat3, -Feature), aes(x = name, y = Feature, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "white") +
  theme_minimal() +
  labs(title = "Heatmap") +
  theme(axis.text.x = element_text(angle = 48, hjust = 1)) +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank())
#MAR data heatmap
dat=Brown_adipose_2022[,10:17]
dat1=apply(dat,1,function(x) scale(x))
t(dat1)
dat2=data.frame(Feature=Brown_adipose_2022[,1])
dat3=cbind(dat2, t(dat1))
#library(ggplot2)
ggplot(data = pivot_longer(dat3, -Feature), aes(x = name, y = Feature, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "white") +
  theme_minimal() +
  labs(title = "Heatmap") +
  theme(axis.text.x = element_text(angle = 48, hjust = 1)) +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank())
#OCT DATA HEATMAP
dat=Brown_adipose_2022[,18:26]
dat1=apply(dat,1,function(x) scale(x))
t(dat1)
dat2=data.frame(Feature=Brown_adipose_2022[,1])
dat3=cbind(dat2, t(dat1))
#library(ggplot2)
ggplot(data = pivot_longer(dat3, -Feature), aes(x = name, y = Feature, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "white") +
  theme_minimal() +
  labs(title = "Heatmap") +
  theme(axis.text.x = element_text(angle = 48, hjust = 1)) +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank())
# SEP DATA HEATMAP
dat=Brown_adipose_2022[,27:35]
dat1=apply(dat,1,function(x) scale(x))
t(dat1)
dat2=data.frame(Feature=Brown_adipose_2022[,1])
dat3=cbind(dat2, t(dat1))
#library(ggplot2)
ggplot(data = pivot_longer(dat3, -Feature), aes(x = name, y = Feature, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "white") +
  theme_minimal() +
  labs(title = "Heatmap") +
  theme(axis.text.x = element_text(angle = 48, hjust = 1)) +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank())
# TOR DATA HEATMAP
dat=Brown_adipose_2022[,36:49]
dat1=apply(dat,1,function(x) scale(x))
t(dat1)
dat2=data.frame(Feature=Brown_adipose_2022[,1])
dat3=cbind(dat2, t(dat1))
#library(ggplot2)
ggplot(data = pivot_longer(dat3, -Feature), aes(x = name, y = Feature, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "white") +
  theme_minimal() +
  labs(title = "Heatmap") +
  theme(axis.text.x = element_text(angle = 48, hjust = 1)) +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank())


