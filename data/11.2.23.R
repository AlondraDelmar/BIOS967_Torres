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
dat_pca = Brown_adipose_2022[,2:49]
pca.info = fast.prcomp(dat_pca)
#check PCA results.
head(pca.info)
summary(pca.info) #You can see the specific explained variance for each PC.
head(pca.info$rotation) #regression coefficients
head(pca.info$sdev) #square root of eigenvalue
head(pca.info$x) #sample score
pca.data = data.frame(sample = rownames(pca.info$rotation),Type=c(rep("4_IBA",8),rep("5_MAR",8),rep("2_OCT",9), rep("1_SEP",9),rep("3_TOR",14)), pca.info$rotation)
#according to the name you generate when read your excel
m<-colnames(Brown_adipose_2022)[1:48]
#first version of pca plot
ggscatter(pca.data,x="PC1", y="PC2", color="Type",
          size=1, ellipse.border.remove=TRUE,
          , repel=TRUE, main="PCA plot"+theme_base())
#second version of pca plot
dat2=t(Brown_adipose_2022[,2:49])
dat2=as.data.frame(dat2)
Type=c(rep("4_IBA",8),rep("5_MAR",8),rep("2_OCT",9), rep("1_SEP",9),rep("3_TOR",14))
dat2$Type=Type
length(dat2[1,])
ord=prcomp(dat2[,1:356])
summary(ord)
dt=ord$x
df=data.frame(dt,dat2$Type)
head(df)
summ<-summary(ord)
xlab=paste0("PC1(",round(summ$importance[2,1]*100,2),"%)")
ylab=paste0("PC2(",round(summ$importance[2,2]*100,2),"%)")
ggplot(df,aes(df$PC1,df$PC2,color=df$dat2.Type))+
  stat_ellipse(aes(fill=df$dat2.Type),type="norm",geom="polygon",alpha=0.2,color=NA)+
  guides(fill="none")+
  geom_point()+
  labs(x=xlab,y=ylab,color="")





# Assuming Brown_adipose_2022 is your data frame
dat_pca <- Brown_adipose_2022[, 2:49]
pca.info <- fast.prcomp(dat_pca)

# Create a data frame for plotting
pca.data <- data.frame(sample = rownames(pca.info$rotation),
                       Type = c(rep("4_IBA", 8), rep("5_MAR", 8), rep("2_OCT", 9), rep("1_SEP", 9), rep("3_TOR", 14)),
                       pca.info$rotation)

# Extract column names for labeling
m <- colnames(Brown_adipose_2022)[1:48]

# PCA plot using ggscatter
p <- ggscatter(pca.data, x = "PC1", y = "PC2", color = "Type",
               size = 1, ellipse.border.remove = TRUE,
               label = c(m), repel = TRUE, main = "PCA plot" + theme_base())

# Add circles around points for each variable
for (variable in m) {
  data_subset <- pca.data[, c("PC1", "PC2", "Type", variable)]
  ellipse_params <- ellipse(cor(data_subset[, c("PC1", "PC2")]),
                            center = colMeans(data_subset[, c("PC1", "PC2")]),
                            level = 0.85)

  p <- p + geom_point(data = ellipse_params, aes(x = "PC1", y = "PC2"),
                      color = scales::hue_pal()(1)[as.numeric(factor(data_subset$Type[1]))],
                      size = 1.5, alpha = 0.2)
}

# Print the PCA plot
print(p)







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


