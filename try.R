
#Starting Individual Project


head(Brown_adipose_2022)
#install.packages("openxlsx")
library(openxlsx)
?read.xlsx
Brown_adipose_2022 = read.xlsx("data/Brown adipose 2022.xlsx",rowNames = F) # keep the row names
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

# PCA plot for entire data
dat2=t(Brown_adipose_2022[,2:49])
dat2[1:8,1:8]
length(dat2[1,])#column number(356)
length(dat2[,1])#row number (48)
#prepare the label for the same varible
Type=c(rep("4_IBA",8),rep("5_MAR",8),rep("2_OCT",9), rep("1_SEP",9),rep("3_TOR",14))
Type
#first we need to check if our data is in data frame format.
class(dat2) #class means type
dat2=as.data.frame(dat2) #we will make it into data frame format.
class(dat2)
dat2$Type=Type #we want to add the variable label to our data frame.
dat2[1:2,] #we look at the first 2 rows to see if the it is in variable label.
ord=prcomp(dat2[,1:356]) #start to run PCA because the new dat2 has a final column isn't numeric so it won't be able to read it.
summary(ord) #summary the PCA result.
dt=ord$x # then wed to isolate x to create the PCA plot data.
df=data.frame(dt,dat2$Type) #this function to help you create data frame which combine the PCA result and the variable label together
head(df)#use to check data frame
summ<-summary(ord)#give the summary of summ.
xlab=paste0("PC1(",round(summ$importance[2,1]*100,2),"%)") #generate the x label for the plot PC1
ylab=paste0("PC2(",round(summ$importance[2,2]*100,2),"%)") #generate the y label for the plot PC2
ggplot(df,aes(df$PC1,df$PC2,color=df$dat2.Type))+ #to generate the graph
  stat_ellipse(aes(fill=df$dat2.Type),type="norm",geom="polygon",alpha=0.2,color=NA)+
  guides(fill="none")+
  geom_point()+
  labs(x=xlab,y=ylab,color="")


dat2=t(Brown_adipose_2022[,2:49])
dat2[1:8,1:8]
length(dat2[1,])#column number(356)
length(dat2[,1])#row number (48)
#prepare the label for the same varible
Type=c(rep("4_IBA",8),rep("5_MAR",8),rep("2_OCT",9), rep("1_SEP",9),rep("3_TOR",14))
Type
#first we need to check if our data is in data frame format.
class(dat2) #class means type
dat2=as.data.frame(dat2) #we will make it into data frame format.
class(dat2)
dat2$Type=Type #we want to add the variable label to our data frame.
dat2[1:2,] #we look at the first 2 rows to see if the it is in variable label.
ord=prcomp(dat2[,1:356]) #start to run PCA because the new dat2 has a final column isn't numeric so it won't be able to read it.
summary(ord) #summary the PCA result.
dt=ord$x # then wed to isolate x to create the PCA plot data.
df=data.frame(dt,dat2$Type) #this function to help you create data frame which combine the PCA result and the variable label together
head(df)#use to check data frame
summ<-summary(ord)#give the summary of summ
xlab=paste0("PC1(",round(summ$importance[2,1]*100,2),"%)") #generate the x label for the plot PC1
ylab=paste0("PC2(",round(summ$importance[2,2]*100,2),"%)") #generate the y label for the plot PC2
ggplot(df,aes(df$PC1,df$PC2,color=df$dat2.Type))+ #to generate the graph
  stat_ellipse(aes(fill=df$dat2.Type),type="norm",geom="polygon",alpha=0.2,color=NA)+
  guides(fill="none")+
  geom_point()+
  labs(x=xlab,y=ylab,color="")

#### Heatmap

a=Brown_adipose_2022 %>%
  mutate(feature=seq(1:nrow(Brown_adipose_2022))) %>% #Adds a new column named 'feature' containing sequential numbers from 1 to the number of rows in
  select(-Feature) %>% #Excludes the column named 'Feature' from the dataframe.
  pivot_longer(-feature)#Reshapes the data from wide to long format, creating a dataframe (a) with columns: 'feature', 'name' (variable names), and 'value' (corresponding values).

#whole data heatmap plot
dat=Brown_adipose_2022[,2:49]# Extracts columns 2 to 49 (assumed to be the compound data) from the original dataframe.
dat1=apply(dat,1,function(x) scale(x)) #Applies the scale function row-wise to standardize the values.
t(dat1) #Transposes the standardized data.
dat2=data.frame(Feature=Brown_adipose_2022[,1]) #Creates a dataframe (dat2) with a 'Feature' column from the first column of the original dataframe.
dat3=cbind(dat2, t(dat1)) #Combines 'Feature' column with the transposed standardized data.

library(ggplot2)# generate the heatmap
ggplot(data = pivot_longer(dat3, -Feature), aes(x = name, y = Feature, fill = value)) + # Uses the pivot_longer function to convert the dataframe from wide to long format, creating columns 'name' (variable names), 'Feature', and 'value' (corresponding values). Specifies the aesthetic mappings. 'name' is mapped to the x-axis, 'Feature' to the y-axis, and 'value' to the fill color.
  geom_tile() + #Adds tiles to the plot, creating a heatmap representation of the data.
  scale_fill_gradient(low = "white", high = "red") + # Sets the color scale for the fill color. The heatmap will have a gradient from white (low values) to red (high values).
  theme_minimal() + ##Applies the 'minimal' theme to the plot, providing a clean and minimalistic appearance.
  labs(title = "Heatmap") + #Sets the title of the plot to "Heatmap"
  theme(axis.text.x = element_text(angle = 48, hjust = 1)) + #rotates the x-axis text by 48 degrees, and hjust = 1 right-aligns the text.
  theme(axis.text.y=element_blank(), axis.text.x=element_blank()) #Hides the y-axis and x-axis text, providing a cleaner look when axis labels are not necessary.
#MAR data heatmap

#variable data heatmap
#The data should be separate according to the variable
Brown_adipose_2022[1,] #check the variable id and get the column of order of each variable
#According to the results shown in the console, the variables are in the order:
#1 columns 2-9 are IBA.
#2 columns 10-17 are MAR.
#3 columns 18-26 are OCT.
#4 columns 27-35 are SEPT.
#5 columns 36-49 are TOR.

#IBA data heatmap
dat=Brown_adipose_2022[,2:9] #Extracts columns 2 to 9 from the dataframe
dat1=apply(dat,1,function(x) scale(x)) #Applies the scale function row-wise to standardize the values in each row of the "IBA" data. Standardization ensures that each row has a mean of 0 and a standard deviation of 1.
t(dat1) # Transposes the standardized data (dat1). This is done to make rows correspond to features (variables) and columns correspond to samples.
dat2=data.frame(Feature=Brown_adipose_2022[,1]) #Creates a dataframe (dat2) with a single column named "Feature" containing the values from the first column of the original dataframe Brown_adipose_2022. This column typically represents the feature names or IDs.
dat3=cbind(dat2, t(dat1)) #Combines the "Feature" column with the transposed standardized data (t(dat1)) to create a new dataframe (dat3). This dataframe will be used for plotting the heatmap.

#library(ggplot2)
ggplot(data = pivot_longer(dat3, -Feature), aes(x = name, y = Feature, fill = value)) + # Uses the pivot_longer function to convert the dataframe from wide to long format, creating columns 'name' (variable names), 'Feature', and 'value' (corresponding values). Specifies the aesthetic mappings. 'name' is mapped to the x-axis, 'Feature' to the y-axis, and 'value' to the fill color.
  geom_tile() + #Adds tiles to the plot, creating a heatmap representation of the data.
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "black", high = "red") + #Sets the color scale for the fill color. The heatmap will have a gradient from Blue (low values) to White (high values).
  theme_minimal() + #Applies the 'minimal' theme to the plot, providing a clean and minimalistic appearance.
  labs(title = "Heatmap") + #Sets the title of the plot to "Heatmap"
  theme(axis.text.x = element_text(angle = 48, hjust = 1)) + #rotates the x-axis text by 48 degrees, and hjust = 1 right-aligns the text.
  theme(axis.text.y=element_blank(), axis.text.x=element_blank()) #Hides the y-axis and x-axis text, providing a cleaner look when axis labels are not necessary.
#MAR data heatmap
dat=Brown_adipose_2022[,10:17] #Extracts columns 10 to 17 from the dataframe
dat1=apply(dat,1,function(x) scale(x)) #Applies the scale function row-wise to standardize the values in each row of the "MAR" data. Standardization ensures that each row has a mean of 0 and a standard deviation of 1.
t(dat1) # Transposes the standardized data (dat1). This is done to make rows correspond to features (variables) and columns correspond to samples.
dat2=data.frame(Feature=Brown_adipose_2022[,1]) #Creates a dataframe (dat2) with a single column named "Feature" containing the values from the first column of the original dataframe Brown_adipose_2022. This column typically represents the feature names or IDs.
dat3=cbind(dat2, t(dat1)) # Combines the "Feature" column with the transposed standardized data (t(dat1)) to create a new dataframe (dat3). This dataframe will be used for plotting the heatmap.
#library(ggplot2)
ggplot(data = pivot_longer(dat3, -Feature), aes(x = name, y = Feature, fill = value)) + # Uses the pivot_longer function to convert the dataframe from wide to long format, creating columns 'name' (variable names), 'Feature', and 'value' (corresponding values). Specifies the aesthetic mappings. 'name' is mapped to the x-axis, 'Feature' to the y-axis, and 'value' to the fill color.
  geom_tile() + #Adds tiles to the plot, creating a heatmap representation of the data.
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "black", high = "red") + #Sets the color scale for the fill color. The heatmap will have a gradient from Blue (low values) to White (high values).
  theme_minimal() + #Applies the 'minimal' theme to the plot, providing a clean and minimalistic appearance.
  labs(title = "Heatmap") + #Sets the title of the plot to "Heatmap"
  theme(axis.text.x = element_text(angle = 48, hjust = 1)) + #rotates the x-axis text by 48 degrees, and hjust = 1 right-aligns the text.
  theme(axis.text.y=element_blank(), axis.text.x=element_blank()) #Hides the y-axis and x-axis text, providing a cleaner look when axis labels are not necessary.
#OCT DATA HEATMAP
dat=Brown_adipose_2022[,18:26] #Extracts columns 18 to 26 from the dataframe
dat1=apply(dat,1,function(x) scale(x)) #Applies the scale function row-wise to standardize the values in each row of the "OCT" data. Standardization ensures that each row has a mean of 0 and a standard deviation of 1.
t(dat1) # Transposes the standardized data (dat1). This is done to make rows correspond to features (variables) and columns correspond to samples.
dat2=data.frame(Feature=Brown_adipose_2022[,1]) # Creates a dataframe (dat2) with a single column named "Feature" containing the values from the first column of the original dataframe Brown_adipose_2022. This column typically represents the feature names or IDs.
dat3=cbind(dat2, t(dat1)) #Combines the "Feature" column with the transposed standardized data (t(dat1)) to create a new dataframe (dat3). This dataframe will be used for plotting the heatmap.
#library(ggplot2)
ggplot(data = pivot_longer(dat3, -Feature), aes(x = name, y = Feature, fill = value)) + # Uses the pivot_longer function to convert the dataframe from wide to long format, creating columns 'name' (variable names), 'Feature', and 'value' (corresponding values). Specifies the aesthetic mappings. 'name' is mapped to the x-axis, 'Feature' to the y-axis, and 'value' to the fill color.
  geom_tile() + #Adds tiles to the plot, creating a heatmap representation of the data.
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "black", high = "red") + #Sets the color scale for the fill color. The heatmap will have a gradient from Blue (low values) to White (high values).
  theme_minimal() + #Applies the 'minimal' theme to the plot, providing a clean and minimalistic appearance.
  labs(title = "Heatmap") + #Sets the title of the plot to "Heatmap"
  theme(axis.text.x = element_text(angle = 48, hjust = 1)) + #rotates the x-axis text by 48 degrees, and hjust = 1 right-aligns the text.
  theme(axis.text.y=element_blank(), axis.text.x=element_blank()) #Hides the y-axis and x-axis text, providing a cleaner look when axis labels are not necessary.
# SEP DATA HEATMAP
dat=Brown_adipose_2022[,27:35] #Extracts columns 27 to 35 from the dataframe
dat1=apply(dat,1,function(x) scale(x)) #Applies the scale function row-wise to standardize the values in each row of the "SEP" data. Standardization ensures that each row has a mean of 0 and a standard deviation of 1.
t(dat1) #Transposes the standardized data (dat1). This is done to make rows correspond to features (variables) and columns correspond to samples.
dat2=data.frame(Feature=Brown_adipose_2022[,1]) #Creates a dataframe (dat2) with a single column named "Feature" containing the values from the first column of the original dataframe Brown_adipose_2022. This column typically represents the feature names or IDs.
dat3=cbind(dat2, t(dat1)) #Combines the "Feature" column with the transposed standardized data (t(dat1)) to create a new dataframe (dat3). This dataframe will be used for plotting the heatmap.
#library(ggplot2)
ggplot(data = pivot_longer(dat3, -Feature), aes(x = name, y = Feature, fill = value)) + # Uses the pivot_longer function to convert the dataframe from wide to long format, creating columns 'name' (variable names), 'Feature', and 'value' (corresponding values). Specifies the aesthetic mappings. 'name' is mapped to the x-axis, 'Feature' to the y-axis, and 'value' to the fill color.
  geom_tile() + #Adds tiles to the plot, creating a heatmap representation of the data.
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "black", high = "red") + #Sets the color scale for the fill color. The heatmap will have a gradient from Blue (low values) to White (high values).
  theme_minimal() + #Applies the 'minimal' theme to the plot, providing a clean and minimalistic appearance.
  labs(title = "Heatmap") + #Sets the title of the plot to "Heatmap"
  theme(axis.text.x = element_text(angle = 48, hjust = 1)) + #rotates the x-axis text by 48 degrees, and hjust = 1 right-aligns the text.
  theme(axis.text.y=element_blank(), axis.text.x=element_blank()) #Hides the y-axis and x-axis text, providing a cleaner look when axis labels are not necessary.
# TOR DATA HEATMAP
dat=Brown_adipose_2022[,36:49] #Extracts columns 36 to 49.
dat1=apply(dat,1,function(x) scale(x)) #Applies the scale function row-wise to standardize the values in each row of the "TOR" data. Standardization ensures that each row has a mean of 0 and a standard deviation of 1.
t(dat1) #Transposes the standardized data (dat1). This is done to make rows correspond to features (variables) and columns correspond to samples.
dat2=data.frame(Feature=Brown_adipose_2022[,1]) #Creates a dataframe (dat2) with a single column named "Feature" containing the values from the first column of the original dataframe Brown_adipose_2022. This column typically represents the feature names or IDs.
dat3=cbind(dat2, t(dat1)) #Combines the "Feature" column with the transposed standardized data (t(dat1)) to create a new dataframe (dat3). This dataframe will be used for plotting the heatmap.
library(ggplot2)
ggplot(data = pivot_longer(dat3, -Feature), aes(x = name, y = Feature, fill = value)) + # Uses the pivot_longer function to convert the dataframe from wide to long format, creating columns 'name' (variable names), 'Feature', and 'value' (corresponding values). Specifies the aesthetic mappings. 'name' is mapped to the x-axis, 'Feature' to the y-axis, and 'value' to the fill color.
  geom_tile() + #Adds tiles to the plot, creating a heatmap representation of the data.
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "black", high = "red") + #Sets the color scale for the fill color. The heatmap will have a gradient from Blue (low values) to White (high values).
  theme_minimal() + #Applies the 'minimal' theme to the plot, providing a clean and minimalistic appearance.
  labs(title = "Heatmap") + #Sets the title of the plot to "Heatmap"
  theme(axis.text.x = element_text(angle = 48, hjust = 1)) + #rotates the x-axis text by 48 degrees, and hjust = 1 right-aligns the text.
  theme(axis.text.y=element_blank(), axis.text.x=element_blank()) #Hides the y-axis and x-axis text, providing a cleaner look when axis labels are not necessary.
# Function to determine top metabolites for each variable
get_top_metabolites <- function(data, variable_columns, num_top_metabolites = 10) {
  dat = data[, variable_columns]
  dat1 = apply(dat, 1, function(x) scale(x))
  top_metabolites_indices = apply(dat1, 1, function(x) order(x, decreasing = TRUE)[1:num_top_metabolites])
  top_metabolites = rownames(dat)[top_metabolites_indices]
  return(top_metabolites)
}

# Determine top metabolites for the entire dataset
top_metabolites_all = get_top_metabolites(Brown_adipose_2022, 2:49)

# Determine top metabolites for each variable
top_metabolites_IBA = get_top_metabolites(Brown_adipose_2022, 2:9)
top_metabolites_MAR = get_top_metabolites(Brown_adipose_2022, 10:17)
top_metabolites_OCT = get_top_metabolites(Brown_adipose_2022, 18:26)
top_metabolites_SEP = get_top_metabolites(Brown_adipose_2022, 27:35)
top_metabolites_TOR = get_top_metabolites(Brown_adipose_2022, 36:49)

# Print  the top metabolites
print(top_metabolites_all)
print(top_metabolites_IBA)
print(top_metabolites_MAR)
print(top_metabolites_OCT)
print(top_metabolites_SEP)
print(top_metabolites_TOR)


