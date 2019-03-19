library(tidytext)
library(shiny)
library(ggplot2)
library(ggpubr)

# ## install.packages("usethis")
# 
# library(usethis)
# use_git_config(user.name = "abannachbrown", user.email = "a.bannach-brown@ed.ac.uk")

depression_data <- read.csv("~/all_all.csv", sep=",")

summary(depression_data$forcedswimRegex)

## change 0 values to NAs in order to plot without zeros
test <- ifelse(depression_data$forcedswimRegex!=0, depression_data$forcedswimRegex, NA)
test2 <- ifelse(depression_data$fluoxetineRegex!=0, depression_data$fluoxetineRegex, NA)



#model summary - looking at data
summary(depression_data[,22:59])

# put data in new dataframe
model <- depression_data[,22:59]
# convert all 0 to NAs
model[model==0] <- NA

#drug summary - look at data
summary(depression_data[,61:176])

# put data in new dataframe
drug <- depression_data[,61:176]
# convert all 0 to NAs
drug[drug==0] <- NA


## remove rows will all NA??
model_clean <- model[rowSums(is.na(model[,1:38]))!=38,]
## model reduced from 18409 --> 14832

drug_clean <- drug[rowSums(is.na(drug[,1:116]))!=116,]
### drug reduced from 18409 --> 14149

## remove columns with all NAs
colnames_model <- dimnames(model)[[2]]

model_clean <- model_clean[,colSums(is.na(model_clean))<nrow(model_clean)]
colname_model_clean <- dimnames(model_clean)[[2]]
# down to 37 cols for model

## remove columns with all NAs
colnames_drug <- dimnames(drug)[[2]]

drug_clean <- drug_clean[,colSums(is.na(drug_clean))<nrow(drug_clean)]
colname_drug_clean <- dimnames(drug_clean)[[2]]
# down to 105 cols for drug

# Multiple histograms for model

for (i in seq_along(model_clean)) {
  png(paste(names(model_clean)[i], ".png"), width=350, height = 350)
  a <- hist(model_clean[,i], xlim=c(0, 200), breaks=100, main=names(model_clean)[i], 
            freq=TRUE, col="gray", border="white")
  
  dev.off()
}

# png("forcedswimRegex.png", width=350, height = 350)
# hist(model_clean$forcedswimRegex, xlim=c(0, 200), breaks=100, main=names(model_clean$forcedswimRegex)[i], 
#      freq=TRUE, col="gray", border="white")
# 
# dev.off()

#mulitple histograms for drug
for (i in seq_along(drug_clean)){
  png(paste(names(drug_clean)[i], ".png"), width=350, height = 350)
  a <- hist(drug_clean[,i], xlim=c(0, 200), breaks=100, main=names(drug_clean)[i], 
            freq=TRUE, col="gray", border="white")
  
  dev.off()
  
}



## reduce document to all FST

FST_only <- depression_data[!depression_data$forcedswimRegex==0, ]
# 7186



