library(tidyverse)
library(tidymodels)
library(skimr)
library(doParallel)
library(kknn)



data2020 <- read.csv("D:\\DMML projects\\Filght delay prediction\\Jan_2020_ontime.csv")
d2020 <- data2020[1:500,]

######### EDA
mydata <- d2020[, c(14,15,17,18,19)]
head(mydata)
cormat <- round(cor(mydata),3)
head(cormat)
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_raster()

library(reshape2)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


#########################

df_all <- d2020 %>% 
  filter(CANCELLED==0) %>%
  filter(!is.na(ARR_DEL15)) %>% 
  select(DISTANCE,
         DEP_DEL15,
         ARR_DEL15,
         OP_CARRIER)

#Change columns to factor
cols_as_factor <- c("DEP_DEL15", "ARR_DEL15", "OP_CARRIER")
df_all[cols_as_factor] <- lapply(df_all[cols_as_factor], factor)

#Verifying proportion of delayed flights (arrivals).
df_all %>% count(ARR_DEL15) %>% mutate(prop = n / sum(n))

#Verify selected features
skim(df_all)

#Split data - training/testing set
df_split <- initial_split(df_all, strata = ARR_DEL15)

df_train <- training(df_split)
df_test <- testing(df_split)


#KNN
#Defining KNN specification
knn_spec <- nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("classification")

#Training data is fit to the model
knn_fit <- knn_spec %>%
  fit(ARR_DEL15 ~ ., data = df_train)

knn_fit

#Evaluating the trained data against the test data
knn_fit %>% 
  predict(new_data = df_test, type = "prob") %>% 
  mutate(truth = df_test$ARR_DEL15) %>% 
  roc_auc(truth, .pred_0)

