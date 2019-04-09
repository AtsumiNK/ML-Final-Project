# References
# https://wwwn.cdc.gov/Nchs/Nnyfs/Y_BMX.htm
# https://wwwn.cdc.gov/nchs/nhanes/search/nnyfs12.aspx
# https://wwwn.cdc.gov/Nchs/Nnyfs/Y_DEMO.htm

# Loading in the libraries
library(Hmisc)
library(tidyr)
library(plyr)
library(dplyr)
library(readr)
library(haven)
library(foreign)
library(RNHANES)
library(purrr)
library(magrittr)
library(glmnet)

setwd('C:\\Users\\haley\\Desktop\\ML_Pipeline\\Final_Project\\')

# NHANES NNYFS 2012 data
demo = read_xpt("C:\\Users\\haley\\Desktop\\ML_Pipeline\\Final_Project\\Y_DEMO.xpt")
write.csv(demo, file = 'C:\\Users\\haley\\Desktop\\ML_Pipeline\\Final_Project\\attempt.csv')
demodf = read_csv("C:\\Users\\haley\\Desktop\\ML_Pipeline\\Final_Project\\attempt.csv")

# exam = read_xpt("C:\\Users\\haley\\Desktop\\ML_Pipeline\\Final_Project\\Y_CEX.xpt")
# write.csv(exam, file = 'exam.csv')
# examdf=read_csv('exam.csv')

# under examinations, body measures
body = read_xpt('C:\\Users\\haley\\Desktop\\ML_Pipeline\\Final_Project\\Y_BMX.xpt')
write.csv(body, file = 'body.csv')
bodydf=read_csv('body.csv')

sum(!is.na(bodydf$BMDBMIC)) # 1571 observations with the outcome of interest

#########################################################################################################
# DATA CLEANING

rm(demo)
rm(body)

# Remove the X1 columns from both dfs
demodf = demodf %>% select(-X1)
bodydf = bodydf %>% select(-X1)

# Join the demographic data to the body data
df = left_join(bodydf, demodf, by = 'SEQN')
# look at where there is missing data
colSums(is.na(df))

# Include only the demographic columns of interest and the Y variable (BMDBMIC)
df = df %>% select(RIAGENDR, RIDAGEYR, RIDRETH1, DMDBORN4, SIALANG, INDFMIN2, DMDHHSIZ, DMDHHSZA,
                   DMDHHSZB, DMDHHSZF, DMDHRMAR, BMDBMIC, DMDHREDU) 

# Rename the columns, so they are easier to understand
names(df)[1]<-"gender"
names(df)[2]<-"age"
names(df)[3]<-"race"
names(df)[4]<-"birth_country"
names(df)[5]<-"language"
names(df)[6]<-"annual_family_income"
names(df)[7]<-"people_in_household"
names(df)[8] <-"num_children_5yrs_younger"
names(df)[9] <-"num_6_to_17"
names(df)[10] <-"ind_adults60_over"
names(df)[11] <-"marital_status_ref"
names(df)[12] <-"Y_BMI_category"
names(df)[13] <-"education_level_ref"

df %>% summary()


# Clean the outcome variable so it is a biclassification problem instead of multi-class
df = df %>% filter(!is.na(Y_BMI_category)) # will remove 5 rows of data missing the outcome

# Relevel the outcome to have just two levels: 1 is healthy (normal weight) and 0 is unhealthy
# (which includes underweight, overweight and obese)
df = df %>% mutate(Y = case_when(
  Y_BMI_category == 2 ~ "Y",
  Y_BMI_category == 1 ~ "N",
  Y_BMI_category == 3 ~ "N",
  Y_BMI_category == 4 ~ "N"
)) %>% select(-Y_BMI_category)

# Need to change levels of factors for predictors: gender, race, birth_country, language, 
# marital_status, education_level 
cols <- c('gender', 'race', 'birth_country', 'language', 'marital_status_ref',
          'education_level_ref', 'ind_adults60_over')

df[cols] <- lapply(df[cols], factor)

# Gender, 1 is male and 2 is female
df$gender <- ordered(df$gender,
                     levels = c(1,2),
                     labels = c("male", "female"))

# Language, 1 is English and 2 is Spanish
df$language <- ordered(df$language,
                     levels = c(1,2),
                     labels = c("English", "Spanish"))

# Race, 1 is Mexican American, 2 is Other Hispanic, 3 Non-Hispanic White, 4 Non-Hispanic Black,
# 5 Other Race
df$race <- ordered(df$race,
                     levels = c(1,2,3,4,5),
                     labels = c("Mexican American", "Other Hispanic","White",
                                "Black", "Other"))

# birth_country, 1 is US, 2 is Other
df$birth_country <- ordered(df$birth_country,
                   levels = c(1,2),
                   labels = c("USA", "Other"))

# marital_status_ref, 
df$marital_status_ref <- ordered(df$marital_status_ref,
                   levels = c(1,2,3,4,5,6, 77,99),
                   labels = c("Married", "Widowed","Divorced", "Separated", "Never Married",
                              "Living with Partner", "Refused", "Don't Know"))

# education_level_ref, 
df$education_level_ref <- ordered(df$education_level_ref,
                                 levels = c(1,2,3,4, 5, 9),
                                 labels = c("No High School", "Some High School", "High School Grad",
                                            "Some College or Associates", "College Graduate or Above",
                                            "Don't Know"))

# annual_family_income_imputed, 77 is Refused and 99 is Don't Know
df$annual_family_income = na_if(df$annual_family_income, 99)
df$annual_family_income = na_if(df$annual_family_income, 77)
sum(is.na(df$annual_family_income))

##########################################################################################################
# DATA EXPLORATION
df %>% select(gender, age, race, language, birth_country) %>% split(df$Y) %>% map(summary)







##########################################################################################################
# IMPUTING MISSING VALUES FOR marital_status_ref and eductation_level_ref
colSums(is.na(df))
### As a function you could call: does it all!
# Step 3, perform multiple (single) imputation for missing X's
# I will perform imputation on ALL the data and then split back into train and test
library(mice)  
mdat = mice(df %>% 
              select(-Y) %>% 
              mutate_if(is.character, as.factor),m=1,maxit = 1) 

complete_and_indicate = function(mdat, labeldf, iter=1, indicators=T) {
  idat = complete(mdat, iter) %>% as_tibble()
  names(idat) = lapply(names(idat), paste0, "_imputed")
  if(!indicators) {
    imputedata = labeldf %>% 
      bind_cols(idat) %>% 
      as_tibble()
  } else {
    imputedata = labeldf %>% 
      bind_cols(idat, 
                mdat$where %>% as_tibble() %>% rename_all(
                  ~ paste0(., "_missing"))
      ) %>%
      as_tibble()
  }
  return(imputedata)
}

all_imputed = complete_and_indicate(mdat, df %>% select(Y))  # e.g.

# Step 4, use knowledge about the data distribution in train to impute X values in test
sum(is.na(all_imputed)) # Yay, no more NAs!

#################################################################################################################
# RUNNING MODELS 
# Step 1, split into train and test data
# Split back into train and test
set.seed(518)
shuffled_df = all_imputed[sample(1:nrow(all_imputed)),]
n = round((0.75 * nrow(shuffled_df)),0)
train = all_imputed[1:n,]
test = all_imputed[-(1:n),]


# Confirm that train and test are balanced with the outcome of interest (Y)
train %>% select(Y) %>% table() %>% prop.table()
test %>% select(Y) %>% table() %>% prop.table()

# Running a variety of models
# Logistic Regression
lr = with(train, glm(Y=="Y" ~ .,
         family = binomial("logit"),
         data = train))
lr %>% summary()

# Show a decision tree
library(rpart); library(rpart.plot)
tree = rpart(train, formula = Y=='Y' ~ .)
rpart.plot(tree, branch.type=5)


# Random Forest
# Make the response variable a factor again so R knows to do binary classification for RF
# consider ntrees for cross validation
library(randomForest)
train$Y <- as.factor(train$Y)
forest = randomForest(formula = Y ~ .,
                      data=train, ntrees=10)

# Boosting
# Example adaboost
library(ada)
aforest = ada(formula = Y=="Y" ~ .,
              data=train,
              iter=10)


# Gradient Boosting
# Example gradient boosted forest
library(gbm)
gforest = gbm(formula = Y=="Y" ~ .,
              data=train %>%
                mutate_if(is.logical, as.factor),
              interaction.depth=10,
              cv.folds = 2)

#########################################################################################################################
library(ROCR)

# Logistic Regression
logit_prediction = predict(lr,test)
logit_rocdata = prediction(predictions=logit_prediction,
                           labels=test$Y) %>%
  performance("tpr", "fpr") %>%
  (function(.) data.frame(FPR=.@x.values[[1]], TPR=.@y.values[[1]]) %>% as_tibble())(.) 

auc_lg =  prediction(predictions=logit_prediction,
                     labels=test$Y) %>% performance("auc")
auc_lg = round(auc_lg@y.values[[1]],3)

# Decision Tree
dt_prediction = predict(tree,test)
dt_rocdata = prediction(predictions=dt_prediction,
                           labels=test$Y) %>%
  performance("tpr", "fpr") %>%
  (function(.) data.frame(FPR=.@x.values[[1]], TPR=.@y.values[[1]]) %>% as_tibble())(.) 

auc_dt =  prediction(predictions=dt_prediction,
                     labels=test$Y) %>% performance("auc")
auc_dt = round(auc_dt@y.values[[1]],3)

# Random Forest
test$Y <- as.factor(test$Y)
rf_prediction = predict(forest, test, type = "prob")
rf_rocdata = prediction(predictions=rf_prediction[,2],
                        labels=test$Y) %>%
  performance("tpr", "fpr") %>%
  (function(.) data.frame(FPR=.@x.values[[1]], TPR=.@y.values[[1]]) %>% as_tibble())(.) 

auc_rf =   prediction(predictions=rf_prediction[,2],
                      labels=test$Y) %>% performance("auc")
auc_rf = round(auc_rf@y.values[[1]],3)

#boosting
boosting_prediction = predict(aforest, test, type = "prob")
boosting_rocdata = prediction(predictions=boosting_prediction[,2],
                              labels=test$Y) %>%
  performance("tpr", "fpr") %>%
  (function(.) data.frame(FPR=.@x.values[[1]], TPR=.@y.values[[1]]) %>% as_tibble())(.) 

auc_boosting = prediction(predictions=boosting_prediction[,2],
                          labels=test$Y) %>% performance("auc")
auc_boosting = round(auc_boosting@y.values[[1]],3)

#gradient boosting
gradient_prediction = predict(gforest, test,
                              n.trees=gforest$n.trees,type = "response")
gradient_rocdata = prediction(predictions=gradient_prediction,
                              labels=test$Y) %>%
  performance("tpr", "fpr") %>%
  (function(.) data.frame(FPR=.@x.values[[1]], TPR=.@y.values[[1]]) %>% as_tibble())(.) 

auc_gradient = prediction(predictions=gradient_prediction,
                          labels=test$Y) %>% performance("auc")
auc_gradient = round(auc_gradient@y.values[[1]],3)


# plot ROC curve
ggplot(data = logit_rocdata, aes(x=FPR,y=TPR, col = paste0("Logistic Regression: AUC", auc_lg))) + geom_line() +
  geom_line(data = dt_rocdata, aes(x=FPR,y=TPR, col = paste0("DT: AUC", auc_dt))) + 
  geom_line(data = rf_rocdata, aes(x=FPR,y=TPR, col = paste0("RF: AUC", auc_rf))) +
  geom_line(data = boosting_rocdata, aes(x=FPR,y=TPR, col = paste0("Boosting: AUC", auc_boosting))) +
  geom_line(data = gradient_rocdata, aes(x=FPR,y=TPR, col = paste0("Gradient boosting: AUC", auc_gradient))) + 
    theme(legend.title=element_blank())


