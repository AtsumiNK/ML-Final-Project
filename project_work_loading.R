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


##########################################################################################################
# DATA EXPLORATION
df %>% select(gender, age, race, language, birth_country) %>% split(df$Y) %>% map(summary)







##########################################################################################################
# RUNNING MODELS 
# Step 1, split into train and test data
# Split back into train and test
set.seed(518)
shuffled_df = df[sample(1:nrow(df)),]
n = round((0.75 * nrow(shuffled_df)),0)
train = df[1:n,]
test = df[-(1:n),]


# Confirm that train and test are balanced with the outcome of interest (Y)
train %>% select(Y) %>% table() %>% prop.table()
test %>% select(Y) %>% table() %>% prop.table()

# Running a variety of models
# Logistic Regression
lr = with(train, glm(Y=="Y" ~ .,
         family = binomial("logit"),
         data = train))
lr %>% summary()




