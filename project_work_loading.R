library(Hmisc)
library(tidyr)
library(dplyr)
library(readr)
library(haven)
library(foreign)
library(RNHANES)

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

rm(demo)
rm(body)

# Remove the X1 columns from both dfs
demodf = demodf %>% select(-X1)
bodydf = bodydf %>% select(-X1)

# Join the demographic data to the body data
df = left_join(bodydf, demodf, by = 'SEQN')
colSums(is.na(df))

# Include only the demographic columns of interest and the Y variable (BMDBMIC)
df = df %>% select(RIAGENDR, RIDAGEYR, RIDRETH1, DMDBORN4, SIALANG, INDFMIN2, DMDHHSIZ, DMDHHSZA,
                   DMDHHSZB, DMDHHSZF, DMDHRMAR, BMDBMIC)

df %>% summary()
