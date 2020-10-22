##package used
require(tidyverse)
require(ggplot2)

## read csv file for the raw data
Clean.Data <- read.csv("C:/Users/evely/Desktop/Clean Data.csv",header = TRUE)

Clean.Data[Clean.Data == ""] <- NA # change the empty values in the data to NA

colnames(Clean.Data)# check column names for the data

names(Clean.Data)[names(Clean.Data) == "Day.Of.Week"] <- "Date of Week" # rename columns for better presentation
names(Clean.Data)[names(Clean.Data) == "X"] <- "Day of Week"

colnames(Clean.Data) # check column names for the data

# > colnames(Clean.Data)# check column names for the data
# [1] "User.ID"      "Date of Week" "Day of Week"  "Show"         "Season"       "Episode"      "Time.Watched" "Gender"      
# [9] "Completed"    "Time.of.Day" 

# remove the first three letters of the first column if BOM exists
colnames(Clean.Data)[1] <- gsub('^...','',colnames(Clean.Data)[1])

cd <- drop_na(Clean.Data)# drop the NAs in the dataset since it occurs below 30% of the data

cd$Gender <- ifelse( cd$Gender %in% c('Female'),0,cd$Gender)#Assigning 0 and 1 value to gender
cd$Gender <- ifelse( cd$Gender %in% c('Male'),1,cd$Gender)

cd$Gender<-as.numeric(cd$Gender)#Changing the class of Gender

attach(cd)# attach the Netflix data

#checking the correlation of columns
cor(cd[,c(5:10)],use='pairwise')

# Season      Episode  Time.Watched       Gender    Completed   Time.of.Day
# Season        1.000000000 -0.010804109 -0.1158692466 -0.005562632 -0.210354896  0.0060357774
# Episode      -0.010804109  1.000000000 -0.0078205042  0.012360262  0.006969891  0.0085378601
# Time.Watched -0.115869247 -0.007820504  1.0000000000 -0.002900487  0.504925057 -0.0004766165
# Gender       -0.005562632  0.012360262 -0.0029004872  1.000000000  0.011569626  0.0007851250
# Completed    -0.210354896  0.006969891  0.5049250574  0.011569626  1.000000000 -0.0160035476
# Time.of.Day   0.006035777  0.008537860 -0.0004766165  0.000785125 -0.016003548  1.0000000000

cor.test(Gender, Time.Watched)# find out correlations from Season to Time of Day

# Pearson's product-moment correlation
# 
# data:  Gender and Time.Watched
# t = -0.15604, df = 2894, p-value = 0.876
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.03931984  0.03352656
# sample estimates:
#          cor 
# -0.002900487 

pairs(cd[,7:8]) # show the correlations between gender and time watched in plots

summary(cd)# perform and summary of the final cleaned data

# User.ID          Date of Week   Day of Week                       Show         Season         Episode        Time.Watched   
# Min.   :    66   2/10/2019:702   Monday  :722   American Horror Story  :685   Min.   : 1.00   Min.   : 1.000   Min.   :  1.00  
# 1st Qu.: 30804   2/11/2019:722   Saturday:732   Friends                :299   1st Qu.: 1.00   1st Qu.: 3.000   1st Qu.: 11.00  
# Median : 61461   2/12/2019:740   Sunday  :702   Orange Is The New Black:701   Median : 2.00   Median : 6.000   Median : 21.00  
# Mean   : 62045   2/9/2019 :732   Tuesday :740   Parks & Rec            :339   Mean   : 2.82   Mean   : 5.518   Mean   : 25.45  
# 3rd Qu.: 93812                                  Stranger Things        :872   3rd Qu.: 4.00   3rd Qu.: 8.000   3rd Qu.: 34.00  
# Max.   :124938                                                                Max.   :10.00   Max.   :10.000   Max.   :300.00  
# Gender        Completed      Time.of.Day    
# Min.   :0.000   Min.   :0.000   Min.   :0.0000  
# 1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.0000  
# Median :3.000   Median :0.000   Median :0.0000  
# Mean   :1.519   Mean   :0.259   Mean   :0.4962  
# 3rd Qu.:3.000   3rd Qu.:1.000   3rd Qu.:1.0000  
# Max.   :3.000   Max.   :1.000   Max.   :1.0000  
