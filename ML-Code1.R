setwd("D:/MBA_3RD SEM/ML")
getwd()

a<-"D:/MBA_3RD SEM/ML/German-Credit_1.csv"
read.csv(a)
b<-"D:/MBA_3RD SEM/ML/German-Credit_2.csv"
read.csv(b)

readLines(con = "German-Credit_1.csv", n = 5)
df1 <- read.table(file ="German-Credit_1.csv", header = T, sep = ",")
head(df1)

df1 <- read.csv(file = "German-Credit_1.csv", header=T)
head(df1)

#Merging	two	datasets

df1 <- read.csv(file = "German-Credit_1.csv", header = TRUE)
head(df1)

df2 <- read.csv(file = "German-Credit_2.csv", header = TRUE)
head(df2)

#Merging df1 and df2 by the common variable OBS

df3 <- merge(x = df1, y = df2, by = "OBS", all = T)
head(df3)

#Functions

head(df3)
tail(df3)
colnames(df3)
summary(df3)
str(df3)


# Create a vector of all the column names that you know have categorical attributes

num_Attr <- c("DURATION","AMOUNT","INSTALL_RATE","AGE","NUM_CREDITS","NUM_DEPENDENTS")

cat_Attr <- setdiff(x = colnames(df3), y = num_Attr)

df3$OBS <- as.character(df3$OBS)

test_vec <- c(0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0)
test_vec

fac_vec <- as.factor(test_vec)
fac_vec

reconverted_vec <- as.numeric(fac_vec)
reconverted_vec

df3$RESPONSE <- as.factor(as.character(df3$RESPONSE))

df_cat <- subset(df3,select =cat_Attr)
df3[,cat_Attr] <- data.frame(apply(df_cat, 2, function(x) as.factor(as.character(x))))
str(df3)                                                                  


#Handling	Missing	Values

colSums(is.na(x = df3))
sum(is.na(df3))

#Dropping	the	records	with	missing	values
df4 <- na.omit(df3)
dim(df3)
dim(df4)
sum(is.na(df4))

#Imputing	/	Substituting	missing	values
install.packages("DMwR")
library(DMwR)

install.packages("lattice")
library(lattice)

install.packages("grid")
library(grid)

manyNAs(df3, 0.1)

#Cenral Imputation
df3_imputed <- centralImputation(data = df3) 
sum(is.na(df3_imputed))

#KNN Imputation
df3_imputed1 <- knnImputation(data = df3, k=5) 
sum(is.na(df3_imputed1))

#Binning	/	Discretizing	the	variable

library(infotheo)

x <- c(5,6,7,8,8,8,8,8,11,20,21,22)
length(x)

x0 <- discretize(x, disc = "equalfreq", nbins = 4)
table(x0)
x1 <- discretize(x, disc = "equalwidth", nbins = 4)
table(x1)

#bin the AMOUNT variable

AmtBin <- discretize(df3_imputed$AMOUNT, disc="equalfreq",nbins=4)
table(AmtBin)

AmtBin <- discretize(df3_imputed$AMOUNT, disc="equalwidth",nbins=4)
table(AmtBin)

#Dummy	Variables

library(dummies)
df_ex <- datasets::warpbreaks
table(df_ex$tension)

dummy_ex <- dummy(df_ex$tension)
head(dummy_ex)

df_cat <- subset(df3_imputed,select =cat_Attr)
df_cat_dummies <- data.frame(apply(df_cat, 2, function(x) dummy(x)))
dim(df_cat_dummies)

#Standardizing	the	data
library(vegan)
df_num <- df3_imputed[, num_Attr]

# using range method 
df_num2 <- decostand(x = df_num, method = "range")
summary(df_num2)

# Using Z score method
df_num3 <- decostand(x = df_num, method = "standardize") 
summary(df_num3)

df_final <- cbind(df_num3,df_cat)
head(df_final)

#Train	& Test	Split

rows <- seq(1,1000,1)
set.seed(123)
trainRows <- sample(rows,600)
train_data <- df_final[trainRows,]
test_data <- df_final[-c(trainRows),]
dim(train_data)
dim(test_data)

#Build	model
lm_model <- lm(AMOUNT~DURATION, data=train_data)
summary(lm_model)

#Data Visualizations

## Histogram
df <- df_final 
hist(df$AGE)
hist(df$AGE,col = "pink")

## Box plot
boxplot(df$AGE,horizontal = TRUE)
boxplot(AMOUNT~RESPONSE, data = df, xlab ="TARGET", ylab = "AMOUNT", main ="Continuous v/s Categorical")
 

## Bar plot
barplot(table(df$RESPONSE))         
barplot(table(df$RESPONSE),col = "blue")

## Scatter Plot
plot(x=df$AGE,y =df$AMOUNT ,xlab = "DURATION",ylab="AMOUNT",main="Continuous v/s Continuous") 
