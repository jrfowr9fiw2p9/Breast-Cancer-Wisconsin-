#reading data
wbcd <- read.csv("C:\\Users\\Jason Dias\\Downloads\\wisc_bc_data.csv", stringsAsFactors = FALSE)
wbcd

#Checking out the Structure
str(wbcd)

#drop wbcd
wbcd <- wbcd[-1]

#factor 
table(wbcd$diagnosis)

#factor proprtion
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)


summary(wbcd[c('radius_mean', 'area_mean', 'smoothness_mean')])

#min-max normalization of data set
normalizer <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}


wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalizer))

#train test split
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

#model training
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)

#cross tabulation
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)

#checking data with z score normalization
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)

wbcd_train <- wbcd_z[1:469,]
wbcd_test <- wbcd_z[470:569,]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21 )

CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)




