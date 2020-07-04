#feature extraction -PCA + Factor Analysis

#dataset

library(readxl)
df = read_excel('.../reqData.xls')

#Removing Response variable

df <- df[2:15]

#spliting data for testing

set.seed(123)
split <- sample.split(df$ln_Price,SplitRatio = 0.70)
train_set<- subset(df,split==T)
test_set<- subset(df,split==F)
View(train_set)

#initialise and fit PCA

ca_train <- train_set[1:13]
pca = prcomp(pca_train,scale. = T)

#generate loadings 

loadings <- as.data.frame(pca$x)
View(loadings)

#generate loading matrix

Matrix <- pca$rotation

#Variance explained by each Principal Component

std_dev <- pca$sdev
pr_comp_var <- std_dev^2
pr_comp_var

#Ratio of Variance explained by each component

prop_var_ex <- pr_comp_var/sum(pr_comp_var)

plot(cumsum(prop_var_ex), xlab = "Principal Component",ylab = "Proportion of Variance Explained",type = "b")

#Concatenate Dependent variable and Principal Components

pca_train2 <- cbind(loadings,Y_train)
View(pca_train2)

#creating dataset

loadings2 <- loadings[1:7]
pca_train2 <- cbind(loadings2,Y_train)
View(pca_train2)

#Transform features of Test Dataset into Principal Components

pca_test <- test_set[1:13]
pca_test2 <- predict(pca, newdata = pca_test)

pca_test2 <- as.data.frame(pca_test2)
View(pca_test2)
pca_test3 <- pca_test2[1:7]
Y_test <- test_set$Xp
pca_test4 <- cbind(pca_test3,Y_test)


#factor
#Removing the dependent and categorical variables

Factor1 = subset(Bos_train2,select = c(1,2,3,5,6,7,8,9,10,11,12,13))

#creating corr matrix

corrm<- cor(Factor1)


#cumulative eigenvalue, percentage variance and cumulative percentage variance

eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       ,pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))

write.csv(eigen_values,"../factor1_2.csv")

#reducing variables

require(psych)
FA<-fa(r=corrm, 4, rotate="varimax", fm="ml")  
FA_SORT<-fa.sort(FA)
FA_SORT$loadings

#grouping variables

load1 = FA_SORT$loadings
write.csv(load1,".../factor1_3.csv")