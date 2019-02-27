## Churn analysis for frequent order churn (customers who ordered every 2 months)

rm(list = ls(all.names = TRUE))
setwd('C:\\Users\\Vishal Vatnani\\Documents\\HC_churned_customer_R')
#pkgs <- c("factoextra",  "NbClust")
#install.packages(pkgs)
library(data.table)
library(factoextra)
library(NbClust)
#install.packages("VIM")
library(VIM)
#install.packages("corrplot")
library(corrplot)
library(cluster)
library(fpc)

# all people who haven't ordered for 60 days or more
churn_60_days = read.csv("Churned customers summary - Frequent order churn.csv")

# plot any missing values
aggr(churn_60_days)

# all people who haven't ordered for 60 days or more
# churn_60_days = read.csv("Churned customers summary - 60 days churned.csv")

#View(churn_60_days)
names(churn_60_days)[grep("max.*since", names(churn_60_days), ignore.case = TRUE)] <- "days_since_order"
names(churn_60_days)[grep("max.*diff", names(churn_60_days), ignore.case = TRUE)] <- "avg_days_btn_orders"

View(churn_60_days)

## overall summary of churn
summary(churn_60_days)
# summary of churn for Cylinder (This segment still has heavy wefts, and it has been mean 883 days since their last order)
summary(churn_60_days[churn_60_days$MAX.of.sum_cylinder > 0, ])
# summary of churn for HTW (All weft purchasers have low mean poly and cylinder types, and mean 334 days since last order)
summary(churn_60_days[churn_60_days$SUM.of.sum_weft > 0, ])
# summary of churn for Poly (only 22 customers, highest mean quantity + amount, and highest days since last order)
summary(churn_60_days[churn_60_days$SUM.of.poly > 0, ])


## Correlation across each application types
# for Cylinder
M <- cor((churn_60_days[, -1]))
corrplot(M)

row.names(churn_60_days) <- churn_60_days[,1]
# remove the last row "Grand Total"
churn_60_days <- churn_60_days[-grep("Grand Total" ,rownames(churn_60_days)),]

# we see there are outliers
plot(churn_60_days[,c(2,9)])

## boxplot
bp <- boxplot(as.list(churn_60_days[,-1]))
bp
str(bp)
bp$out
# for each variable one-by-one
for(i in 2:length(churn_60_days)) {
  
  boxplot(churn_60_days[,c(1, i)])
  
}

# assign row names
row.names(churn_60_days) <- (churn_60_days[,1])

# remove the last row "Grand Total"
#churn_60_days <- churn_60_days[-grep("Grand Total" ,rownames(churn_60_days)),]

## scale the data
scaled_churn_60_d <- data.frame(scale(churn_60_days[,2:length(churn_60_days)]))
# set row.names
row.names(scaled_churn_60_d) <- (churn_60_days[,1])

## boxplot for each variable in scaled data
for(i in 2:length(scaled_churn_60_d)) {
  
  boxplot(scaled_churn_60_d[,c(1, i)])
  
}

## corrplot to see which variables are correlated
M <- cor(scaled_churn_60_d[, c(1,4,5,6,7,9)])
corrplot(M)
M
# remove the variables with a high correlation
scaled_churn_60_d <- scaled_churn_60_d[!names(scaled_churn_60_d) %in% c("SUM.of.count_orders")]

## Elbow chart: choose the right number of cluster centers
wss <- NULL
for(i in 1:10) {
  
  wss[i] <- kmeans(scaled_churn_60_d[, c(1,6,7)], centers = i)$tot.withinss
  
}
# plot the chart
plot(wss, type = "b")

## heirarchical clustering
d <- dist(scaled_churn_60_d[, c(1,4,5,6,7,9)], method = "euclidean")
fit <- hclust(d, method = "ward.D2")
plot(fit)

## Silhouette method
fviz_nbclust(scaled_churn_60_d[, c(1,4,5,6,7,9)], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
fviz_nbclust(scaled_churn_60_d[, c(1,4,5,6,7,9)], kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")



set.seed(1234)
## run k-means for n = 4
kmm = kmeans(scaled_churn_60_d[, c(1,4,5,6,7,9)], 5, nstart = 50, iter.max = 15)
scaled_churn_60_d$cluster <- kmm$cluster
churn_60_days$cluster <- kmm$cluster

summary(churn_60_days[which(churn_60_days$cluster == 1),])
summary(churn_60_days[which(churn_60_days$cluster == 2),])
summary(churn_60_days[which(churn_60_days$cluster == 3),])
summary(churn_60_days[which(churn_60_days$cluster == 4),])
summary(churn_60_days[which(churn_60_days$cluster == 5),])
summary(churn_60_days[,c(5,6,7)])

plotcluster(scaled_churn_60_d, kmm$cluster)

# linear regression

lmodel <- lm(churn_60_days$SUM.of.sum_amount ~  scaled_churn_60_d$MAX.of.avg_days_difference + scaled_churn_60_d$MAX.of.days_since_last_order)
summary(lmodel)

write.csv(churn_60_days, "60_days_churn.csv")
