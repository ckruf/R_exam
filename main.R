# read file
housing <- read.csv(
  file="./housing.csv",
  sep = ";",
  header = FALSE,
  col.names = c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")
)

# get random subset
set.seed(0)
index <- sample(1:nrow(housing), 480)
housing_subset <- housing[index, ]
summary(housing_subset)

# check for NA values
na_count <- sapply(housing_subset, function(value) sum(length(which(is.na(value)))))
na_count <- data.frame(na_count)
na_count

# functino for finding mode
find_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# CRIME histogram
par(mfrow=c(1, 2))
hist(housing_subset$CRIM,
     col="blue",
     border="black",
     xlab="Per capita crime rate",
     ylab="counts",
     main="Histogram of per capita crime rate")
box(which="plot", lty="solid", col="black")

z_score_CRIM <- ((housing_subset$CRIM-mean(housing_subset$CRIM))/sd(housing_subset$CRIM))

hist(zscore_CRIM,
     breaks=10,
     col="blue",
     border="black",
     xlim = c(-6, 6),
     xlab="Z-standardized per capita crime rate",
     ylab="count",
     main="Histogram of z-standardized per capita crime rate")
box(which="plot", lty="solid", col="black")

plot(housing_subset$MEDV, housing_subset$CRIM,
     xlab = "Median house value",
     ylab = "Per capita crime rate",
     main = "Scatterplot of crime rate against median house value",
     type = "p",
     pch = 16,
     col = "blue")

summary(housing_subset$CRIM)
find_mode(housing_subset$CRIM)


# ZN histogram
par(mfrow=c(1, 2))
hist(housing_subset$ZN,
     col="blue",
     border="black",
     xlab="Proportion of residential land zoned for lots over 25k sq ft",
     ylab="counts",
     main="Histogram of proportion of residential land zoning")
box(which="plot", lty="solid", col="black")

z_score_ZN <- ((housing_subset$ZN-mean(housing_subset$ZN))/sd(housing_subset$ZN))

hist(z_score_ZN,
     breaks=10,
     col="blue",
     border="black",
     xlim = c(-6, 6),
     xlab="Z-standardized proportion of residential land zoning",
     ylab="count",
     main="Histogram of z-standardized proportion of residential land zoning")
box(which="plot", lty="solid", col="black")

plot(housing_subset$MEDV, housing_subset$ZN,
     xlab = "Median house value",
     ylab = "Proportion of residential land zoned for lots over 25k sq ft",
     main = "Scatterplot of ZN against median house value",
     type = "p",
     pch = 16,
     col = "blue")

summary(housing_subset$ZN)
find_mode(housing_subset$ZN)

# INDUS histogram
par(mfrow=c(1, 2))
hist(housing_subset$INDUS,
     col="blue",
     border="black",
     xlab="Proportion of non-retail business acres",
     ylab="counts",
     main="Histogram of proportion of noon-retail business acres")
box(which="plot", lty="solid", col="black")

z_score_INDUS <- ((housing_subset$INDUS-mean(housing_subset$INDUS))/sd(housing_subset$INDUS))

hist(z_score_INDUS,
     breaks=10,
     col="blue",
     border="black",
     xlim = c(-6, 6),
     xlab="Z-standardized proportion of non-retail business acres",
     ylab="count",
     main="Histogram of z-standardized proportion of non-retail business acres")
box(which="plot", lty="solid", col="black")

plot(housing_subset$MEDV, housing_subset$ZN,
     xlab = "Median house value",
     ylab = "Proportion of non-retail business acres",
     main = "Scatterplot of INDUS against median house value",
     type = "p",
     pch = 16,
     col = "blue")

summary(housing_subset$INDUS)
find_mode(housing_subset$INDUS)


# River data
summary(housing_subset$CHAS)
river_count <-sum(housing_subset$CHAS)
river_count

# bin house values - equal binning

xdata <- housing_subset$MEDV
n <- length(xdata)
nbins <- 4
whichbin <- c(rep(0, n))
range_xdata <- max(xdata) - min(xdata) + 1
binwidth <- range_xdata/nbins
for (i in 1:nbins) {
  for (j in 1:n) {
    if((i-1)*binwidth < xdata[j] &&
       xdata[j] <= (i)*binwidth)
      whichbin[j] <- i
  }
}

housing_subset$MEDV_bin <- whichbin

# overlayed bar chart
summary_river <- summary(as.logical(housing_subset$CHAS))
library(ggplot2)
ggplot() +
  geom_bar(data = housing_subset,
           aes(x = factor(housing_subset$MEDV_bin),
               fill = factor(housing_subset$CHAS)),
           position = "stack") +
  scale_x_discrete("Median house value") +
  scale_y_continuous("Counts") +
  guides(fill=guide_legend(title="River")) +
  scale_fill_manual(values=c("blue", "red"))

# scaled overlayed bar chart
ggplot() +
  geom_bar(data = housing_subset,
           aes(x = factor(housing_subset$MEDV_bin),
               fill = factor(housing_subset$CHAS)),
           position = "fill") +
  scale_x_discrete("Median house value") +
  scale_y_continuous("Counts") +
  guides(fill=guide_legend(title="River")) +
  scale_fill_manual(values=c("blue", "red"))


