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

hist(z_score_CRIM,
     breaks=10,
     col="blue",
     border="black",
     xlim = c(-6, 6),
     ylim = c(0, 100),
     xlab="Z-standardized per capita crime rate",
     ylab="count",
     main="Histogram of z-standardized per capita crime rate")
axis(side=1)
box(which="plot", lty="solid", col="black")

plot(housing_subset$MEDV, housing_subset$CRIM,
     xlab = "Median house value",
     ylab = "Per capita crime rate",
     main = "Scatterplot of crime rate against median house value",
     type = "p",
     pch = 16,
     col = "blue")

fit <- lm(housing_subset$MEDV ~ housing_subset$CRIM)
summary(fit)

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

fit <- lm(housing_subset$MEDV ~ housing_subset$ZN)
summary(fit)

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

plot(housing_subset$MEDV, housing_subset$INDUS,
     xlab = "Median house value",
     ylab = "Proportion of non-retail business acres",
     main = "Scatterplot of INDUS against median house value",
     type = "p",
     pch = 16,
     col = "blue")

fit <- lm(housing_subset$MEDV ~ housing_subset$INDUS)
summary(fit)

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
housing_subset$CHAS_bool <- sapply(housing_subset$CHAS, as.logical)

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

table(housing_subset$MEDV_bin, housing_subset$CHAS_bool, dnn=c("Median house value bin", "Charles river"))

# NOX histogram
par(mfrow=c(1, 2))
hist(housing_subset$NOX,
     col="blue",
     border="black",
     xlab="Nitric oxide concentration ppm",
     ylab="counts",
     main="Histogram of nitric oxide concentration")
box(which="plot", lty="solid", col="black")

z_score_NOX <- ((housing_subset$NOX-mean(housing_subset$NOX))/sd(housing_subset$NOX))

hist(z_score_NOX,
     breaks=10,
     col="blue",
     border="black",
     xlim = c(-6, 6),
     xlab="Z-standardized nitric oxide concentration",
     ylab="counts",
     main="Histogram of z-standardized nitric oxide concentration")
box(which="plot", lty="solid", col="black")

plot(housing_subset$MEDV, housing_subset$NOX,
     xlab = "Median house value",
     ylab = "Nitric oxide concentration",
     main = "Scatterplot of NOX against median house value",
     type = "p",
     pch = 16,
     col = "blue")

fit <- lm(housing_subset$MEDV ~ housing_subset$NOX)
summary(fit)

summary(housing_subset$NOX)
find_mode(housing_subset$NOX)

# RM histogram
par(mfrow=c(1, 2))
hist(housing_subset$RM,
     col="blue",
     border="black",
     xlab="Avg number of rooms",
     ylab="counts",
     main="Histogram of avg number of rooms")
box(which="plot", lty="solid", col="black")

z_score_RM <- ((housing_subset$RM-mean(housing_subset$RM))/sd(housing_subset$RM))

hist(z_score_RM,
     breaks=10,
     col="blue",
     border="black",
     xlim = c(-6, 6),
     xlab="Z-standardized avg number of rooms",
     ylab="counts",
     main="Histogram of z-standardized avg number of rooms")
box(which="plot", lty="solid", col="black")

plot(housing_subset$MEDV, housing_subset$RM,
     xlab = "Median house value",
     ylab = "Avg number of rooms",
     main = "Scatterplot of RM against median house value",
     type = "p",
     pch = 16,
     col = "blue")

fit <- lm(housing_subset$MEDV ~ housing_subset$RM)
summary(fit)

summary(housing_subset$RM)
find_mode(housing_subset$RM)

# AGE histogram
par(mfrow=c(1, 2))
hist(housing_subset$AGE,
     col="blue",
     border="black",
     xlab="Proportion of old housing",
     ylab="counts",
     main="Histogram of proportion of old housing")
box(which="plot", lty="solid", col="black")

z_score_AGE <- ((housing_subset$AGE-mean(housing_subset$AGE))/sd(housing_subset$AGE))

hist(z_score_AGE,
     breaks=10,
     col="blue",
     border="black",
     xlim = c(-6, 6),
     xlab="Z-standardized proportion of old housing",
     ylab="counts",
     main="Histogram of z-standardized proportion of old housing")
box(which="plot", lty="solid", col="black")

plot(housing_subset$MEDV, housing_subset$AGE,
     xlab = "Median house value",
     ylab = "Proportion of old housing",
     main = "Scatterplot of AGE against median house value",
     type = "p",
     pch = 16,
     col = "blue")

fit <- lm(housing_subset$MEDV ~ housing_subset$AGE)
summary(fit)

summary(housing_subset$AGE)
find_mode(housing_subset$AGE)

# DIS histogram
par(mfrow=c(1, 2))
hist(housing_subset$DIS,
     col="blue",
     border="black",
     xlab="Distance from empl. centres",
     ylab="counts",
     main="Histogram of distance from empl. centres")
box(which="plot", lty="solid", col="black")

z_score_DIS <- ((housing_subset$DIS-mean(housing_subset$DIS))/sd(housing_subset$DIS))

hist(z_score_DIS,
     breaks=10,
     col="blue",
     border="black",
     xlim = c(-6, 6),
     xlab="Z-standardized distance from empl. centres",
     ylab="counts",
     main="Histogram of distance from empl. centres")
box(which="plot", lty="solid", col="black")

plot(housing_subset$MEDV, housing_subset$DIS,
     xlab = "Median house value",
     ylab = "Distance from empl. centres",
     main = "Scatterplot of DIS against median house value",
     type = "p",
     pch = 16,
     col = "blue")

fit <- lm(housing_subset$MEDV ~ housing_subset$DIS)
summary(fit)

summary(housing_subset$DIS)
find_mode(housing_subset$DIS)

# RAD histogram
par(mfrow=c(1, 2))
hist(housing_subset$RAD,
     col="blue",
     border="black",
     xlab="Highway accessibility",
     ylab="counts",
     main="Histogram of highway accesibility")
box(which="plot", lty="solid", col="black")

z_score_RAD <- ((housing_subset$RAD-mean(housing_subset$RAD))/sd(housing_subset$RAD))

hist(z_score_RAD,
     breaks=10,
     col="blue",
     border="black",
     xlim = c(-6, 6),
     xlab="Z-standardized highway accessibility",
     ylab="counts",
     main="Histogram of Z-standardized highway accesibility")
box(which="plot", lty="solid", col="black")

plot(housing_subset$MEDV, housing_subset$RAD,
     xlab = "Median house value",
     ylab = "Highway accessibility",
     main = "Scatterplot of RAD against median house value",
     type = "p",
     pch = 16,
     col = "blue")

fit <- lm(housing_subset$MEDV ~ housing_subset$RAD)
summary(fit)

summary(housing_subset$RAD)
find_mode(housing_subset$RAD)

# TAX histogram
par(mfrow=c(1, 2))
hist(housing_subset$TAX,
     col="blue",
     border="black",
     xlab="Property tax per $10k",
     ylab="counts",
     main="Histogram of property tax per $10k")
box(which="plot", lty="solid", col="black")

z_score_TAX <- ((housing_subset$TAX-mean(housing_subset$TAX))/sd(housing_subset$TAX))

hist(z_score_TAX,
     breaks=10,
     col="blue",
     border="black",
     xlim = c(-6, 6),
     xlab="Z-standardized property tax per $10k",
     ylab="counts",
     main="Histogram of Z-standardized property tax per $10k")
box(which="plot", lty="solid", col="black")

plot(housing_subset$MEDV, housing_subset$TAX,
     xlab = "Median house value",
     ylab = "Property tax per $10k",
     main = "Scatterplot of TAX against median house value",
     type = "p",
     pch = 16,
     col = "blue")

fit <- lm(housing_subset$MEDV ~ housing_subset$TAX)
summary(fit)

summary(housing_subset$TAX)
find_mode(housing_subset$TAX)

# PTRATIO histogram
par(mfrow=c(1, 2))
hist(housing_subset$PTRATIO,
     col="blue",
     border="black",
     xlab="Pupil-teacher ratio",
     ylab="counts",
     main="Histogram of pupil-teacher ratio")
box(which="plot", lty="solid", col="black")

z_score_PTRATIO <- ((housing_subset$PTRATIO-mean(housing_subset$PTRATIO))/sd(housing_subset$PTRATIO))

hist(z_score_PTRATIO,
     breaks=10,
     col="blue",
     border="black",
     xlim = c(-6, 6),
     xlab="Z-standardized pupil-teacher ratio",
     ylab="counts",
     main="Histogram of z-standardized pupil-teacher ratio")
box(which="plot", lty="solid", col="black")

plot(housing_subset$MEDV, housing_subset$PTRATIO,
     xlab = "Median house value",
     ylab = "Pupil-teach ratio",
     main = "Scatterplot of PTRATIO against median house value",
     type = "p",
     pch = 16,
     col = "blue")

fit <- lm(housing_subset$MEDV ~ housing_subset$PTRATIO)
summary(fit)

summary(housing_subset$PTRATIO)
find_mode(housing_subset$PTRATIO)

# B histogram
par(mfrow=c(1, 2))
hist(housing_subset$B,
     col="blue",
     border="black",
     xlab="Proportion of blacks",
     ylab="counts",
     main="Histogram of proportion of blacks")
box(which="plot", lty="solid", col="black")

z_score_B <- ((housing_subset$B-mean(housing_subset$B))/sd(housing_subset$B))

hist(z_score_B,
     breaks=10,
     col="blue",
     border="black",
     xlim = c(-6, 6),
     xlab="Z-standardized proportion of blacks",
     ylab="counts",
     main="Histogram of z-standardized proportion of blacks")
box(which="plot", lty="solid", col="black")

plot(housing_subset$MEDV, housing_subset$B,
     xlab = "Median house value",
     ylab = "Proportion of blacks",
     main = "Scatterplot of B against median house value",
     type = "p",
     pch = 16,
     col = "blue")

fit <- lm(housing_subset$MEDV ~ housing_subset$B)
summary(fit)

summary(housing_subset$B)
find_mode(housing_subset$B)

# LSTAT histogram
par(mfrow=c(1, 2))
hist(housing_subset$LSTAT,
     col="blue",
     border="black",
     xlab="Proportion of lower status",
     ylab="counts",
     main="Histogram of proportion of lower status")
box(which="plot", lty="solid", col="black")

z_score_LSTAT <- ((housing_subset$LSTAT-mean(housing_subset$LSTAT))/sd(housing_subset$LSTAT))

hist(z_score_LSTAT,
     breaks=10,
     col="blue",
     border="black",
     xlim = c(-6, 6),
     xlab="Z-standardized proportion of lower status",
     ylab="counts",
     main="Histogram of z-standardized proportion of lower status")
box(which="plot", lty="solid", col="black")

plot(housing_subset$MEDV, housing_subset$LSTAT,
     xlab = "Median house value",
     ylab = "Proportion of low status",
     main = "Scatterplot of LSTAT against median house value",
     type = "p",
     pch = 16,
     col = "blue")

fit <- lm(housing_subset$MEDV ~ housing_subset$LSTAT)
summary(fit)

summary(housing_subset$LSTAT)
find_mode(housing_subset$LSTAT)

# MEDV histogram
par(mfrow=c(1, 2))
hist(housing_subset$MEDV,
     col="blue",
     border="black",
     xlab="Median house value",
     ylab="counts",
     main="Histogram of median house values")
box(which="plot", lty="solid", col="black")

z_score_MEDV <- ((housing_subset$MEDV-mean(housing_subset$MEDV))/sd(housing_subset$MEDV))

hist(z_score_MEDV,
     breaks=10,
     col="blue",
     border="black",
     xlim = c(-6, 6),
     xlab="Z-standardized proportion of median house value",
     ylab="counts",
     main="Histogram of z-standardized proportion of median house value")
box(which="plot", lty="solid", col="black")

plot(housing_subset$MEDV, housing_subset$MEDV,
     xlab = "Median house value",
     ylab = "Median house value",
     main = "Scatterplot of MEDV against median house value",
     type = "p",
     pch = 16,
     col = "blue")

fit <- lm(housing_subset$MEDV ~ housing_subset$MEDV)
summary(fit)

summary(housing_subset$MEDV)
find_mode(housing_subset$MEDV)

housing_subset()

