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

# MEDV histogram
par(mfrow=c(1, 2))
hist(housing_subset$MEDV,
     col="blue",
     border="black",
     xlab="Median house value",
     ylab="counts",
     main="Histogram of median house values")
box(which="plot", lty="solid", col="black")

housing_subset$MEDV_z <- ((housing_subset$MEDV-mean(housing_subset$MEDV))/sd(housing_subset$MEDV))

hist(housing_subset$MEDV_z,
     breaks=10,
     col="blue",
     border="black",
     xlim = c(-6, 6),
     xlab="Z-standardized proportion of median house value",
     ylab="counts",
     main="Histogram of z-standardized proportion of median house value")
box(which="plot", lty="solid", col="black")

housing_subset[housing_subset$MEDV_z > 3, ]

nrow(housing_subset[housing_subset$MEDV_z > 3, ])

summary(housing_subset$MEDV_z)