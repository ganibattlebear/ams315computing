install.packages('mice')
library(mice)
setwd('/Users/gania/Documents/Project1/Project1_PartA_Data')
getwd();
PartA_one <- read.csv('095768_DV.csv', header=TRUE)
PartA_two <- read.csv('095768_IV.csv', header=TRUE)
Part <- merge(PartA_one, PartA_two, by ='ID')
View(Part)
##testing phase
any(is.na(Part[,3]) == TRUE)
any(is.nan(Part[,3]) == TRUE)
any(is.null(Part[,3]) == TRUE)
toty <- 656
toty
#number of subject IDs that had at least one independent variable value or dependent variable
#value.
for(i in 1:656){
  bool1 <- FALSE
  bool2 <- FALSE
  if (is.na(Part[i,2]) == TRUE) {bool1 <- TRUE}
  if (is.nan(Part[i,2]) == TRUE) {bool1 <- TRUE}
  if (is.null(Part[i,2]) == TRUE) {bool1 <- TRUE}
  if (is.na(Part[i,3]) == TRUE) {bool2 <- TRUE}
  if (is.nan(Part[i,3]) == TRUE) {bool2 <- TRUE}
  if (is.null(Part[i,3]) == TRUE) {bool2 <- TRUE}
  if (bool1 == TRUE && bool2 == TRUE) {toty <- toty - 1}
}
toty
#the number of subject IDs that had an independent
#variable value
total <- 656
for(i in 1:656){
  bool <- FALSE
  if (is.na(Part[i,3]) == TRUE) {bool <- TRUE}
  if (is.nan(Part[i,3]) == TRUE) {bool <- TRUE}
  if (is.null(Part[i,3]) == TRUE) {bool <- TRUE}
  if (bool == TRUE) {total <- total - 1}
}
total
## number of IDs with dependent value
tot <- 656
for(i in 1:656){
  bool <- FALSE
  if (is.na(Part[i,2]) == TRUE) {bool <- TRUE}
  if (is.nan(Part[i,2]) == TRUE) {bool <- TRUE}
  if (is.null(Part[i,2]) == TRUE) {bool <- TRUE}
  if (bool == TRUE) {tot <- tot - 1}
}
tot

##Next we need the number of subject IDs that had both an independent 
#and dependent variable value. We need both column 2 and column 3 
#for each row to have is.na() == FALSE
number <- 0
for(i in 1:656){
  bool1 <- FALSE
  if (is.na(Part[i,2]) == FALSE && is.nan(Part[i,2]) == FALSE
      && is.null(Part[i,2]) == FALSE && is.na(Part[i,3]) == FALSE
      && is.nan(Part[i,3]) == FALSE && is.null(Part[i,3]) == FALSE) {bool1 <- TRUE}
  if (bool1 == TRUE) {number <- number + 1}
}
number
##the number of subject IDs that had at least one independent variable value
#or dependent variable value.
md.pattern(Part)
#get rid of both NA
PartA_imp <- Part[!is.na(Part$IV)==TRUE|!is.na(Part$DV)==TRUE,]
imp <- mice(PartA_imp, method = "midastouch", printFlag = FALSE)
PartA_complete <- complete(imp)
md.pattern(PartA_complete)

M <- lm(DV ~ IV, data=PartA_complete)
summary(M)
library(knitr)
kable(anova(M), caption='ANOVA Table')
plot(PartA_complete$DV ~ PartA_complete$IV, main='Scatter : DV ~ IV', xlab='IV', ylab='DV', pch=20)
abline(M, col='red', lty=3, lwd=2)
legend('topleft', legend='Estimated Regression Line', lty=3, lwd=2, col='red')
confint(M, level = 0.95)
