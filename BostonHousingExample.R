rm(list=ls())

library(mlbench)

data(BostonHousing2)
# ?BostonHousing2
head(BostonHousing2)
str(BostonHousing2)
# need to change chas to numeric
BostonHousing2$chas <- as.numeric(BostonHousing2$chas=="1")
DATA <- BostonHousing2[,c("cmedv","crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat")]
# exploratory
library(ggplot2)
# plotmatrix(BostonHousing2)
summary(glm(cmedv~.,data=DATA))
qplot(x=lstat,y=cmedv,data=DATA)
qplot(x=rm,y=cmedv,data=DATA)
qplot(x=crim,y=cmedv,data=DATA)
qplot(x=zn,y=cmedv,data=DATA)
qplot(x=indus,y=cmedv,data=DATA)
qplot(x=age,y=cmedv,data=DATA)
qplot(x=dis,y=cmedv,data=DATA)
qplot(x=rad,y=cmedv,data=DATA)
qplot(x=tax,y=cmedv,data=DATA)
qplot(x=ptratio,y=cmedv,data=DATA)
qplot(x=b,y=cmedv,data=DATA)
qplot(cmedv, data=DATA, binwidth=2, xlab = "Median home values", ylab = "Count")



# SuperLearner
library(SuperLearner)
SL.gam.3 <- function(..., deg.gam = 3){
	SL.gam(..., deg.gam = deg.gam)
}
SL.gam.4 <- function(..., deg.gam = 4){
	SL.gam(..., deg.gam = deg.gam)
}
SL.gam.5 <- function(..., deg.gam = 5){
	SL.gam(..., deg.gam = deg.gam)
}
create.SL.glmnet <- function(alpha = c(0.25, 0.50, 0.75)) {
  for(mm in seq(length(alpha))){
    eval(parse(text = paste('SL.glmnet.', alpha[mm], '<- function(..., alpha = ', alpha[mm], ') SL.glmnet(..., alpha = alpha)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.glmnet(alpha = c(0.2, 0.4, 0.6, 0.8, 1.0))

SL.gbm.1 <- function(..., interaction.depth = 1) SL.gbm(..., interaction.depth = interaction.depth)

create.SL.randomForest <- function(tune = list(mtry = c(1, 5, 10), nodesize = c(1, 5, 10))) {
  tuneGrid <- expand.grid(tune, stringsAsFactors = FALSE)
  for(mm in seq(nrow(tuneGrid))) { 
    eval(parse(file = "", text = paste("SL.randomForest.", mm, "<- function(..., mtry = ", tuneGrid[mm, 1], ", nodesize = ", tuneGrid[mm, 2], ") SL.randomForest(..., mtry = mtry, nodesize = nodesize)", sep = "")), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.randomForest(list(mtry = c(2, 5, 7, 12), nodesize = c(5, 10, 30)))

SL.library <- c("SL.gam", "SL.gam.3", "SL.gam.4", "SL.gam.5", "SL.gbm.1", "SL.gbm", "SL.glm", "SL.glmnet.0.2", "SL.glmnet.0.4", "SL.glmnet.0.6", "SL.glmnet.0.8", "SL.glmnet.1", "SL.polymars", paste("SL.randomForest.", 1:12, sep = ''), "SL.ridge", "SL.svm", "SL.bayesglm", "SL.DSA", "SL.step", "SL.step.interaction", "SL.bart")

fitSL <- SuperLearner(Y = log(DATA$cmedv), X = DATA[, -1], verbose = TRUE, SL.library = SL.library)

# important output
fitSL
fitSL$coef
fitSL$cv.risk

# to find risk of SuperLearner
fitSL.CV <- CV.SuperLearner(Y=log(DATA$cmedv), X=DATA[, -1], outside.V=10, inside.V=10, verbose=TRUE, SL.library=SL.library)

mean((log(DATA$cmedv)- fitSL.CV$pred.SL)^2)
mean((log(DATA$cmedv) - fitSL.CV$pred.discreteSL)^2)
apply(fitSL.CV$pred.library, 2, function(x) mean((log(DATA$cmedv) - x)^2))

cbind(c(0,0,fitSL$coef), c(mean((log(DATA$cmedv)- fitSL.CV$pred.SL)^2), mean((log(DATA$cmedv) - fitSL.CV$pred.discreteSL)^2), apply(fitSL.CV$pred.library, 2, function(x) mean((log(DATA$cmedv) - x)^2))))

library(Hmisc)
latex(format.df(cbind(c(NA,NA,fitSL$coef), c(mean((log(DATA$cmedv)- fitSL.CV$pred.SL)^2), mean((log(DATA$cmedv) - fitSL.CV$pred.discreteSL)^2), apply(fitSL.CV$pred.library, 2, function(x) mean((log(DATA$cmedv) - x)^2)))), cdec=c(3,4)), file="", booktabs=TRUE, rowname=c("SuperLearner", "discreteSL", "gam","gam(3)","gam(4)","gam(5)","gbm(1)","gbm(2)","glm","glmnet(1)","glmnet(.25)","glmnet(.50)","glmnet(.75)","polymars","randomForest","ridge","svm","bayesglm","DSA","step","step.interaction", "bart"), colheads=c("coef", "MSE"), title="method", caption="The weights for each algorithm in the Super Learner and the honest 10-fold cross validated mean squared errors for the Super Learner, the discrete Super Learner and each algorithm")