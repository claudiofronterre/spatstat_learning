rm(list=ls())
library(PrevMap)
rb <- read.csv("LiberiaRemoData.csv")
Liberia.bndrs <- read.csv("Liberia_bndrs.csv")

glm.fit <- glm(cbind(npos,ntest-npos)~I(utm_x/1000)+I(utm_y/1000),data=rb,family=binomial)

summary(glm.fit)

Liberia.grid.pred <- read.csv("Liberia_grid_pred.csv")

# Estiamtes of the regression coefficients
beta.hat <- coef(glm.fit)

# Matrix of the explanatory variables at prediction locations
D.pred <- as.matrix(cbind(1,Liberia.grid.pred/1000))

# Linear predictor at the prediction locations
eta.hat <- D.pred%*%beta.hat

# Covariance matrix of the regression coefficients
beta.covar <- vcov(glm.fit)

# Standard errors of the linear predictor
se.eta.hat <- sqrt(diag(D.pred%*%beta.covar%*%t(D.pred)))

# Exceedance probabilities of 20% threshold
exceed.20 <- 1-pnorm(-log(4),mean=eta.hat,sd=se.eta.hat)

# Plot of the exceedance probabilities

plot(rasterFromXYZ(cbind(Liberia.grid.pred/1000,exceed.20)))
lines(Liberia.bndrs/1000,type="l")     


check.spat <- spat.corr.diagnostic(npos~I(utm_x/1000)+I(utm_y/1000),
                                   units.m = ~ntest,coords = ~I(utm_x/1000)+I(utm_y/1000),
                                   data=rb,likelihood = "Binomial",
                                   uvec=seq(20,300,length=15),n.sim=1000)
