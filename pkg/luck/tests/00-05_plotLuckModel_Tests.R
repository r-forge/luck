# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#                                                 
# S4 implementation of generalized iLUCK models   
# -- Tests for plot method for Class LuckModel --             
#                                                 
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

library(luck)
library(testthat)

#source(file="00-01_LuckModelData.r")
#source(file="00-02_LuckModel.r")
bsp1 <- LuckModel(n0 = c(1,10), y0 = c(-5,5))
n0(bsp1) <- c(1,2)
y0(bsp1) <- c(-1,1)
set.seed(42)
data(bsp1) <- list(tau=sum(rnorm(5)), n=5)

plot(bsp1)
plot(bsp1, control = controlList(posterior = TRUE))
par(mfrow = c(1,2))
plot(bsp1, ylim = c(-1,1), cex.main = 1.5)
plot(bsp1, control = controlList(posterior = TRUE), ylim = c(-1,1))
par(mfrow = c(1,1))

bsp2 <- LuckModel(n0 = c(1,10), y0 = c(-5, 10, 5, 15)) 
plot(bsp2) 
plot(bsp2, ylim=c(-6,16), control=controlList(borderCol=c(1,2), polygonCol=c("blue", "grey"), density=c(10,NA), angle=c(25,95))) 
expect_error(plot(bsp2, control = controlList(posterior = TRUE))) # error: no data
par(mfrow = c(1,2))
plot(bsp2, ylim=c(-6,16), control=controlList(plotdim=1))
plot(bsp2, ylim=c(-6,16), control=controlList(plotdim=2))
par(mfrow = c(1,1))

bsp5 <- LuckModel(n0 = c(1,10), y0 = c(-5,5), data = matrix(c(sum(rnorm(10)), 10), ncol = 2, byrow=FALSE))
plot(bsp5, control = controlList(posterior = TRUE, polygonCol = "grey"))

bsp6 <- LuckModel(n0 = c(1,10), y0 = c(-5,5,10,15), data = matrix(rep(c(20, 10), 2), ncol=2, byrow=TRUE))
n0(bsp6)[1] <- 2
y0(bsp6)[2,1] <- -5
data(bsp6) <- list(tau=c(25,26), n=c(14,14))
plot(bsp6, xlim = c(1,11), ylim = c(-6,16), cex.main = 0.8, #
     control = controlList(annotate=FALSE, numbers = TRUE, borderCol=c(1,2),
                           polygonCol=c(1,2), density=5, angle=c(45,135)))
plot(bsp6, control = controlList(posterior = TRUE, numbers = TRUE), #
     ylim = c(-5,15), cex.main = 0.8, xlim = c(14,26), cex = 0.7)
plot(bsp6, control = controlList(posterior = TRUE, numbers = TRUE, plotdim=2), #
     ylim = c(-5,15), cex.main = 0.8, xlim = c(14,26), cex = 0.7)
bsp8 <- LuckModel(n = c(1,10), y = 1:10)
bsp8
plot(bsp8, ylim=c(0,11)) 
plot(bsp6, control = controlList(plotdim = 2, posterior = TRUE, rectangle = TRUE, #
                                 rectCol = 2, rectLty = "2244", numbers = TRUE, #
                                 numCol = 4), #
     xlim = c(15,25), ylim = c(-5,15), cex.main = 0.8, cex = 0.5)

#