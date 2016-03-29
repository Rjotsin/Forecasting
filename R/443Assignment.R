# 443 Assignment

newARMA <- function(n,ar,ma)
{
	sim.arma <- arima.sim(n=n, list(ar=ar,ma=ma), sd = sqrt(1))
	return(sim.arma)
}

AICmatrix <- function(n,pstart,pfinish,qstart,qfinish)
{

output <- list(); model <- list(); mat <- list(); pqValues <- list(); 
forecast1 <- list(); forecast2 <- list(); forecast5 <- list(); forecast10 <- list()

for (p in 1:30)
{
	# Generating simulated data from model 
	model[[p]] <- newARMA(n,c(0.5),c(1))
	mat[[p]] <- matrix(nrow = pfinish + 1,ncol = qfinish + 1)
	dimnames(mat[[p]]) <- list(c(pstart:pfinish),c(qstart:qfinish))

		for (i in pstart:pfinish)
		{
		  for (l in qstart:qfinish)
		  {
		  	# We pretend that we don't know p and q so we look at the matrix.
		    k <- arima(model[[p]][1:(n-10)], order=c(i,0,l), include.mean = F,method="ML") 
		    mat[[p]][i+1,l+1] <- k$aic
		  }
		}

	# Getting output of min aic values and the corresponding matrices
	output[[p]] <- mat[[p]]
	pqValues[p] <- paste(as.character(c(which(mat[[p]] == min(mat[[p]]), arr.ind = TRUE)) - 1),collapse="")

	# Predicting model[[p]] for 1,2,5,10 steps ahead
	forecast1[p] <- predict(arima(model[[p]][1:(n-10)], order=c(1,0,1)),n.ahead=1)
	forecast2[p] <- predict(arima(model[[p]][1:(n-10)], order=c(1,0,1)),n.ahead=2)
	forecast5[p] <- predict(arima(model[[p]][1:(n-10)], order=c(1,0,1)),n.ahead=5)
	forecast10[p] <- predict(arima(model[[p]][1:(n-10)], order=c(1,0,1)),n.ahead=10)
}

out <- list(output,pqValues,forecast1,forecast2,forecast5,forecast10)
return(out)
}

out <- AICmatrix(110,0,3,0,3)
matrices <- out[[1]]
pqVals <- unlist(out[2])
f1 <- out[[3]]
f2 <- out[[4]]
f5 <- out[[5]]
f10 <- out[[6]]
names(matrices) <- pqVals

freq <- table(pqVals == "11")
proportion <- freq[[2]]/(freq[[1]] + freq[[2]])

forecast1 <- predict(arima(sim.arma12[1:100], order=c(1,0,1)),n.ahead=1)
forecast2 <- predict(arima(sim.arma12[1:100], order=c(1,0,1)),n.ahead=2)
forecast3 <- predict(arima(sim.arma12[1:100], order=c(1,0,1)),n.ahead=5)
forecast4 <- predict(arima(sim.arma12[1:100], order=c(1,0,1)),n.ahead=10)

# sim.ar2 <- arima.sim(n=500,model=list(ar=c(0.3,0.1)),sd = sqrt(4))
# sim.ma2 <- arima.sim(n=500,model=list(ma=c(2,1)),sd = sqrt(4))
# sim.arma11 <- arima.sim(n=500,model=list(ar=-0.8,ma=2),sd = sqrt(4))

# sim.arma <- arima.sim(n=110, list(ar=c(-.06),ma=c(0.6,-0.3)), sd = sqrt(1))