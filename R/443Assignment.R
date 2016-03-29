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

out <- list(output,pqValues,forecast1,forecast2,forecast5,forecast10,model)
return(out)
}

out <- AICmatrix(110,0,3,0,3)
matrices <- out[[1]]
pqVals <- unlist(out[2])
f1 <- out[[3]]
f2 <- out[[4]]
f5 <- out[[5]]
f10 <- out[[6]]
models <- out[[7]]
names(matrices) <- pqVals

freq <- table(pqVals == "11")
proportion <- freq[[2]]/(freq[[1]] + freq[[2]])

s1 <- list(); s2 <- list(); s5 <- list(); s10 <- list()

for (i in 1:30)
{
	s1[i] <- (models[[i]][101] - f1[[i]])^2
	s2[[i]] <- (models[[i]][101:102] - f2[[i]])^2
	s5[[i]] <- (models[[i]][101:105] - f5[[i]])^2
	s10[[i]] <- (models[[i]][101:110] - f10[[i]])^2
}

sum1 <- 0; sum2 <- c(0,0); sum5 <- c(0,0,0,0,0); sum10 <- c(0,0,0,0,0,0,0,0,0,0)

for (i in 1:30)
{
	sum1 <- sum1 + s1[[i]]
	sum2 <- sum2 + s2[[i]]
	sum5 <- sum5 + s5[[i]]
	sum10 <- sum10 + s10[[i]]
}

mse1 <- sum1/30; mse2 <- sum2/30; mse5 <- sum5/30; mse10 <- sum10/30

