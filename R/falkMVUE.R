"falkMVUE" <-
function (x, omega) 
{
    n <- length(x)
    v1 <- 1:n * NA
    v2 <- v1
    if (omega < x[n]) {
        cat("omega must be greater than max(x)!")
    }
    else {
        est <- activeSetLogCon(x, print = FALSE)
        q <- 1:n * NA
        for (c in 1:n) {
            q[c] <- quantilesLogConDens(c/n, x, est$phi, est$Fhat)
        }
        for (k in 2:(n - 1)) {
            j <- 2:k
            temp <- (omega - q[n - j + 1])/(omega - q[n - k])
            v1[k] <- sum(log(temp))/k
            temp <- (omega - x[n - j + 1])/(omega - x[n - k])
            v2[k] <- sum(log(temp))/k
        }
        res <- cbind(k = 1:n, logcon = v1, order = v2)
        return(res)
    }
}
