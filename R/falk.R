`falk` <-
function (x) 
{
    n <- length(x)
    est <- activeSetLogCon(x, print = FALSE)
    q <- 1:n * NA
    for (c in 1:n) {
        q[c] <- quantilesLogConDens(c/n, x, est$phi, est$Fhat)
    }
    v1 <- 1:n * NA
    v2 <- v1
    for (k in 3:(n - 1)) {
        j <- 2:k
        temp <- (q[n] - q[n - j + 1])/(q[n] - q[n - k])
        v1[k] <- sum(log(temp))/(k - 1)
        temp <- (x[n] - x[n - j + 1])/(x[n] - x[n - k])
        v2[k] <- sum(log(temp))/(k - 1)
    }
    res <- cbind(k = 1:n, logcon = v1, order = v2)
    return(res)
}
