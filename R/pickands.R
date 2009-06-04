`pickands` <-
function (x) 
{
    n <- length(x)
    res <- NA
    est <- activeSetLogCon(x, print = FALSE)
    q <- 1:n * NA
    for (c in 1:n) {
        q[c] <- quantilesLogConDens(c/n, x, est$phi, est$Fhat)
    }
    v1 <- 1:n * NA
    v2 <- v1
    for (k in 4:n) {
        k2 <- floor(k/4)
        q1 <- quantilesLogConDens((n - k/4 + 1)/n, x, est$phi, 
            est$Fhat)
        q2 <- quantilesLogConDens((n - k/2 + 1)/n, x, est$phi, 
            est$Fhat)
        q3 <- quantilesLogConDens((n - k + 1)/n, x, est$phi, 
            est$Fhat)
        v1[k] <- (q1 - q2)/(q2 - q3)
        v2[k] <- (x[n - k2 + 1] - x[n - 2 * k2 + 1])/(x[n - 2 * 
            k2 + 1] - x[n - 4 * k2 + 1])
    }
    v1 <- log(v1)/log(2)
    v2 <- log(v2)/log(2)
    res <- cbind(k = 1:n, logcon = v1, order = v2)
    return(res)
}
