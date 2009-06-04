`generalizedPick` <-
function (x, c = 0.75, gam0 = -1/3) 
{
    n <- length(x)
    delta <- abs(gam0 + 1/2) - 1/2
    res <- NA
    est <- activeSetLogCon(x, print = FALSE)
    q <- 1:n * NA
    for (w in 1:n) {
        q[w] <- quantilesLogConDens(w/n, x, est$phi, est$Fhat)
    }
    v1 <- 1:n * NA
    v2 <- v1
    for (k in 1:(n - 1)) {
        j <- 1:k
        lambda1 <- lambdaGenPick(t = j/k, delta, c)
        lambda2 <- lambdaGenPick(t = (j - 1)/k, delta, c)
        v1[k] <- sum((lambda1 - lambda2) * log(q[n - floor(c * 
            j)] - q[n - j]))
        v2[k] <- sum((lambda1 - lambda2) * log(x[n - floor(c * 
            j)] - x[n - j]))
    }
    res <- cbind(k = 1:n, logcon = v1, order = v2)
    return(res)
}
