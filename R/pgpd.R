`pgpd` <-
function (q, gam, sigma = 1) 
{
    n <- length(q)
    p <- 1:n * NA
    if (gam < 0) {
        II <- (1:n) * (q >= 0) * (q < -sigma/gam)
        p[II] <- 1 - (1 + gam * q[II]/sigma)^(-1/gam)
    }
    II <- (1:n) * (q >= 0)
    if (gam == 0) {
        p[II] <- 1 - exp(-q[II]/sigma)
    }
    if (gam > 0) {
        p[II] <- 1 - (1 + gam * q[II]/sigma)^(-1/gam)
    }
    return(p)
}
