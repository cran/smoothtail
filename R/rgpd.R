`rgpd` <-
function (n, gam, sigma = 1) 
{
    u <- runif(n, 0, 1)
    r <- qgpd(u, gam, sigma)
    err <- 0
    if (n <= 0) {
        err <- 1
        cat("Please give a strict positive n!")
    }
    if (err == 0) {
        return(sort(r))
    }
}
