fget_A_RothC <- function(clay = 23.4) 
{ 
    if ((clay < 0) | (clay > 100)) stop("Invalid clay value. Must be >= 0 AND <= 100.")
    ks <- c(10,0.3,0.66,0.02,0)
    x <- 1.67 * (1.85 + 1.6 * exp(-0.0786 * clay))
    B <- 0.46/(x + 1)
    H <- 0.54/(x + 1)
    ai3 <- B * ks
    ai4 <- H * ks
    A <- diag(-ks)
    A[3, ] <- A[3, ] + ai3
    A[4, ] <- A[4, ] + ai4
    A
}  
