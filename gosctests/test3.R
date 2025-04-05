library(nloptr)
library(volesti)

# Function: max_inner_ball calculate the largest inscribed ball using CRAN package nloptr given the following parameters:
# P: the Hpolytope with the constraint Ax <= b
# x0: the initial guess for nloptr
# algorithm: the algorithm for nloptr 
# maxeval: max evaluation time for nloptr
# xtol_rel: tolerance for nloptr
# it returns a list with c represent the center of the ball and r represent the radius
max_inner_ball <- function(P, x0 = NULL, algorithm = "NLOPT_LN_COBYLA",
                               maxeval = 100, xtol_rel = 0.000001) {
 
  if (!inherits(P, "Hpolytope")) {
    stop("P should be Hpolytope")
  }
  
  A <- P@A
  b <- P@b
  n <- ncol(A)
  
  #initial guess
  if (is.null(x0)) {
    norms <- sqrt(rowSums(A^2))
    minr <- min(b / norms)
    x0 <- c(rep(2, n), minr * 0.1)
  }
  
  
  objective <- function(x) {
    r <- x[n+1]
    -r #the function minize r, but we want to maximize it
  }
  
  #make sure the ball does not go out of the bound
  constraint <- function(x) {
    center <- x[1:n]
    r <- x[n + 1]
    norms <- sqrt(rowSums(A^2))
    A %*% center + r * norms - b
  }
  
  opts <- list(
    algorithm = algorithm,
    maxeval = maxeval,
    xtol_rel = xtol_rel
  )
  
  lower_bounds <- c(rep(-Inf, n), 0)
  upper_bounds <- c(rep(Inf, n + 1))
  
  result <- nloptr::nloptr(
    x0 = x0,
    eval_f = objective,
    lb = lower_bounds,
    ub = upper_bounds,
    eval_g_ineq = constraint,
    opts = opts
  )
  
  solution <- result$solution
  list(
    c = solution[1:n],
    r = solution[n + 1]
  )
}

P <- volesti::gen_cube(3,"H")
result <- max_inner_ball(P)
cat("Center: ",result$c,"\n")
cat("Radius: ",result$r,"\n")

