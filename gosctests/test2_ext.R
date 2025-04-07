library(CVXR)

# Ax <= b
A <- rbind(
  c(2, 0),   
  c(-2, 0),  
  c(0, 2),   
  c(0, -2) 
)
b <- c(1, 1, 1, 1)


n <- ncol(A)  
c <- Variable(n) #extract c&r
r <- Variable(1)


objective <- Maximize(r)

constraints <- list()
for (i in 1:nrow(A)) {
  a_i <- A[i, ]
  constraints <- c(constraints, t(A[i, ]) %*% c + r * norm(A[i, ], "2") <= b[i])
}


problem <- Problem(objective, constraints)
result <- solve(problem, solver = "ECOS")  # use "ECOS", other solver like "OSQP" "SCS" works fine. 
#QSQP seems to have highest accuracy


c_result <- result$getValue(c)
r_result <- result$getValue(r)


cat("Optimal Center:\n", c_result, "\n","Optimal Radius:", r_result, "\n")
