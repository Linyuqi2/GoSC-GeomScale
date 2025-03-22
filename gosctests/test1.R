library(volesti)

dimension = 4
P <- volesti::gen_cube(dimension,"H") #using prototype cube, for instance

result <- volesti::inner_ball(P)
print(class(result))  
cat(result,"\n") 

c <- result[1:dimension]
r <- result[dimension + 1]
cat("Center:", c, "\n")
cat("Radius:", r, "\n")