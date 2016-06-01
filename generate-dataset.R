game_of_life_iter <- function(x)
{
  side = ncol(x)
  X = x
  allW = cbind( rep(0,side) , X[,-side] )
  allNW = rbind(rep(0,side),cbind(rep(0,side-1),X[-side,-side]))
  allN = rbind(rep(0,side),X[-side,])
  allNE = rbind(rep(0,side),cbind(X[-side,-1],rep(0,side-1)))
  allE = cbind(X[,-1],rep(0,side))
  allSE = rbind(cbind(X[-1,-1],rep(0,side-1)),rep(0,side))
  allS = rbind(X[-1,],rep(0,side))
  allSW = rbind(cbind(rep(0,side-1),X[-1,-side]),rep(0,side))
  
  # summation of the matrices
  X2 <- allW + allNW + allN + allNE + allE + allSE + allS + allSW
  
  # the rules of GoL are applied using logical subscripting
  X3 <- X
  X3[X==0 & X2==3] <- 1
  X3[X==1 & X2<2] <- 0
  X3[X==1 & X2>3] <- 0
  X <- X3
  
  return(X)
}

x = matrix(data = rbinom(n = 400,size = 1, prob = 0.4), nrow = 20)
game_of_life_iter(x)
