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

y = data.frame(row = c(3,5), column=c(2,7))

require(ggplot2)
ggplot(y + 0.5, aes(row,column)) + geom_tile() + 
  theme_bw() + 
  theme(panel.grid.major = element_line(size = 2, color='black'),       
        panel.grid.minor = element_line(size=2, color = 'black'),
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank()) + 
  coord_cartesian(xlim=c(0,20), ylim=c(0,20))


x = matrix(data = rbinom(n = 400,size = 1, prob = 0.4), nrow = 20)
for(i in 1:10)
{
  y = melt(x, varnames = c('row', 'column'))
  y = y[ y$value == 1, c(1,2)]
  jpeg(filename = paste0("R/game-of-life/plot/game-of-life-", i, ".jpg"))
  print(ggplot(y + 0.5, aes(row,column)) + geom_tile() + theme_bw())
  dev.off()
  x = game_of_life_iter(x)
}


chessdat <- read.table(text='row,column
                      1,0
                      5,1
                      7,2
                      2,3
                      0,4
                      3,5
                      6,6
                      4,7', sep =',', header = T)


y = data.frame(x)

require(highcharter)
highchart() %>% 
  hc_chart(type = "heatmap") %>% 
  hc_add_series(data = y) %>% 
  hc_colorAxis(stops = c('black', 'white'))