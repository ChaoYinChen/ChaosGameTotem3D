#'ChaosGameTotem3D
#'
#'進行Chaos Game，依照package使用者在三維所設置的初始點與隨機的頂點及所走的步長，迭代數次後點出一個圖騰。
#'
#'Chaos Game是依照玩家給予在三維上一個初始點與任意三個(含)以上的頂點及步長比例。利用隨機選擇玩家給予的任一個頂點，並將其與初始點的距離依照步長比例，畫出一個點，此點即為下一個起點。反覆這些步驟，迭代多次即可畫出明顯的圖騰。
#'
#'
#'
#' @param initial 在平面上任意一個初始點及三個(含)以上頂點，為矩陣型態。
#' @param q 步長比例，設定於[-0.5,0.5]範圍裡，為數值型態。
#' @param iteration 迭代次數，預設=20000。
#'
#'
#' @export
#'
#' @examples
#' initial = matrix(c(2,2,2,20,40,0,55,100,0,80,25,0,100,85,0,40,30,20),nrow = 6,ncol = 3,byrow = TRUE)
#' #初始點(2,2,2)、四個頂點(20,40,0)、(55,100,0)、(80,25,0)、(40,30,20)
#' game3D(initial,q=0.3)
#' # returns 一個三角柱圖騰



game3D = function(initial,q,iteration = 20000){
  library(rgl)
  iteration = iteration+1
  dim = 3
  j = dim(initial)[1]

  points = matrix(0,nrow = (iteration+j-1),ncol = dim)
  vertex = sample(2:j,size = iteration, replace =TRUE)

  points[1,] = initial[1,]

  for(i in 1: (iteration-1)){
    startpoint = initial[vertex[i+1],]
    points[i+1,] = (1-q)*startpoint + q*points[i,]
  }
  points = rbind(points,initial[2:j,])
  plot3d(points[,1],points[,2],points[,3],xlab = "x",ylab = "y",zlab = "z")

}



