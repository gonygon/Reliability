#' test() Function
#'
#' 신뢰성학회 연습용 입니다.
#' @param a,b,c :a is data b is numeric
#' @keywords test
#' @export

test<-function(a,b,c){
  n_star<-ceiling(sqrt(a*(b/c)))
  n_s<-n_star+30
  n<-c(1:n_s)
  k<-(a/n)*b+n*c
  r<-min(k)
  x<-plot(n,k, type = 'b',col="red")
  Reliability<-c(n_star,r,x)
  names(Reliability)<-c("최적점검횟수","최적비용")
  Reliability
}
