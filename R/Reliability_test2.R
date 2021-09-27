#' Reliability_test2() Function
#'
#' 신뢰성학회 연습용 입니다.
#' @param a,b,c :a is data b is numeric
#' @keywords Reliability_test2
#' @export

Reliability_test2<-function(a,b,c){
  n_star<-ceiling(sqrt(a*(b/c)))
  n_s<-n_star+30
  n<-c(1:n_s)
  k<-(a/n)*b+n*c
  r<-min(k)
  t<-which.min(k)
  x<-plot(n,k, type = 'b',col="red")
  Reliability<-c(t,r,x)
  names(Reliability)<-c("n","best_cost")
  Reliability
}
