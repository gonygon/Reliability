#' Reliability_test2() Function
#'
#' 신뢰성학회 연습용 입니다.
#' @param a,b,c :a is data b is numeric
#' @keywords Reliability_test2
#' @export

Reliability_test2<-function(r,c_r,c_i){
  n_star<-ceiling(sqrt(r*(c_r/c_i)))
  n_s<-n_star+30
  n<-c(1:n_s)
  C_n<-(r/n)*c_r+n*c_i
  best_cost<-min(C_n)
  best_n<-which.min(C_n)
  x<-plot(n,C_n, type = 'l',col="red",abline(v=best_n))
  Reliability<-c(best_n,best_cost,x)
  names(Reliability)<-c("최적점검횟수","총 기대비용")
  Reliability
}
