#' Reliability_test1() Function
#'
#' 신뢰성학회 연습용 입니다.
#' @param x,t :x is data t is numeric
#' @keywords Reliability_test
#' @export

Reliability_test1<-function(x,t){
  a<-mean(x)
  b<-1/mean(x)
  c<-exp(-((1/mean(x))*t))
  Reliability<-c(a,b,c)
  names(Reliability)<-c("MMTF","Lambda","R")
  Reliability
}
