#' Reliability_test1() Function
#'
#' 신뢰성학회 연습용 입니다.
#' @param x,t :x is data t is numeric
#' @keywords Reliability_test1
#' @export

Reliability_test1<-function(x,t){
  M<-mean(x)
  L<-1/mean(x)
  R<-exp(-((1/mean(x))*t))
  Reliability<-c(M,L,R)
  names(Reliability)<-c("MMTF","Lambda","R")
  Reliability
}
