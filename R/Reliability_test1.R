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
  names(Reliability)<-c("평균수명","고장률","단위시간t동안 고장나지 않을 확률")
  Reliability
}
