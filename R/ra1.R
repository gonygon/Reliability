#' ra1() Function
#'
#' 평균수명,고장률,단위시간동안 고장나지 않을 확률을 계산해주는 함수 입니다.
#' x에는 데이터 값을 t에는 단위시간을 입력하면 됩니다
#' @param x:데이터값 t:단위시간
#' @keywords ra1
#' @export

ra1<-function(x,t){
  M<-mean(x)
  L<-1/mean(x)
  R<-exp(-((1/mean(x))*t))
  g<-boxplot(x)
  Reliability<-c(M,L,R,g)
  names(Reliability)<-c("평균수명","고장률","단위시간t동안 고장나지 않을 확률")
  Reliability
}
