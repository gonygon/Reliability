#' Reliability_test2() Function
#'
#' 비용최적화를 위한 R 패키지 함수이며 (r,Cr,Ci)형태의 함수를 유지하고 있습니다
#' r에는 단위시간당 평균고장횟수를 입력 c_r에는 고장시수리비용 c_i에는 점검비용을 입력합니다.
#' 따라서 이를 통해 최적 점검횟수와 총 기대비용이 구해지며 그래프를 통해 기대비용이 어떠한 분포를 이루는지 알 수 있습니다.
#' @param r:평균고장횟수 Cr: 고장시 수리비용 Ci:점검비용
#' @keywords Reliability_test2
#' @export

Reliability_test2<-function(r,Cr,Ci){
  n_star<-ceiling(sqrt(r*(Cr/Ci)))
  n_s<-n_star+30
  n<-c(1:n_s)
  Cn<-(r/n)*Cr+n*Ci
  best_cost<-min(Cn)
  best_n<-which.min(Cn)
  x<-plot(n,Cn, type = 'l',col="red",xlab="점검주기",ylab="기대비용",main="점검주기에 따른 기대비용 분포"); abline(v=best_n,lty="dotted",col="blue");abline(h=best_cost,lty="dotted",col="blue")
  Reliability<-c(best_n,best_cost,x)
  names(Reliability)<-c("최적점검횟수","총기대비용")
  Reliability
}
