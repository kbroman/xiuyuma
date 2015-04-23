#' count the numbers of the first digits of numeric vector data 
#'
#' count the numbers of the first digits(0 excluded) of numeric vector data
#'
#' @param v a numerical vector 
#' 
#'
#' @return A length 9 vector with ith component represents the number of first digits to be i in the input vector v.
#'
#' @examples
#' v<-c(0,0,0,1.2,265.7,345.6,78,89,-91,-90.9,0.1,-0.02)
#' digitscount(v)
#'
#' @export
digitscount<-function(v){
  if(is.vector(v)&&is.numeric(v)){
    n<-length(v)
    count<-rep(0,9)
    for(i in 1:n){
      if(v[i]!=0){
      if(abs(v[i])>1){
        while(abs(v[i]/10)>=1){
          v[i]<-v[i]/10
        }
        count[floor(abs(v[i]))%%10]=count[floor(abs(v[i]))%%10]+1
      }
      else{
        while(abs(v[i])<1){
          v[i]<-v[i]*10
        }
        count[floor(abs(v[i]))%%10]=count[floor(abs(v[i]))%%10]+1
      }
      }
      
    }
  }
  else{
    print("invalid input")
  }
  return(count)
}
