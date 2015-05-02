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
     ch <- as.character(v)
    ch <- gsub('[\\.0\\-]', '', ch)
    firstdig <- as.numeric(substr(ch, 1, 1))
    count<-table( factor(firstdig, levels=1:9) )

  }
  else{
    print("invalid input")
  }
  return(count)
}
