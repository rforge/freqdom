#' @export
print.timedom = function (object,operators=TRUE) {
  cat("Time domain object\n\n")
  if (operators)
    for (i in 1:length(object$lags)){
    cat(paste("$operators[",i,",,] (operator at the lag ",object$lags[i],")\n",sep=""))
    print(object$operators[i,,])
    cat("\n")
  }
  cat(paste("Dimentions:",paste(dim(object$operators)[-1],collapse=" x "),"\n"))
  cat(paste("\n$lags (available lags)\n",paste(object$lags,collapse=" "),"\n"))
}

#' @export
print.freqdom = function (object,operators=TRUE) {
  cat("Frequency domain object\n\n")
  cat(paste("Operator dimentions:",paste(dim(object$operators)[-1],collapse=" x "),"\n"))
  cat(paste("\n$operators (matrices at each frequency)\n"))
  
  if (operators)
    for (i in 1:length(object$freq)){
    cat(paste("$operators[",i,",,] (operator at frequency ",object$freq[i],")\n",sep=""))
    print(signif(object$operators[i,,],4))
    cat("\n")
  }
  cat(paste("$freq (sample frequencies)\n"))
  print(signif(object$freq,4))
}

#' @export
summary.timedom = function (object) {
  print.timedom(object,FALSE)
}

#' @export
summary.freqdom = function (object) {
  print.freqdom(object,FALSE)
}