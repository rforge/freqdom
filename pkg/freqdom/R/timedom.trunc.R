#' @export
timedom.trunc = function(A, lags){
  if (!is.timedom(A))
    stop ("A must be a time domain filter")
  
  D = dim(A$operators)
  D[1] = sum(A$lags %in% lags)
  A$operators = array(A$operators[A$lags %in% lags,,],D)
  A$lags = intersect(lags,A$lags)
  A
}

