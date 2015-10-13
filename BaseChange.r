base.change <- function(x,base.origin,base.out) {
  x <- as.character(x)
  x <- strsplit(x,"")
  x <- x[[1]]
  b <- cbind(rep(10:35,2),c(letters,LETTERS))
  x[x %in% b[,2]] <-
    unlist(lapply(1:length(x),function(i)
      b[x[i] == b[,2],1]))
  x <- as.numeric(x)
  x <- sum((base.origin ^ ((length(
    x
  ) - 1):0)) * x)
  while (x[length(x)] %/% base.out != 0) {
    x[length(x) + 1] <- x[length(x)] %/% base.out
  }
  o <- x %% base.out
  o[which(o > 9)] <- LETTERS[o[which(o > 9)] - 9]
  noquote(paste(rev(o),collapse = ""))
}
