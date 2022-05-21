n = as.integer(readline(prompt = "enter number"))
for (i in 1:n){
  for (j in 1:i){
    if (j==1||j==i||i==n) {
      cat("*")
    } else{
      cat(" ")
    }
  }
  cat("\n")
}