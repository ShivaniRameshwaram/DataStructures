bstnode <- function(key, value) {
  node <- new.env(hash = FALSE, parent = emptyenv())
  node$key <- key # Node key
  node$value <- value # Node Value
  node$left <- NULL # left children key
  node$right <- NULL # Right children key
  class(node) <- "bstnode"
  return(node)
}

# Function to check if node is empty
check_empty<-function(node){
  ifelse((is.null(node$lkey) & is.null(node$rkey)), T, F)
}
  # Function to insert if the node has empty space
  leaf_insert<-function(node, key, val){
    if(check_empty(node)) return(tttnode(lkey=key,
                                         lvalue=val))
    if(is.null(node$rkey)){
      if(key>node$lkey){
        node$rkey<-key
        node$rvalue<-val
      } else
      {
        node$rkey<-node$lkey
        node$rvalue<-node$lvalue
        node$lkey<-key
        node$lvalue<-val
      }
    } else
    {
      node$left<-tttnode(key, val)
    }
    return(node)
  }
  
  l=leaf_insert(1,1,1)
  x=leaf_insert(1,2,2)
  