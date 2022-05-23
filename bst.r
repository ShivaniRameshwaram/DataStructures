binarySearchtree<-function(node,val)
{
  node <- new.env(hash = FALSE, parent = emptyenv())
  
  if(val>btree$value){
    node$right<-node
  }else{
    node$left<-node
  }
  node$left <- NULL # left children key
  node$right <- NULL # Right children key
  class(node) <- "bnode"
  
  return(node)
}
preorder<-function(tree){
  if(!is.null(tree))
  { print(tree$value)
    preorder(tree$left)
    preorder(tree$right)
    
    
  }
}
btree<-0
newt<-btree<-binarySearchtree(btree,11)
binarySearchtree(btree,23)
binarySearchtree(btree,7)

preorder(newt)