binarytree<-function(val)
{
  node <- new.env(hash = FALSE, parent = emptyenv())
  node$value <- val # Node Value
  node$left <- NULL # left children key
  node$right <- NULL # Right children key
  class(node) <- "bnode"
  return(node)
}
inorder<-function(tree){
  if(!is.null(tree))
  { 
    inorder(tree$left)
    print(tree$value)
    inorder(tree$right)
  }}
postorder<-function(tree){
    if(!is.null(tree))
    { 
      postorder(tree$left)
      postorder(tree$right)
      print(tree$value)
      
    }
    
}
preorder<-function(tree){
  if(!is.null(tree))
  { print(tree$value)
    preorder(tree$left)
    preorder(tree$right)
    
    
  }
}
maxDepth<-function(node){
  if (is.null(node))
  return (-1) 

else{
  lDepth = maxDepth(node$left)
rDepth = maxDepth(node$right)
if (lDepth > rDepth){
  y<-lDepth+1
  return (y)
}else{
  x<-rDepth+1
  return (x)
}
}
}
btree=binarytree(10)
second<-btree$left<-binarytree(20)
third<-btree$right<-binarytree(30)
second$left<-binarytree(40)
second$right<-binarytree(50)
third$left<-binarytree(60)
third$right<-binarytree(70)

height<-maxDepth(btree)
print(height)