
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
    return (0) 
  
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
diameter<-function(root){
  if (is.null(root))
    return 
  lh <-maxDepth(root$left)
  rh <-maxDepth(root$right)
  ld <-diameter(root$left)
  rd <-diameter(root$right)
  return (max(lh + rh + 1, max(ld, rd)))
}


leftViewUtil<-function(root, level,max_level){

  if (is.null(root))
    return()
 if (max_level[1] < level){
    print (root$value)
    max_level[1] = level
    }
    leftViewUtil(root$left, level + 1,max_level)
     }
leftView<-function(root){
  max_level<-c(0)
leftViewUtil(root, 1,max_level)
}


rightViewUtil<-function(root, level,max_level){
  
  if (is.null(root)){
    return()
  }else if (max_level[1] < level){
    print (root$value)
    max_level[1] = level
    
  }

  rightViewUtil(root$right, level + 1,max_level)

  
}
rightView<-function(root){
  max_level<-c(0,0,0,0,0,0)
  rightViewUtil(root, 1,max_level)
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
y<-diameter(btree)
print(y)
#left view of a perfect binary tree
leftView(btree)
#right view of a perfect binary tree
rightView(btree)
#top view of a perfect binary tree
leftView(btree)+rightView(btree)
