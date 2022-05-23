bst<-function(val)
{ node <- new.env(hash = FALSE, parent = emptyenv())
  node$value <- val # Node Value
  node$left <- NULL # left children key
  node$right <- NULL # Right children key
  class(node) <- "bnode"
  return(node)
  
}

insertnode<-function(nod,key){
  if(is.null(nod)){ 
    nod<-bst(key)
  }else if(key>nod$value){
   nod$right<-insertnode(nod$right,key)
  }else{
    nod$left<-insertnode(nod$left,key)
  }
 nod
}
searchnode<-function(nod,key)
{ if(is.null(nod)){
  return(FALSE)
}else if(key==nod$value){  return(TRUE)
  }else if(key>nod$value){
    searchnode(nod$right,key)
  }else{
    searchnode(nod$left,key)
  }
}

inorder<-function(tree){
  if(!is.null(tree))
  { 
    inorder(tree$left)
    print(tree$value)
    inorder(tree$right)
  }
}

btree<-bst(10)
second<-btree$left<-bst(8)
third<-btree$right<-bst(13)
btree<-insertnode(btree,7)
btree<-insertnode(btree,15)
btree<-insertnode(btree,12)
btree<-insertnode(btree,9)
inorder(btree)
res<-searchnode(btree,21)
print(res)