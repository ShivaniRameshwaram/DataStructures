create_emptyenv <- function() {
  emptyenv()
}

isEmpty <- function(llist) {
  if(class(llist)!= "dlinkList") warning("Not linkList class")
  identical(llist, create_emptyenv())
}

dlinkListNode <- function(val, prevnode=NULL, node=NULL) {
  llist <- new.env(parent=create_emptyenv())
  llist$prevnode <- prevnode
  llist$element <- val
  llist$nextnode <- node
  class(llist) <- "dlinkList"
  llist
}

sizeLinkList<-function(llist, size=0){
  if (isEmpty(llist))
  {
    return(size)
  } else
  {
    size<-size+1L
    sizeLinkList(llist$nextnode, size)
  }
}
addFirst<-function(llist,new){
  if (isEmpty(llist))
    llist<-dlinkListNode(new)
  else{
    
    llist$prevnode<-new
    new$nextnode<-llist
    llist<-new
  }
  llist
}

addLast<-function(llist,last){
  if (isEmpty(llist)){
    llist<-dlinkListNode(last)
  }else{
    temp<-new.env(parent = create_emptyenv())
    temp<-llist
    k=1
    listsize<-sizeLinkList(llist)
    while(k<listsize){
      temp<-temp$nextnode
      k=k+1
    }
    
    last$prevnode<-temp
    temp$nextnode<-last
  }
  llist
}

InsertIndex<-function(new,pos,llist)
{  if (isEmpty(llist)) {
  warning("Empty List")
}
  else{
   
    temp<-new.env(parent = create_emptyenv())
    temp<-llist
    i=1
    while(i<pos)
    { prev<-temp
      temp<-temp$nextnode
      i=i+1
     
    }
    new$nextnode<-temp
    new$prevnode<-prev
    prev$nextnode<-new
    temp$prevnode<-new
  }
  llist
}


LList<-dlinkListNode(5,create_emptyenv(),dlinkListNode(4,LList,dlinkListNode(3,LList$nextnode,)))
listsize<-sizeLinkList(LList)
new<-dlinkListNode(10)
LList<-addFirst(LList,new)
last<-dlinkListNode(15)
LList<-addLast(LList,last)
index<-dlinkListNode(17)
LList<-InsertIndex(index,3,LList)
listsize<-sizeLinkList(LList)
temp<-new.env(create_emptyenv())
temp<-LList
j=1
while(j<=listsize)
{  cat(temp$element," <- ")
  temp<-temp$nextnode
  j=j+1
}
cat("NULL")
