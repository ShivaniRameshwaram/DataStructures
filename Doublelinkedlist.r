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
LList<-dlinkListNode(5,create_emptyenv(),dlinkListNode(4,LList,dlinkListNode(3,LList$nextnode,create_emptyenv())))
listsize<-sizeLinkList(LList)
new<-dlinkListNode(10)
LList<-addFirst(LList,new)
listsize<-sizeLinkList(LList)
temp<-new.env(create_emptyenv())
temp<-LList
i=listsize
j=1
while(j<=listsize)
{  cat(temp$element," <- ")
  temp<-temp$nextnode
  j=j+1
}
cat("NULL")
#while(i>0 )
#{
#  print(temp$element)
#  temp<-temp$prevnode
#  i=i-1
#}