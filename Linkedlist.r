create_emptyenv <- function() {
  emptyenv()
}

isEmpty <- function(llist) {
  if(class(llist)!= "linkList") warning("Not linkList class")
  identical(llist, create_emptyenv())
}

linkListNode <- function(val, node=NULL) {
  llist <- new.env(parent=create_emptyenv())
  llist$element <- val
  llist$nextnode <- node
  class(llist) <- "linkList"
  llist
}

setNextNode<-function(llist){
  llist$nextnode
}

setNextElement<-function(llist){
  llist$element
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
addElement<-function(new,llist)
{
  if (isEmpty(llist)) {
    llist<-linkedlist(new)
  } else{
    new$nextnode<-llist
    llist<-new
  }
  llist
}

addLast<-function(last,llist)
{
  if (isEmpty(llist)) {
    llist<-linkedlist(last)
  } else{
    temp<-new.env(parent = create_emptyenv())
    temp<-llist
    i=1
    while(i<listsize){ 
      temp<-temp$nextnode
      i=i+1
    }
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
    
    prev$nextnode<-new
    new$nextnode<-temp
  }
  llist
}

delElement<-function(llist,val)
{ 
  listsize<-sizeLinkList(llist)
  i=1
  temp<-new.env(parent=create_emptyenv())
  temp<-llist
  while(i<listsize)
  { if(temp$element==val){
    break
  }
    prev<-temp
    temp<-temp$nextnode
    i=i+1
  }
  prev$nextnode<-temp$nextnode
  llist
}

LList <-linkListNode(5,linkListNode(2,linkListNode(3,create_emptyenv())))
listsize<-sizeLinkList(LList)
new<-linkListNode(10,create_emptyenv())
LList<-addElement(new,LList)
listsize<-sizeLinkList(LList)
last<-linkListNode(15,create_emptyenv())
LList<-addLast(last,LList)
listsize<-sizeLinkList(LList)

new<-linkListNode(7,create_emptyenv())
LList<-InsertIndex(new,3,LList)
listsize<-sizeLinkList(LList)

LList<-delElement(LList,7)
listsize<-sizeLinkList(LList)
temp<-LList
for(i in 1:listsize)
{ print(temp$element)
  temp=temp$nextnode
}