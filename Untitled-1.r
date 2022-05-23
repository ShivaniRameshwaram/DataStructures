traversal<-function(llist)
{
  if (isEmpty(llist))
  warning("List is empty")
  else{
    temp<-new.env(create_empenv())
    temp<-llist
    listsize<-sizeLinkList(llist)
    i=1
    for(i in 1:listsize)
  { cat(temp$element,"<-")
    temp=temp$nextnode
  }
  cat("NULL")
  }
}