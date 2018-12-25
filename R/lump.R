
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#Takes vectors and combines them, adding na to have uniform length

lump<- function(...) {
  m<-list(...)
  n<-max(lengths(m))
  m<-lapply(m, 'length<-', n)
  result<-as.data.frame(m)
  names(result)<-NULL
  return(result)
}
