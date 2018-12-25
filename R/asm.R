
#calc length by lack of na
#x is group of subsets
#y is original set of features

#in x, each column is a subset

asm<-function(x,y){
  L<-length(x[1,])
  t<-length(y)
  if (L<2 || t<1) {return("Error: wrong input")}

  Astab<-0
  for (i in (1:(L-1))) {
    for (j in ((i+1):L)) {
      if ((length(na.omit(x[,i]))!=0 && length(na.omit(x[,j]))!=0) && (length(na.omit(x[,i]))!=t && length(na.omit(x[,j]))!=t))
        {
      Astab<-Astab+(((length(intersect(na.omit(x[,i]),na.omit(x[,j]))))-((length(na.omit(x[,i]))*length(na.omit(x[,j])))/t))/((min(length(na.omit(x[,i])),length(na.omit(x[,j]))))-(max(0,length(na.omit(x[,i]))+length(na.omit(x[,j]))-t))))
    }}
  }
  Astab<-Astab*2/(L*(L-1))
  return(Astab)
}
