#'Comparison of raman spectrum intensity with control
#'
#'Compare intensity between sample and control according to raman shift
#'@param g.name: group name of comparison target
#'@param var.name: variable name (Raman shift)
#'@param ctr.name: control sp in group
#'@return statiscal comparison of raman intensity distributionby raman shift
#'@examples
#'examples not included
#
#'@export
ctr.tt <- function(data=data, g.name,var.name, ctr.name="a0"){
  grp=as.vector(unique(data %>% select(g.name)))
  var=as.vector(unique(data %>% select(var.name)))
  d=data.frame()
  for (i in 1:nrow(var)) {
    temp=subset(data,data[,which(colnames(data)==var.name)]==var[i,])
    ctemp=subset(temp,temp[,which(colnames(data)==g.name )]==ctr.name)
    for (j in 1:nrow(grp)) {
      ttemp=subset(temp,temp[,which(colnames(data)==g.name)]==grp[j,])
      tt=t.test(ctemp$value, ttemp$value, var.equal = T)
      new_d=data.frame(ctr.name,grp[j,],var[i,], tt$statistic,tt$stderr,tt$p.value)
      d=rbind.data.frame(d,new_d)
    }
  }
  d=d %>% `colnames<-`(c("Ctr.sp","Comp.sp","variable","statistic","stderr","p"))
  d=subset(d,d[,2]!=ctr.name) %>% droplevels()
  return(d)
}
