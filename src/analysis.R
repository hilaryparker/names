setwd("C:/Users/Hilary/GitHub/names")
library('ProjectTemplate')
load.project()

year.ind<-1880:2011

rel.risk <- female.percents[,-1]
for(i in 1:dim(female.percents)[2]-1){
  rel.risk[,i]<-female.percents[,i+1]/female.percents[,i]
}

# create table of biggest drops #
which(rel.risk<0.33,arr.ind=TRUE)->bigdropsind
loss<-(1-round(rel.risk[bigdropsind],2))*100
yearlost<-year.ind[bigdropsind[,2]+1]
bigdrops<-as.data.frame(cbind(name=rownames(bigdropsind),loss,yearlost))

setwd("C:/Users/Hilary/GitHub/names/reports")
write.table(bigdrops,file="bigdrops.csv",row.names=FALSE,sep=",")
setwd("C:/Users/Hilary/GitHub/names")

#compare to boys#

rel.risk.b <- male.percents[,-1]
for(i in 1:dim(male.percents)[2]-1){
  rel.risk.b[,i]<-male.percents[,i+1]/male.percents[,i]
}
which(rel.risk.b<0.33,arr.ind=TRUE)->bigdropsind.b
loss.b<-(1-round(rel.risk.b[bigdropsind.b],2))*100
yearlost.b<-year.ind[bigdropsind.b[,2]+1]
bigdrops.b<-as.data.frame(cbind(name=rownames(bigdropsind.b),loss.b,yearlost.b))


# create graph of these names #
len<-length(loss)

#display.brewer.all()
colors <- brewer.pal(12, "Paired")
pal <- colorRampPalette(colors)
cols <- pal(len)

# quick function for plotting names on graph #
plotname<-function(ind,offx,offy){
	tmprow<-bigdropsind[ind,1]
	tmpcol<-bigdropsind[ind,2]
	text(x=year.ind[tmpcol]+offx,y=female.percents[tmprow,tmpcol]+offy,
		 labels=rownames(bigdropsind)[ind],col=cols[ind],font=2)
}

# create graph #

setwd("C:/Users/Hilary/GitHub/names/graphs")
png(file="names.png",width = 480*2, height = 480)
plot(x=year.ind,y=female.percents["Hilary",],type="l",ylim=c(0,0.2),xlab="Year",ylab="Percent",
	 main="Percent of baby girls given a name over time for the 14 most poisoned names")
for(i in 1:len){
	lines(x=year.ind,y=female.percents[bigdropsind[i,1],],col=cols[i],lwd=3)
}
lines(x=year.ind,y=female.percents["Hilary",],type="l",col=cols[12],lwd=5)

# put names on graph #
# tweak each of these individually so graph looks nice in this case #
plotname(1,1,0.005)
plotname(2,0,0.005)
plotname(3,0,0.012)
plotname(4,0,0.005)
plotname(5,-2,0.005)
plotname(6,0,0.018)
plotname(7,-3,0.01)
plotname(8,1,0.008)
plotname(9,5,-0.015)
plotname(10,0,-0.025)
plotname(11,0,0.005)
plotname(12,-7,0.005)
plotname(13,2.5,0.005)
plotname(14,0,0.005)
dev.off()



## non-one-hit-wonders ##

nonflashind<-rowSums((!is.na(female.percents[bigdropsind[,1],])))>20
bigdropsind.non<-bigdropsind*nonflashind
bigdropsind.non<-bigdropsind.non[bigdropsind.non[,1]!=0,]

setwd("C:/Users/Hilary/GitHub/names/graphs")
png(file="names_trimmed.png",width = 480*2, height = 480)

plot(x=year.ind,y=female.percents["Hilary",],type="l",ylim=c(0,0.1),xlab="Year",ylab="Percent",
	 main="Percent of baby girls given a name over time for the 14 most poisoned names, controlling for fads")
for(i in 1:dim(bigdropsind.non)[1]){
	lines(x=year.ind,y=female.percents[bigdropsind.non[i,1],],col=cols[i],lwd=3)
}
lines(x=year.ind,y=female.percents["Hilary",],type="l",col=cols[12],lwd=5)
plotname(1,1,0.005)
plotname(2,0,0.005)
plotname(3,0,0.012)
plotname(12,0,0.008)
dev.off()



# get Hillary on the graph too #

# create table of biggest drops #
which(rel.risk<0.39,arr.ind=TRUE)->bigdropsind
loss<-(1-round(rel.risk[bigdropsind],2))*100
yearlost<-year.ind[bigdropsind[,2]+1]
bigdrops<-as.data.frame(cbind(name=rownames(bigdropsind),loss,yearlost))
bigdrops

nonflashind<-rowSums((!is.na(female.percents[bigdropsind[,1],])))>20
bigdropsind.non<-bigdropsind*nonflashind
bigdropsind.non<-bigdropsind.non[bigdropsind.non[,1]!=0,]
len<-dim(bigdropsind.non)[1]

cols <- pal(len)


setwd("C:/Users/Hilary/GitHub/names/graphs")
png(file="more_names_trimmed.png",width = 480*2, height = 480)
plot(x=year.ind,y=female.percents["Hilary",],type="l",ylim=c(0,0.35),xlab="Year",ylab="Percent",
	 main="Percent of baby girls given a name over time for the 39 most poisoned names, controlling for fads")
for(i in 1:dim(bigdropsind.non)[1]){
	lines(x=year.ind,y=female.percents[bigdropsind.non[i,1],],col=cols[i],lwd=3)
}

# Hilary
text(x=1996.5,y=0.06,col=cols[20],labels="Hilary",font=2)
# Hillary
text(x=1995,y=0.14,col=cols[21],labels="Hillary",font=2)
# Marian
text(x=1954,y=0.21,col=cols[18],labels="Marian",font=2)
# Christin
text(x=1993,y=-0.001,col=cols[19],labels="Christin",font=2)

dev.off()



# Adolph and Adolf

#display.brewer.all()
colors <- brewer.pal(8, "Dark2")
pal <- colorRampPalette(colors)
cols <- pal(5)

setwd("C:/Users/Hilary/GitHub/names/graphs")
png(file="names_adolf.png",width = 480*2, height = 480)
plot(x=year.ind,y=male.percents["Adolf",],type="l",ylim=c(0,0.15),col=cols[2],lwd=3,
	 xlab="Year",ylab="Percent",
	 main="Percent of babies named Adolf, Adolph, Hilary or Hillary over time")
lines(x=year.ind,y=male.percents["Adolph",],col=cols[1],lwd=3)
lines(x=year.ind,y=female.percents["Hilary",],col=cols[3],lwd=3)
lines(x=year.ind,y=female.percents["Hillary",],col=cols[4],lwd=3)


# Adolf
text(x=1919,y=0.01,col=cols[2],labels="Adolf",font=2)
# Adolph
text(x=1937,y=0.025,col=cols[1],labels="Adolph",font=2)
# Hilary
text(x=1997,y=0.06,col=cols[3],labels="Hilary",font=2)
# Hillary
text(x=1995,y=0.13,col=cols[4],labels="Hillary",font=2)
dev.off()
