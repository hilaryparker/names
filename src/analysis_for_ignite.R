library('ProjectTemplate')
load.project()

year.ind<-1880:2011

rel.risk <- female.percents[,-1]
for(i in 1:dim(female.percents)[2] - 1){
  rel.risk[,i] <- female.percents[,i+1] / female.percents[,i]
}

# create table of biggest drops #
bigdropsind <- which(
  rel.risk < 0.33,
  arr.ind = TRUE
)
loss <- (1 - round(rel.risk[bigdropsind],2)) * 100
yearlost <- year.ind[bigdropsind[,2] + 1]
bigdrops <- as.data.frame(
  cbind(
    name = rownames(bigdropsind),
    loss,
    yearlost
  )
)

setwd("./reports")
write.table(
  bigdrops,
  file = "bigdrops.csv",
  row.names = FALSE,
  sep = ","
)
setwd("..")

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


# limit to names past 1935
bigdropsind <- bigdropsind[-c(1,2,3,4),]
# adjust len
len<-length(year.ind[which(year.ind==1945):length(year.ind)])

# create graph #

setwd("./graphs")
pdf(
  file="names_past_1945.pdf",
  width = 10, 
  height = 7
)

cutoff <- which(colnames(rel.risk) == 1945) + 1

plot(
  x = year.ind[which(year.ind==1945):length(year.ind)],
  y = female.percents["Hilary", cutoff:dim(female.percents)[2]],
  type = "l",
  ylim = c(0,0.2),
  xlab = "Year",
  ylab = "Percent",
  cex.lab = 1.5,
  cex.axis = 1.5
)

for(i in 1:len){
	lines(
    x = year.ind[which(year.ind==1945):length(year.ind)],
    y = female.percents[bigdropsind[i,1], cutoff:dim(female.percents)[2]],
    col = cols[i+4],
    lwd=3
  )
}

lines(
  x = year.ind[which(year.ind==1945):length(year.ind)],
  y = female.percents["Hilary", cutoff:dim(female.percents)[2]],
  type = "l",
  col = cols[2],
  lwd = 5
)

# put names on graph #


# quick function for plotting names on graph #
plotname <- function(ind, offx, offy){
  tmprow <- bigdropsind[ind,1]
  tmpcol <- bigdropsind[ind,2]
  text(
    x = year.ind[tmpcol] + offx,
    y = female.percents[tmprow, tmpcol] + offy,
    labels = rownames(bigdropsind)[ind],
    col = cols[ind+4],
    cex = 1.5,
    font = 2
  )
}

# tweak each of these individually so graph looks nice in this case #
plotname(1,-5,0.002)
plotname(2,-2,0.02)
plotname(3,-4.8,0.013)
plotname(4,1,0.008)
plotname(5,0,-0.04)
plotname(6,0,-0.025)
plotname(7,-2,0.01)
# skip hilary
plotname(9,3,0.01)
plotname(10,0,0.01)

# add hilary by hand
text(
  x = 1955,
  y = 0.018,
  labels = "Hilary",
  col = cols[2],
  cex = 1.5,
  font = 2
)

dev.off()



# Catina, Katina
pdf(
  file="Catina_Katina.pdf",
  width = 10, 
  height = 7
)

plot(
  x = year.ind[which(year.ind==1945):length(year.ind)],
  y = female.percents["Katina", cutoff:dim(female.percents)[2]],
  type = "l",
  ylim = c(0,0.2),
  xlab = "Year",
  ylab = "Percent",
  cex.lab = 1.5,
  cex.axis = 1.5
)

lines(
    x = year.ind[which(year.ind==1945):length(year.ind)],
    y = female.percents["Katina", cutoff:dim(female.percents)[2]],
    col = cols[6],
    lwd=3
)

lines(
  x = year.ind[which(year.ind==1945):length(year.ind)],
  y = female.percents["Catina", cutoff:dim(female.percents)[2]],
  col = cols[7],
  lwd=3
)

# tweak each of these individually so graph looks nice in this case #
plotname(2,-2,0.02)
plotname(3,-4.8,0.013)

dev.off()


# Farrah
pdf(
  file="Farrah.pdf",
  width = 10, 
  height = 7
)

plot(
  x = year.ind[which(year.ind==1945):length(year.ind)],
  y = female.percents["Farrah", cutoff:dim(female.percents)[2]],
  type = "l",
  ylim = c(0,0.2),
  xlab = "Year",
  ylab = "Percent",
  cex.lab = 1.5,
  cex.axis = 1.5
)

lines(
  x = year.ind[which(year.ind==1945):length(year.ind)],
  y = female.percents["Farrah", cutoff:dim(female.percents)[2]],
  col = cols[8],
  lwd=3
)


# tweak each of these individually so graph looks nice in this case #
plotname(4,1,0.008)

dev.off()

# Iesha
pdf(
  file="Iesha.pdf",
  width = 10, 
  height = 7
)

plot(
  x = year.ind[which(year.ind==1945):length(year.ind)],
  y = female.percents["Iesha", cutoff:dim(female.percents)[2]],
  type = "l",
  ylim = c(0,0.2),
  xlab = "Year",
  ylab = "Percent",
  cex.lab = 1.5,
  cex.axis = 1.5
)

lines(
  x = year.ind[which(year.ind==1945):length(year.ind)],
  y = female.percents["Iesha", cutoff:dim(female.percents)[2]],
  col = cols[11],
  lwd=3
)


# tweak each of these individually so graph looks nice in this case #
plotname(7,-2,0.01)

dev.off()

# Ashanti
pdf(
  file="Ashanti.pdf",
  width = 10, 
  height = 7
)

plot(
  x = year.ind[which(year.ind==1945):length(year.ind)],
  y = female.percents["Ashanti", cutoff:dim(female.percents)[2]],
  type = "l",
  ylim = c(0,0.2),
  xlab = "Year",
  ylab = "Percent",
  cex.lab = 1.5,
  cex.axis = 1.5
)

lines(
  x = year.ind[which(year.ind==1945):length(year.ind)],
  y = female.percents["Ashanti", cutoff:dim(female.percents)[2]],
  col = cols[14],
  lwd=3
)


# tweak each of these individually so graph looks nice in this case #
plotname(10,0,0.01)

dev.off()


# Hilary and Hillary
pdf(
  file="Hilary_Hillary.pdf",
  width = 10, 
  height = 7
)

plot(
  x = year.ind[which(year.ind==1945):length(year.ind)],
  y = female.percents["Hillary", cutoff:dim(female.percents)[2]],
  type = "l",
#  ylim = c(0,0.2),
  xlab = "Year",
  ylab = "Percent",
  cex.lab = 1.5,
  cex.axis = 1.5
)

lines(
  x = year.ind[which(year.ind==1945):length(year.ind)],
  y = female.percents["Hillary", cutoff:dim(female.percents)[2]],
  col = cols[1],
  lwd=3
)

lines(
  x = year.ind[which(year.ind==1945):length(year.ind)],
  y = female.percents["Hilary", cutoff:dim(female.percents)[2]],
  col = cols[2],
  lwd=3
)

# tweak each of these individually so graph looks nice in this case #
text(
  x = 1999,
  y = 0.05,
  labels = "Hilary",
  col = cols[2],
  cex = 1.5,
  font = 2
)

text(
  x = 1985,
  y = 0.10,
  labels = "Hillary",
  col = cols[1],
  cex = 1.5,
  font = 2
)

dev.off()


# Adolph and Adolf
pdf(
  file="Adolph_Adolf_Hilary_Hillary.pdf",
  width = 10, 
  height = 7
)

plot(
  x = year.ind,
  y = male.percents["Adolf",],
  type = "l",
  ylim = c(0,0.15),
  col = cols[4],
  lwd = 3,
	xlab="Year",
  ylab="Percent",
  cex.lab = 1.5,
  cex.axis = 1.5
)
lines(x = year.ind, y = male.percents["Adolph",], col = cols[3], lwd = 3)
lines(x = year.ind, y = female.percents["Hilary",], col = cols[2], lwd=3)
lines(x = year.ind, y = female.percents["Hillary",], col = cols[1], lwd=3)


# Adolf
text(x=1919,y=0.015,col=cols[4],labels="Adolf",font=2,cex=1.5)
# Adolph
text(x=1930,y=0.05,col=cols[3],labels="Adolph",font=2,cex=1.5)
# Hilary
text(x=2001,y=0.055,col=cols[2],labels="Hilary",font=2,cex=1.5)
# Hillary
text(x=1984,y=0.13,col=cols[1],labels="Hillary",font=2,cex=1.5)
dev.off()


# Katrina
pdf(
  file="Katrina_Hilary.pdf",
  width = 10, 
  height = 7
)
plot(
  x=year.ind,
  y=female.percents["Katrina",],
  type="l",
  xlim=c(1945,2011),
#  ylim=c(0,0.03),
  xlab="Year",
  ylab="Percent",
  col=cols[3],
  lwd=3,
  cex.lab = 1.5,
  cex.axis = 1.5
)

lines(
  x = year.ind[which(year.ind==1945):length(year.ind)],
  y = female.percents["Hilary", cutoff:dim(female.percents)[2]],
  col = cols[2],
  lwd=3
)
text(x=1980,y=0.06,col=cols[2],labels="Hilary",font=2,cex=1.5)
text(x=2000,y=0.1,col=cols[3],labels="Katrina",font=2,cex=1.5)
dev.off()



