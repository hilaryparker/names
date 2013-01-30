# Function for retrieving names from the SSA Website #
# set the years you'd like to examine #
# min year = 1880, max year = 2011 #
# number="p" gives percent, number="n" gives raw number. Rank is always given #
# female=TRUE female names, FALSE male names

# returns two matrices -- one with the raw number or percentages, and one with the ranks #

getNames<-function(year.ind=seq(1950,2011),number="p",female=TRUE){
  nametable<-list()
  names<-c(NA)
  
  # scrape from website #
  for(i in 1:length(year.ind)){
    # get the data from the website as a POST form #
    raw <- postForm("http://www.ssa.gov/cgi-bin/popularnames.cgi",year=year.ind[i],top=1000,number=number,style="post")
    
    # read the HTML output into an R table
    nametable[[i]] <- cbind(readHTMLTable(raw,which=3)[-1001,],"Year"=rep(year.ind[i],1000))
  
    # keep a vector with just the female names for creating results matrix next
    if(female==TRUE){
      names<-c(names,as.character(nametable[[i]]$"Female name"))
    }
    if(female==FALSE){
      names<-c(names,as.character(nametable[[i]]$"Male name"))
    }
  }
  names<-names[-1]

  # unique names from all of the years you looked at
  unique.names<-unique(names)

  # create results matrix, rows are the unique names from all years, columns are the years #
  names.mat<-matrix(nrow=length(unique.names),ncol=length(year.ind))
  rownames(names.mat)<-unique.names 
  nms<-rep(NA,length(year.ind))
  for(i in 1:length(year.ind)){
    nms[i]<-as.character(year.ind[i])
  }
  colnames(names.mat)<-nms
  ranks.mat<-names.mat

  
  for(i in 1:length(year.ind)){
    if(female==TRUE){
      temp.names<-as.character(nametable[[i]]$"Female name")
      # need to replace commas and extract numbers#
      if(number=="n"){
        temp.nums<-as.numeric(gsub(",","",as.character(nametable[[i]]$"Number of females")))
      }
      if(number=="p"){
        temp.nums<-as.numeric(gsub("%","",as.character(nametable[[i]]$"Percent oftotal females")))
      }
    }
    if(female==FALSE){
      temp.names<-as.character(nametable[[i]]$"Male name")
      # need to replace commas and extract numbers#
      if(number=="n"){
        temp.nums<-as.numeric(gsub(",","",as.character(nametable[[i]]$"Number of males")))
      }
      if(number=="p"){
        temp.nums<-as.numeric(gsub("%","",as.character(nametable[[i]]$"Percent oftotal males")))
      }
    }
	temp.ranks<-1:1000
    
    # match matrix column to the year index #
    ind<-match(unique.names,temp.names)
    
    # go thru every unique name and fill in that year's data for that name #
    for(j in 1:length(ind)){
      if(!is.na(ind[j])){
        names.mat[j,i]<-temp.nums[ind[j]]
		ranks.mat[j,i]<-temp.ranks[ind[j]]
      }
    }
  }
  res<-list(names.mat,ranks.mat)
  return(res)
}