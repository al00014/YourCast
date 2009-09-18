# plot.yourcast function
# Author: Jon Bischof <jbischof@fas.harvard.edu>


############################################
# user prompt function
user.option <- function(i,uniquecsid,cntryname,G.names) {
       ANSWER <- readline(paste("Plot for '",
          ifelse(is.null(cntryname),
          paste("geo unit",uniquecsid[i+1]),
          paste(G.names[grep(uniquecsid[i+1],
                          G.names[,1]),2])),
          "' (plot ",i+1," of ",length(uniquecsid),
          ") next.
Type 'y' to continue or 'n' to quit: ",
                          sep=""))
       if (substr(ANSWER, 1, 1) == "n")
         stop("Function terminated by user.")
     }

####################################################
# time series plotter
timeplot <- function(x,geoname,dvlabel,cntryname,
                     time.insamp.obs,time.insamp.predict,time.xlab,
                     time.ylab) {

 # Set up labels 
 if(is.null(time.xlab)) {time.xlab <- "Time"}
 if(is.null(time.ylab)) {time.ylab <- "Data and Forecasts"}

 # Set up axes
  ylim <- c(min(min(na.omit(geoname$yhat)),
            min(na.omit(geoname$y))),
            max(max(na.omit(geoname$yhat)),
                max(na.omit(geoname$y))))
 
  # If not time.insamp.predict, remove the in sample observations
  # from the 'yhat' dataframe
  if(!time.insamp.predict) {sample.frame <- x$aux$sample.frame;
      geoname$yhat[c(1:grep(sample.frame[2],
                         rownames(geoname$yhat))),] <- NA}
 
 # If not time.insamp.obs, only consider 'yhat' values when setting
 # up plots
 if(!time.insamp.obs) {
   ylim <- c(min(na.omit(geoname$yhat)),max(na.omit(geoname$yhat)))
 }
 
  # plot the predicted values
  suppressWarnings(
  matplot(x=as.integer(rownames(geoname$yhat)),y=geoname$yhat,
          type="l",lty=1,col=rainbow(ncol(geoname$yhat)),
          xlab=time.xlab,ylab=time.ylab,
          xlim=c(as.integer(rownames(geoname$yhat))[1],
            as.integer(rownames(geoname$yhat))
            [nrow(geoname$yhat)]+
            0.075*(max(as.integer(rownames(geoname$yhat)))-
                  min(as.integer(rownames(geoname$yhat))))),
          ylim=ylim,
          main=paste(if(!is.null(dvlabel)){dvlabel},
            if(!is.null(dvlabel) & !is.null(cntryname)){", "},
            if(!is.null(cntryname)){cntryname},sep=""))
 )
  
  # only plot the observed values if time.insamp.obs==T
  if(time.insamp.obs) {
  matlines(x=as.integer(rownames(geoname$y)),y=geoname$y,
           type="l",lty=2,col=rainbow(ncol(geoname$y)))
}

  # plotting labels on predicted lines
  yp <- unlist(geoname$yhat[nrow(geoname$yhat),][order(unlist(geoname$yhat[nrow(geoname$yhat),]))])
  xp <- rep(c(as.integer(rownames(geoname$yhat))[nrow(geoname$yhat)],
            as.integer(rownames(geoname$yhat))[nrow(geoname$yhat)]+0.05*(max(as.integer(rownames(geoname$yhat)))-min(as.integer(rownames(geoname$yhat))))),
                ncol(geoname$yhat)/2)
  # If the age group labels are numeric, use numeric version to get
  # rid of leading zeroes 
  if(!any(is.na(try(as.numeric(colnames(geoname$yhat)))))) {
    labels <- as.numeric(colnames(geoname$yhat))}
  else{labels <- colnames(geoname$yhat)}
  text(x=xp,y=yp,labels=labels[order(unlist(geoname$yhat[nrow(geoname$yhat),]))],
       pos=4,cex=0.85,col=rainbow(ncol(geoname$yhat))[order(unlist(geoname$yhat[nrow(geoname$yhat),]))])
}


####################################################
# age profile plotter
ageplot <- function(x,geoname,dvlabel,cntryname,
                    age.insamp.predict,age.xlab,age.ylab) {

 # Set up labels 
 if(is.null(age.xlab)) {age.xlab <- "Age"}
 if(is.null(age.ylab)) {age.ylab <- "Forecasts"}

 
 # If not age.insamp.predict, remove the in sample observations
 # from the 'yhat' dataframe and adjust colors and axes
 col <- rainbow(nrow(geoname$yhat))
 ylim <- c(min(geoname$yhat),max(geoname$yhat)+
            abs(0.05*max(geoname$yhat)))
 yp <- rep(max(geoname$yhat)+abs(0.025*max(geoname$yhat)),
           nrow(geoname$yhat))
  if(!age.insamp.predict) {sample.frame <-
                             x$aux$sample.frame;
      geoname$yhat[c(1:grep(sample.frame[2],
                         rownames(geoname$yhat))),] <- NA;
      col <- col[grep(sample.frame[3],rownames(geoname$yhat)):
                      nrow(geoname$yhat)];
      ylim <- c(min(min(na.omit(geoname$yhat)),
            min(na.omit(geoname$y))),
            max(max(na.omit(geoname$yhat)),
                max(na.omit(geoname$y)))+
            abs(0.05*max(max(na.omit(geoname$yhat)),
                max(na.omit(geoname$y))))
                );
      yp <- rep(max(na.omit(geoname$yhat),na.omit(geoname$y))+
                abs(0.025*max(na.omit(geoname$yhat),
                              na.omit(geoname$y))),
           nrow(geoname$yhat))
      }

 # plot predicted values
 suppressWarnings(
 matplot(x=as.integer(colnames(geoname$yhat)),
         y=t(geoname$yhat),type="l",lty=1,
         col=col,xlab=age.xlab,ylab=age.ylab,
        ylim=ylim,
         main=paste(if(!is.null(dvlabel)){dvlabel},
            if(!is.null(dvlabel) & !is.null(cntryname)){", "},
            if(!is.null(cntryname)){cntryname},sep=""))
                  )

  # plot legend
 xp <- seq(as.integer(colnames(geoname$yhat))[2],
          as.integer(colnames(geoname$yhat))
          [round(ncol(geoname$yhat)/3)],
          length=nrow(geoname$yhat))
 yp <- yp
 points(x=xp,y=yp,col=rainbow(nrow(geoname$yhat)))
 text(x=xp[1],y=yp[1],labels=rownames(geoname$yhat)[1],pos=2)
 text(x=xp[length(xp)],y=yp[length(yp)],
      labels=rownames(geoname$yhat)[nrow(geoname$yhat)],pos=4)
 
 # if no predicted values in sample, plot observed values
 if(!age.insamp.predict) {
  suppressWarnings(
 matlines(x=as.integer(colnames(geoname$y)),
         y=t(geoname$y),type="l",lty=1,
         col=rainbow(nrow(geoname$yhat))[1:nrow(geoname$y)])
                   )
}
 
 
}


####################################################
# 3D plotter
threedimplot <- function(x,geoname,dvlabel,cntryname,
                         threedim.insamp.predict,
                         screen=list(z=-40, x=-60, y=0),
                         threedim.xlab,threedim.ylab,
                         threedim.zlab) {

  # Set up labels 
  if(is.null(threedim.xlab)) {threedim.xlab <- "Year"}
  if(is.null(threedim.ylab)) {threedim.ylab <- "Age"}
  if(is.null(threedim.zlab)) {threedim.zlab <- "Forecasts"}

  if(threedim.insamp.predict) {
  # set up special data matrix for wireframe
  data <- expand.grid(x = as.integer(rownames(geoname$yhat)),
              y = as.integer(colnames(geoname$yhat)))
  data$z <- unlist(geoname$yhat)
}

  if(!threedim.insamp.predict) {
  sample.frame <- x$aux$sample.frame
  npredict <- sample.frame[4]-sample.frame[3]+1
  nobs <- sample.frame[2]-as.integer(rownames(geoname$yhat))[1]+1
  predictrows <- grep(sample.frame[3],rownames(geoname$yhat)):
                      nrow(geoname$yhat)
  data2 <- expand.grid(x=as.integer(rownames(geoname$yhat)[(nobs+1):nrow(geoname$yhat)]),
                       y=as.integer(colnames(geoname$y)))
  data2$z <- unlist(geoname$yhat[(nobs+1):nrow(geoname$yhat),])
  data1 <- expand.grid(x=as.integer(rownames(geoname$y)
                         [1:nobs]),
                       y=as.integer(colnames(geoname$y)))
  data1$z <- unlist(geoname$y[1:nobs,])
  data <- na.omit(rbind(data1,data2))
  data <- data[order(data$y),]
                     }

  # create lattice object
  out <- wireframe(z ~ x * y, data=data, xlab=threedim.xlab,
                   ylab=threedim.ylab,zlab=threedim.zlab,
                   shade=TRUE,scales=list(arrows=F),
                   screen = screen,
                   main=paste(if(!is.null(dvlabel)){dvlabel},
                     if(!is.null(dvlabel) & !is.null(cntryname)){", "},
                     if(!is.null(cntryname)){cntryname},sep=""))
  return(out)
}

####################################################
# main function
plot.yourcast <- function(x,dpath=getwd(),dvlabel=NULL,family="agetime",
                          time.insamp.obs=TRUE,time.insamp.predict=TRUE,
                          age.insamp.predict=TRUE,
                          threedim.insamp.predict=TRUE,
                          age.xlab=NULL,age.ylab=NULL,
                          time.xlab=NULL,time.ylab=NULL,
                          threedim.xlab=NULL,threedim.ylab=NULL,
                          threedim.zlab=NULL,screen=list(z=-40, x=-60, y=0),
                          print="device",
                          ...) {

# loading aux files from 'yourcast' object

G.names <- NULL
if(!is.null(x$aux$G.names) && length(x$aux) > 0)
                                          {G.names <- x$aux$G.names}
index.code <- x$aux$index.code

# establish how many "g", "a", "t", and "-" used in index.code
indexcheck <- unlist(strsplit(index.code,split=""))
g <- length(grep("g",indexcheck))
a <- length(grep("a",indexcheck))
t <- length(grep("t",indexcheck))
dash <- length(grep("-",indexcheck))
  
# looking for groups of geo areas: create a list of the unique
# geo areas
geolist <- vector("list",length(names(x$yhat)))
  geolist <- sapply(names(x$yhat),substr,1,g)
  uniquecsid <- unique(geolist)

# create data matrices for 'y' and 'yhat' for each geo area
# matrix is of rank 'T' by 'A'
# each geo area gets a list with the 'y' and 'yhat' matrices
# as elements
geonames <- c()
for(i in 1:length(uniquecsid)) {
# grab all the cross sections from geo area i and throw them
# into a data.frame
allcs <- as.data.frame(x$yhat[grep(paste("^",uniquecsid[i],
                                           sep=""),
                        names(x$yhat))])

# separate out the 'y' from the 'yhat' and put it in a new
# data.frame
y <- allcs[seq(1,length(allcs)-1,by=2)]

# making the colnames of 'y' the ages
colnames(y) <- sapply(names(x$yhat)
                      [grep(paste("^",uniquecsid[i],
                                      sep=""),
                         names(x$yhat))],substr,g+1,g+a)

# separate out the 'yhat' from the 'y' and put it in a new
# data.frame
yhat <- allcs[seq(2,length(allcs),by=2)]
colnames(yhat) <- colnames(y)

# for every geo area, make a list of the 'y' and 'yhat' matrices
geolist <- list(y=y,yhat=yhat)

# label the list by geo area
assign(paste("geo",uniquecsid[i],sep=""),geolist)
geonames[i] <- paste("geo",uniquecsid[i],sep="")
rm(allcs,y,yhat,geolist)
}


# use 'y' and 'yhat' matrices to print plot requested by user
# pass 'par' settings specified by user

# age profile plot
if(family=="age") {

if(print=="device") {
for(i in 1:length(geonames)) {
    cntryname <- NULL
    if(!is.null(G.names)) {
       cntryname <- G.names[grep(uniquecsid[i],
                          G.names[,1]),2]}
    par(...)
    ageplot(x,get(geonames[i]),dvlabel,cntryname,
            age.insamp.predict,age.xlab,age.ylab)
    if(i != length(geonames)) {
    user.option(i,uniquecsid,cntryname,G.names)}
  }
}
  
else if(print=="pdf") {
  for(i in 1:length(geonames)) {
    cntryname <- NULL
    if(!is.null(G.names)) {
       cntryname <- G.names[grep(uniquecsid[i],
                          G.names[,1]),2]}
    pdf(file=paste(dpath,"/","ageplot",uniquecsid[i],
          ".pdf",sep=""))
    par(...)
    ageplot(x,get(geonames[i]),dvlabel,cntryname,
            age.insamp.predict,age.xlab,age.ylab)
    dev.off()
  }  
}
}

# time series plot
else if(family=="time") {

if(print=="device") {
for(i in 1:length(geonames)) {
    cntryname <- NULL
    if(!is.null(G.names)) {
       cntryname <- G.names[grep(uniquecsid[i],
                          G.names[,1]),2]}
    #get(getOption("device"))()
    par(...)
    timeplot(x,get(geonames[i]),dvlabel,cntryname,
             time.insamp.obs,time.insamp.predict,time.xlab,time.ylab)
    if(i != length(geonames)) {
    user.option(i,uniquecsid,cntryname,G.names)}
  }
}

else if(print=="pdf") {  
  for(i in 1:length(geonames)) {
    cntryname <- NULL
    if(!is.null(G.names)) {
       cntryname <- G.names[grep(uniquecsid[i],
                          G.names[,1]),2]}
    pdf(file=paste(dpath,"/","timeplot",uniquecsid[i],
          ".pdf",sep=""))
    par(...)
    timeplot(x,get(geonames[i]),dvlabel,cntryname,
             time.insamp.obs,time.insamp.predict,time.xlab,time.ylab)
    dev.off()
  }
}
}

# both age and time plots
else if(family=="agetime") {

if(print=="device") {
for(i in 1:length(geonames)) {
    cntryname <- NULL
    if(!is.null(G.names)) {
       cntryname <- G.names[grep(uniquecsid[i],
                          G.names[,1]),2]}
    dev.new(height=7,width=14)
    par(mfrow=c(1,2),...)
    timeplot(x,get(geonames[i]),dvlabel,cntryname,
             time.insamp.obs,time.insamp.predict,time.xlab,time.ylab)
    ageplot(x,get(geonames[i]),dvlabel,cntryname,
            age.insamp.predict,age.xlab,age.ylab)
    if(i != length(geonames)) {
    user.option(i,uniquecsid,cntryname,G.names)}
  }
}

else if(print=="pdf") {  
  for(i in 1:length(geonames)) {
    cntryname <- NULL
    if(!is.null(G.names)) {
       cntryname <- G.names[grep(uniquecsid[i],
                          G.names[,1]),2]}
    pdf(file=paste(dpath,"/","agetimeplot",uniquecsid[i],
          ".pdf",sep=""),width=14)
    par(mfrow=c(1,2),...)
    timeplot(x,get(geonames[i]),dvlabel,cntryname,
             time.insamp.obs,time.insamp.predict,time.xlab,time.ylab)
    ageplot(x,get(geonames[i]),dvlabel,cntryname,
            age.insamp.predict,age.xlab,age.ylab)
    dev.off()
  }
}
}

# 3D plot
else if(family=="threedim") {

if(print=="device") {
for(i in 1:length(geonames)) {
    cntryname <- NULL
    if(!is.null(G.names)) {
       cntryname <- G.names[grep(uniquecsid[i],
                          G.names[,1]),2]}
    out.plot <- threedimplot(x,get(geonames[i]),dvlabel,
                             cntryname,threedim.insamp.predict,
                             screen=screen,
                             threedim.xlab,threedim.ylab,
                             threedim.zlab)
    par(...)
    plot(out.plot)
    if(i != length(geonames)) {
    user.option(i,uniquecsid,cntryname,G.names)}
  }
}

else if(print=="pdf") {
  for(i in 1:length(geonames)) {
    cntryname <- NULL
    if(!is.null(G.names)) {
       cntryname <- G.names[grep(uniquecsid[i],
                          G.names[,1]),2]}
    out.plot <- threedimplot(x,get(geonames[i]),dvlabel,
                             cntryname,threedim.insamp.predict,
                             screen=screen,
                             threedim.xlab,threedim.ylab,
                             threedim.zlab)
   pdf(file=paste(dpath,"/","threedimplot",uniquecsid[i],
          ".pdf",sep=""))
    par(...)
    plot(out.plot)
    dev.off()
  }
}
}

else{stop(paste("Family type '",family,"' not recognized.",
                sep=""))}
}
