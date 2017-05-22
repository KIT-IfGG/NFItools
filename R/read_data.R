read_NFI <- function(datapath=datapath, exdir=datapath, type="tree", country="GER") {
  ### "country-if"
  if(type=="tree"){ 
    print("Unzip bwi2002dt_wzp4.zip")   ### think how to include file names; several years...
    unzip(paste(datapath, "bwi2002dt_wzp4.zip", sep=""), files="bwi2002dt_wzp4.dbf", list=F, exdir=datapath)
    data2002 <- read.dbf(paste(datapath, "bwi2002dt_wzp4.dbf", sep=""))
    head(data2002)
    data2002$BaumID <- paste(data2002$tnr, data2002$enr, data2002$bnr, sep="_")
    data2002$SiteID <- paste(data2002$tnr, data2002$enr, sep="_")
    
    ### Add coordinates
    print("Unzip bwi2002dt_ecken.zip to add coordinates")
    unzip(paste(datapath, "bwi2002dt_ecken.zip", sep=""), files=NULL, list=F, exdir=paste(datapath, "bwi2002dt_ecken", sep=""))
    ### datapath does not work here. Change path.
    ogrListLayers("/home/klara/Datenvorrat/Waldinventuren/BWI2009/bwi2002dt_ecken")
    we <- readOGR("/home/klara/Datenvorrat/Waldinventuren/BWI2009/bwi2002dt_ecken", layer="bwi2002dt_ecken")   
    
    coords <- lapply(we@polygons, function(x) x@labpt)
    coords <- do.call(rbind, coords)
    colnames(coords) <- c("GKrechts", "GKhoch")
    
    coords <- cbind.data.frame(coords, we@data[,c("tnr", "enr")])
    
    m3 <- match(paste(data2002$tnr, data2002$enr), paste(coords$tnr, coords$enr))
    #table(is.na(m3))
    data2002 <- cbind.data.frame(data2002, coords[m3, c("GKrechts", "GKhoch")])
    data2002
  } else {
    ### establiahment  
  }  
  
}

