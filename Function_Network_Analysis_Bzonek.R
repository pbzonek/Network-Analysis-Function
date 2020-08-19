library(ggplot2)
library(igraph)
library(rgdal)

        # ###Read example dataset
        # tdata2 <-read.csv("tdata2.csv")
        # 
        # #Read shapefile of Hamilton Harbour
        # shp2 <- readOGR(dsn = file.path("Lake_Ontario_2/Lakes.shp"), stringsAsFactors = F)
        # shp2 <- spTransform(shp2, CRS("+proj=longlat +datum=WGS84")) #Tranform projection
#-------------------------------------------------------------#
### Function made by Paul Bzonek with modifications from Whoriskey et al. 2019 to get the network movements between the UniqueIDs###
#-------------------------------------------------------------#
make_network_analysis <- function(data, FishID, ReceiverID, lat, long, shapefile=NA, labels=FALSE, xlim=NULL, ylim=NULL, Min.traffic=1, ...){
  data$FishID<- as.integer(FishID) #Show function where to find data
  data$ReceiverNames <- data$ReceiverID
  data$ReceiverID<-as.integer(ReceiverID) #Show function where to find data
  data$lat <- as.numeric(lat)
  data$long <- as.numeric(long)
  
  #Look for unique reciever-location pairs. This is important if one reciever has been moved to multiple locations
  data$UniqueID <- as.numeric(as.factor(interaction(data$ReceiverID, data$lat, data$long))) #Combine lat and long values to find unique combinations. Convert text to numbers
  data$UniqueID <- as.integer(as.numeric(as.factor(data$UniqueID))) #Make UniqueID numbers sequential
   
  fishunique <- unique(data$FishID) #build vector of unique fish in dataset
  from <- numeric() #the UniqueID the fish moved FROM
  to <- numeric() #the UniqueID the fish moved TO
  fish <- numeric() #the fish
  
  for(i in 1:length(fishunique)){ #Build loop to track the reciever movements per fish#
    fishsub <- data[data$FishID==fishunique[i],] ##subset data$FishID to the run of detections for one fish
    from <- append(from, rle(fishsub$UniqueID)$values[-length(rle(fishsub$UniqueID)$values)]) #build a list of UniqueIDs, and show all but last, making a 'from' vector
    to <- append(to, rle(fishsub$UniqueID)$values[-1]) #build a list of UniqueIDs, and show all but first, making a 'to' vector
    fish <- append(fish, rep(unique(fishsub$FishID), length(rle(fishsub$UniqueID)$values)-1)) #log the fish id 
  }
  
  ###Inspect componenets of function loop
          # FishID <- tdata2$transmitter #Specify your fishID data #Show function where to find data
          # ReceiverID<- tdata2$station #Specify your ReceiverID data #Show function where to find data
          # lat <- tdata2$lat #Specify your latitude data #Show function where to find data
          # long <- tdata2$long #Specify your longitude data #Show function where to find data
          # data <- as.data.frame(cbind(FishID, ReceiverID, lat, long)) #Inspection dataframe
          # 
          # data$UniqueID <- as.numeric(as.factor(interaction(data$ReceiverID, data$lat, data$long))) #Combine lat and long values to find unique combinations. Convert text to numbers
          # data$UniqueID <- as.integer(as.numeric(as.factor(data$UniqueID))) #Make UniqueID numbers sequential
          # 
          # fishunique <- unique(data$FishID) #Follow function steps
          # fishsub <- subset(data,data$FishID==fishunique[1]) #Follow function steps #pick fish to inspect
          # 
          # values.rle.full<-rle(fishsub$UniqueID)$values #Follow function steps
          # values.from<-rle(fishsub$UniqueID)$values[-length(rle(fishsub$UniqueID)$values)] #Follow function steps
          # values.to<-rle(fishsub$UniqueID)$values[-1] #Follow function steps
          # values.fish <- rep(unique(fishsub$FishID), length(rle(fishsub$UniqueID)$values)-1) #Follow function steps
          # n <-(length(values.rle.full)) #deal with uneven lengths
          # length(values.from) <- n #deal with uneven lengths
          # length(values.to) <- n #deal with uneven lengths
          # length(values.fish) <- n #deal with uneven lengths
          # values.rle.df <- cbind(values.rle.full,values.from,values.to, values.fish) #Display what function loop data looks like
          # # #Inspect matrix data by running line below, and clicking real matrix1 code
          # # matrix1<-as.data.frame(full.network[["moves"]]) #Run function once so that full.network[["moves"]] has been made
          
  #record the data
  individual.moves <- data.frame(from, to, fish) #movements between UniqueIDs
  moves.matrix <- table(individual.moves[,1:2]) #gives the pairwise counts of all fish moving from one UniqueID to another, in matrix form
  moves <- data.frame(moves.matrix) #summarizes the pairwise counts
  moves <- moves[moves$Freq!=0,] #summarizes the pairwise counts
  
  Receiver.locations <- data.frame(table(data[,c('UniqueID','lat','long')])) #Summarize frequency of pings at unique Reciever locations
  Receiver.locations <- Receiver.locations[Receiver.locations$Freq!=0,] #Remove 0s made by non-existenent reciever-lat/long combinations
  Receiver.locations$lat <- as.numeric(as.character(Receiver.locations$lat))
  Receiver.locations$long <- as.numeric(as.character(Receiver.locations$long))
  
  
   ### Plot network in ggplot with modifications from https://chrischizinski.github.io/rstats/igraph-ggplotll/
  #Build dataframe for ggplot     
  matrix1<-as.data.frame(moves)
  #match the matrix data with the 'reciver.locations' data
  matrix1$from.x <- Receiver.locations$long[match(matrix1$from, Receiver.locations$UniqueID)]  
  matrix1$from.y <- Receiver.locations$lat[match(matrix1$from, Receiver.locations$UniqueID)]
  matrix1$to.x <- Receiver.locations$long[match(matrix1$to, Receiver.locations$UniqueID)]
  matrix1$to.y <- Receiver.locations$lat[match(matrix1$to, Receiver.locations$UniqueID)]
  #I expect the function adds ghost connections between the nodes. Specify which connections are worth plotting.
  matrix2<-subset(matrix1, matrix1$Freq>Min.traffic-1)
  
  #Plot data without shapefile
  a<- ggplot()+
    geom_segment(data=matrix2, aes(x=from.x, xend = to.x, y=from.y, yend = to.y, size=Freq), colour='burlywood4') +  #Plot movement lines
    scale_size("line", range = c(.5, 2.5))+  #Set line-size range
    geom_point(data=Receiver.locations, aes(x=long, y=lat, colour=Freq), shape=21, stroke=2, size=4)+  #Add reciever circles, colour by matrix frequency
    scale_color_gradient(low="blue", high="red")+  #Basic colour scale
    coord_map(xlim=xlim, ylim=ylim)+  #Specify limits if shapefile is too big
    ylab('long')+ #y axis label
    xlab('lat')+ # x axis label
    theme_classic()
  
  #If else loop to look for optional shapefile. 
  if(is.na(shapefile)){b <- NULL} else{
    b<-geom_polygon(data = shapefile, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
  }
  
  #If else loop to look for optional labels. 
  if(is.na(labels)){c <- NULL} else{
    c<-geom_label(aes(label=(data$ReceiverNames)))
  }
  
  #print plot with optional shapefile
   print(a + b + c)
   
  return(list(moves=moves, moves.matrix=moves.matrix, individual.moves=individual.moves, Receiver.locations=Receiver.locations, plot.data=matrix2 )) #Store function data in useful structure
  
}
