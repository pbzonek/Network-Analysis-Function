#-------------------------------------------------------------#
#-------------------------------------------------------------#
###
###Walk-through example for network functions
###
#-------------------------------------------------------------#
#-------------------------------------------------------------#
###
### Functions built by Paul Bzonek to summarize and plot network analysis data
### Functions were made by modifying code from:
#Author:Kim Whoriskey    |   Whoriskey et al. 2019 
#code used to get network movements between receivers  
#https://doi.org/10.1111/2041-210X.13188
#Author:Christopher Chizinski
#code used to plot networks with a ggplot base
#https://chrischizinski.github.io/rstats/igraph-ggplotll/
###
#-------------------------------------------------------------#
#-------------------------------------------------------------#

#####Bring in necessary packages##################################----
#-------------------------------------------------------------# 
library(ggplot2) #To plot network
library(rgdal) #To work with shapefiles
library(sp) #To work with shapefiles
require(vegan) #To run mantel test
source("Function_Network_Analysis_Bzonek.R") #Bring in network function


#####Read-in, and structure data##################################----
#-------------------------------------------------------------# 
#Bring in first data example <- Bull Trout data from Whoriskey et al. 2019 
data1 <- read.csv("exampledata1.csv")
str(data1)
data1$x_plane <- data1$x_plane/100000 
data1$y_plane <- data1$y_plane/100000

#Bring in second data example <- Common Carp movement in Hamilton Harbour 
data2<-read.csv("exampledata2.csv")
str(data2)

    ###Format date, and order data by time 
    data2$date <- as.POSIXct(as.character(data2$date, format="%Y-%m-%d %H:%M:%S"))
    data2 <-data2[order(data2$date),]
    
    
    #Format shapefile
    shp1 <- readOGR(dsn = file.path(                       
      "HamiltonHarbour.shp/Shoreline.shp"), stringsAsFactors = F)
    shp1 <- spTransform(shp1, CRS("+proj=longlat +datum=WGS84")) #Tranform projection

#Bring in simplified 'test' data
data_test <- read.csv("testdata.csv")

#####Use network functions########################################----
#-------------------------------------------------------------# 
###Example 1: The basics###
#-------------------------# 
    #Use function to summarize network
    network1 <- network_summary(data=data1, FishID=data1$FISHID, ReceiverID=data1$Receiver, #Specify where the function should look for the data, fishID, etc.
                                lat=data1$y_plane, long=data1$x_plane)
    
    #This function summarizes network data that can be printed from a named list
    print(network1$receiver.locations) #Summary of receiver details
    print(network1$moves.matrix) #Matrix of fish traffic between recievers
    print(network1$individual.moves) #Track the movement of individual fish between receivers
    print(network1$plot.data) #Data used to make a network plot
    str(network1$plot.data)
    network1$plot.data$from
    network1$plot.data$to
    #Use function to plot the network we just summarized
    network_plot(data=network1, x.axis = "East", y.axis = "North") 

###Example 2: Customize the plot###
#---------------------------------# 
    #Use function to summarize network
    network2 <- network_summary(data=data2, FishID=data2$transmitter, ReceiverID=data2$station, #Specify where the function should look for the data, fishID, etc.
                                lat=data2$lat, long=data2$long)
    print(network2$plot.data)
    str(network2$plot.data)
    
    #plot network
    network_plot(data=network2)        
    network_plot(data=network2, shapefile=shp1) #add shapefile
    network_plot(data=network2, shapefile=shp1, xlim = c(-79.95, -79.775), ylim = c(43.25, 43.325)) #set domain limits
    network_plot(data=network2, shapefile=shp1, xlim = c(-79.95, -79.775), ylim = c(43.25, 43.325), Min.traffic=5) #ignore edges with little traffic between nodes
    network_plot(data=network2, shapefile=shp1, xlim = c(-79.95, -79.775), ylim = c(43.25, 43.325), labels=TRUE)# add labels
    network_plot(data=network2, shapefile=shp1, xlim = c(-79.95, -79.775), ylim = c(43.25, 43.325), colour.gradient.low="grey", colour.gradient.high="purple") #play with colours
    
    #A number of default parameters can be modified within the function. These parameters will fallback to default values if unspecified
    network_plot(data=network2,                         
                 Min.traffic=2, #specify if you want ignore any lines below traffic threshold
                 shapefile=shp1, xlim = c(-79.95, -79.775), ylim = c(43.25, 43.325), #specify shapefile, and potential domain limits
                 colour.gradient.low="grey", colour.gradient.high="purple", #pick your own colour scale
                 line.min=1, line.max=4, #pick your own line weights
                 receiver.shape=22, receiver.size=6, receiver.stroke=4, #customize the look of your nodes
                 plot.title="Network plot with modified parameters", #speficy optional figure title (useful if looping across fish)
                 labels=TRUE, label.size=1.8, label.transparency=0.5, label.nudge=0.00009, #add labels, and specify details
                  ) 
    
    
###Example 3: Working with loops###
#---------------------------------# 
    #Build a loop to make a network plot per fish
    for (i in unique(data2$transmitter)){  #Start a loop that will go fish-by-fish  
      a <- subset(data2, data2$transmitter == i)  #Only look at the data of one fish at a time
      networki <- network_summary(data=a, FishID=a$transmitter, ReceiverID=a$station, lat=a$lat, long=a$long) #Make a network summary for the data of 1 fish
      network_plot(data=networki, shapefile=shp1, plot.title=i, xlim = c(-79.95, -79.775), ylim = c(43.25, 43.325)) #plot the network summaries made for each fish 
    }
    
    #Build a loop to make a network plot per treatment
    for (i in unique(data2$treatment)){  #Start a loop that will go treatment-by-treatment  
      a <- subset(data2, data2$treatment == i)  #Only look at the data of one treatment at a time
      networki <- network_summary(data=a, FishID=a$transmitter, ReceiverID=a$station, lat=a$lat, long=a$long) #Make a network summary for the data of 1 fish
      network_plot(data=networki, shapefile=shp1, plot.title=i, xlim = c(-79.95, -79.775), ylim = c(43.25, 43.325)) #plot the network summaries made for each fish 
    }


    
###Example 4: Mantel Test###
#---------------------------------# 
    ###Use network summaries in mantel test to test for differences between treatments
    
          ###Use function from Whoriskey et al. 2019###---------------------------------------------------------------------------------------------------###
          get_adj_matx = function(dat, all.receivers){
            adj.matx = as.data.frame(matrix(0, nrow=length(all.receivers), ncol=length(all.receivers), dimnames=list(all.receivers, all.receivers)))
            for(j in 1:length(all.receivers)){
              for(k in j:length(all.receivers)){
                sub1 = dat[(dat$from==all.receivers[j]) & (dat$to==all.receivers[k]),]
                sub2 = dat[(dat$from==all.receivers[k]) & (dat$to==all.receivers[j]),]
                adj.matx[as.character(all.receivers[j]),as.character(all.receivers[k])] = dim(sub1)[1] + dim(sub2)[1]
                adj.matx[as.character(all.receivers[k]),as.character(all.receivers[j])] = dim(sub1)[1] + dim(sub2)[1]
                # index based on receiver name to be sure it's the right spot
              }
            }
            return(adj.matx)
          }
          ###-----------------------------------------------------------------------------------------------------------------------------------------###
    
    ###Example with Whoriskey et al. 2019 data (exampledata1) 
      for (i in unique(data1$Sex)){  #Start a loop that will go treatment-by-treatment
        network <- network_summary(data=data1, FishID=data1$FISHID, ReceiverID=data1$Receiver, lat=data1$x_plane, long=data1$y_plane) #Make a network summary for ALL treatments, so the matrix will be comparable
          a <- subset(data1, data1$Sex == i)  #Only look at the data of one treatment at a time
          networki <- network_summary(data=a, FishID=a$FISHID, ReceiverID=a$Receiver, lat=a$x_plane, long=a$y_plane) #Make a network summary for each treatment
          assign(paste0("adj.matrix.", i), get_adj_matx(dat=networki$individual.moves, network$receiver.locations$UniqueID)) #make a matrix for each treatment. The matrix will be named "adj.matrix.(treatment name)
      }
          
      mantel(xdis=adj.matrix.m, ydis=adj.matrix.m, method="pearson", permutations=999) #pearson's correlation comparing the newly made matrices
      mantel(xdis=adj.matrix.m, ydis=adj.matrix.f, method="spearman", permutations=999) #spearman's correlation comparing the newly made matrices
    
    
    ###Example with very simple 'test' data   
    
      for (i in unique(data_test$treatment)){  #Start a loop that will go treatment-by-treatment
        network <- network_summary(data=data_test, FishID=data_test$Fish, ReceiverID=data_test$Hydrophone, lat=data_test$x, long=data_test$y) #Make a network summary for ALL treatments, so the matrix will be comparable
        
        a <- subset(data_test, data_test$treatment == i)  #Only look at the data of one treatment at a time
        networki <- network_summary(data=a, FishID=a$Fish, ReceiverID=a$Hydrophone, lat=a$x, long=a$y)  #Make a network summary for each treatment
        assign(paste0("adj.matrix.", i), get_adj_matx(dat=networki$individual.moves, network$receiver.locations$UniqueID)) #make a matrix for each treatment. The matrix will be named "adj.matrix.(treatment name)
      }
    
     a<- mantel(xdis=adj.matrix.1, ydis=adj.matrix.2, method="pearson", permutations=999) #pearson's correlation comparing the newly made matrices
    

    

    
    
