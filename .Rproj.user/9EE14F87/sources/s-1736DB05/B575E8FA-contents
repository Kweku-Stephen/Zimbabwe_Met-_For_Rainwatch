

library(readxl)

Data <- read_excel(
    "F:/Book1.xlsx"
)


Accra <- read.


a <- matrix(1:30, 5, 6)
ta <- t(a)

#
Date
Mat
Month

data.frame(Date,Month,Mat)



apply(
    Data, margin, ...)


#Got to work on this
#Month_1 %>% 
#    rbind.data.frame(
#        ifelse(ncol(Month_2) < 31, data.frame(Month_2,rep(NA,31-length(Month_2))), 
#               Month_2[,31] <- Month_2[,31])
#    )



#########################################################################################################
library(tidyverse)
library(readxl)

Data <- read_excel(
    "F:/Book1.xlsx" #Path and name of data
)
#Data[,2] <- as.character(Data[,2])
#Start and end years
start_year = 1980
end_year = 2018

#Slicing the data into 12 transposed chunks,based on the 12 Months 
Month_1 <- data.frame(t(Data[Data[,1] == 1, ])) #extracting and transposing all Januarys
Month_2 <- data.frame(t(Data[Data[,1] == 2, ])) #extracting and transposing all Februarys
Month_3 <- data.frame(t(Data[Data[,1] == 3, ])) #extracting and transposing all Marchs
Month_4 <- data.frame(t(Data[Data[,1] == 4, ])) #extracting and transposing all Aprils
Month_5 <- data.frame(t(Data[Data[,1] == 5, ])) #extracting and transposing all Mays
Month_6 <- data.frame(t(Data[Data[,1] == 6, ])) #extracting and transposing all Junes
Month_7 <- data.frame(t(Data[Data[,1] == 7, ])) #extracting and transposing all Julys
Month_8 <- data.frame(t(Data[Data[,1] == 8, ])) #extracting and transposing all Augusts
Month_9 <- data.frame(t(Data[Data[,1] == 9, ])) #extrActing and transposing all Septembers
Month_10 <- data.frame(t(Data[Data[,1] == 10, ])) #extracting and transposing all Octobers
Month_11 <- data.frame(t(Data[Data[,1] == 11, ])) #extracting and transposing all November
Month_12 <- data.frame(t(Data[Data[,1] == 12, ])) #extracting and transposing all Decembers

# Bind the individual Months above rowise
Month_1 %>% 
    rbind.data.frame(
        if (ncol(Month_2) < 31){
            Month_2[,(ncol(Month_2)+1):31]<-NA 
            colnames(Month_2)<- colnames(Month_1)
            data.frame(Month_2) 
        }
    ) %>% 
    rbind.data.frame(
        Month_3
    ) %>% 
    rbind(
        if (ncol(Month_4) < 31){
            Month_4[,(ncol(Month_4)+1):31]<- NA
            colnames(Month_4) <- colnames(Month_1)
            data.frame(Month_4)
        } 
    ) %>% 
    rbind.data.frame(
        Month_5
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_6) < 31){
            Month_6[,(ncol(Month_6)+1):31]<- NA
            colnames(Month_6) <- colnames(Month_1)
            data.frame(Month_6)
        }
    ) %>% 
    rbind.data.frame(
        Month_7
    ) %>% 
    rbind(
       Month_8
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_9) < 31){
            Month_9[,(ncol(Month_9)+1):31] <- NA
            colnames(Month_9) <- colnames(Month_1)
            data.frame(Month_9)
        }
    ) %>% 
    rbind.data.frame(
        Month_10
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_11) < 31) {
            Month_11[,(ncol(Month_11)+1):31] <- NA
            colnames(Month_11) <- colnames(Month_1)
            data.frame(Month_11)
        }
    ) %>% 
    rbind.data.frame(
        Month_12
    ) -> Data_T  

# Removes rownames of the above Bind or Data_T
rownames(Data_T) <- NULL
#Creates a dataframe (Final Result)
data.frame(
    Year=rep(c("Month","Day",start_year:end_year),times = 12),
    Month=rep(1:12, each = length(Data[1,])),
    Data_T
) %>% arrange(Year) %>% filter(Year >= start_year & Year <= end_year)-> Data_TT
    
#To make it more generic, first have to run the ifs for al"tidyverse"l Months and after that we rbind,
#but that would make it quite a lengthy code(relative), so we avoid the ifs for the full stack
#months(months with 31 days)
##############################################################################################




Data_TT[order(Data_TT[,1]), ] -> Data_TTT

data.frame(Year=Data_TTT$Year,Month = 1:12,Data_TTT[,2:length(Data_TTT)]) %>% 
    filter(Year >= 1980 & Year <= 2018)


filter(Data_TT, Year >= 1980 & Year <= 2018)


packages <- c("parallel", "dplyr", "future","microbenchmark","janeaustenr",
              "doFuture","data.table","doMC","caret")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
}
cc <- c(1:5)

ff <- file()
vector(mode = "character", length = 12*41) -> y
(attr(y,"dim") <- c(12,41)) 
data.frame(y) -> y

Month = data.frame(character(12),character(12))
for (i in 1:12){
    print(c("Month","Day",rep(i,39)))
}


matrix(1:6, nrow=6, byrow=T) -> mat
(dim(mat))[1]
dim[1]

data.frame()
Data_T[,length(Data_T)+1] <- rep(
    c(
        "Month","Day",1980:2018
        ), 
    times = 12
    )

?addpath

#########################################################################################

Data_new <- data.frame(t(Data))

nam <- rownames(Data_new)

unname(Data_new, force = F) -> Data_new

Data_new$ID <- nam


NewData <- data.frame(Data_new[1:nrow(Data_new), 1:length(Data_new[Data_new[1,] == 7, ])])
Data_new[1:nrow(Data_new), 32:(32+31)]
#Change all headers to NA


NewData <- Data_new[ ,1: 31] %>% 
    rbind.data.frame(
        Data_new[1:nrow(Data_new), 32:(31+31)]
    ) %>% 
    rbind.data.frame(
        data.frame(Data_new[1:nrow(Data_new), 63:(63+29)],NA)
    )



if (ncol(Month_2 == 28)) {
    print(data.frame(Month_2,X29=NA,X30=NA,X31=NA))
} else {
    print(data.frame(Month_2,X30=NA,X31=NA))
}
    



if (ncol(Month_2) < 31){
    Month_2[,(length(Month_2)+1):31]<-NA 
    print(Month_2) 
}



if(ncol(fd) > 3){
    fd[,3] <- NA
    print(fd)
}

