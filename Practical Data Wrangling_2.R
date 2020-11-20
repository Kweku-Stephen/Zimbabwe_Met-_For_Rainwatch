

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
#Slicing the data into 12 transposed chunks,thus selecting the individual
#months in the data after you have transposed the entire data
Month_1 <- data.frame(t(Data[Data[,1] == 1, ]))
Month_2 <- data.frame(t(Data[Data[,1] == 2, ])) 
Month_3 <- data.frame(t(Data[Data[,1] == 3, ])) 
Month_4 <- data.frame(t(Data[Data[,1] == 4, ]))
Month_5 <- data.frame(t(Data[Data[,1] == 5, ]))
Month_6 <- data.frame(t(Data[Data[,1] == 6, ])) 
Month_7 <- data.frame(t(Data[Data[,1] == 7, ])) 
Month_8 <- data.frame(t(Data[Data[,1] == 8, ]))
Month_9 <- data.frame(t(Data[Data[,1] == 9, ]))
Month_10 <- data.frame(t(Data[Data[,1] == 10, ])) 
Month_11 <- data.frame(t(Data[Data[,1] == 11, ])) 
Month_12 <- data.frame(t(Data[Data[,1] == 12, ]))


# Bind the individual Months above rowise
Month_1 %>% 
    rbind.data.frame(
        if (ncol(Month_2) == 28) {
            data.frame(Month_2,X29=NA,X30=NA,X31=NA)
        } else {
            data.frame(Month_2,X30=NA,X31=NA)
        }
    ) %>% 
    rbind.data.frame(
        Month_3
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_3) == 30) {
            data.frame(Month_3, X31=NA)
        } 
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_4) == 30) {
            data.frame(Month_4, X31=NA)
        } 
    ) %>% 
    rbind.data.frame(
        Month_5
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_6) == 30) {
            data.frame(Month_6, X31=NA)
        } 
    ) %>% 
    rbind.data.frame(
        Month_7
    ) %>% 
    rbind(
        Month_8
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_9) == 30) {
            data.frame(Month_9, X31=NA)
        }
    ) %>% 
    rbind.data.frame(
        Month_10
    ) %>% 
    rbind.data.frame(
        if (ncol(Month_11) == 30) {
            data.frame(Month_11, X31=NA)
        }
    ) %>% 
    rbind.data.frame(
        Month_12
    ) -> Data_T  

rownames(Data_T) <- NULL
data.frame(
    Year = rep(c("Month","Day",1980:2018), times = 12),
    Month = rep(1:12, each = 41),
    Data_T
) %>% 
    
    
    Data_TT[order(Data_TT[,1]), ] -> Data_TTT

data.frame(Year=Data_TTT$Year,Month = 1:12,Data_TTT[,2:length(Data_TTT)]) %>% 
    filter(Year >= 1980 & Year <= 2018)



filter(Data_TT, Year >= 1980 & Year <= 2018)






vector(mode = "character", length = 12*41) -> y
(attr(y,"dim") <- c(12,41)) 
data.frame(y) -> y

Month = data.frame(character(12),character(12))
for (i in 1:12){
    print(c("Month","Day",rep(i,39)))
}





data.frame()
Data_T[,length(Data_T)+1] <- rep(
    c(
        "Month","Day",1980:2018
    ), 
    times = 12
)



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









