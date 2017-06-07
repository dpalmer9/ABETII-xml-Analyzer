###########################################################################################################
#                                                                                                         #
#                                                                                                         #
#                             ABET II XML - Analyzer                                                      #
#                               Version: .01a                                                             #
#                                                                                                         #
#                            This program will handle PAL .xml data                                       #
#                                                                                                         #
#                           Written by: Daniel Palmer, Phd                                                #
###########################################################################################################

## Library ##
library(tidyverse)
library(tcltk)
library(tcltk2)
library(XML)

## Import .xml data ##
#filelist <- tk_choose.files(caption="please select all xml files to analyze")


## For Loop (Each File) ##
for(i in 1:length(file_list)){}

Final_Data_File <- data.frame()
## Read XML Data ##
#current_xml_raw <- xmlParse(file_list[i])
current_xml_raw <- xmlRoot(xmlTreeParse("TCNLAB1_Sys5PALAPP4M_Mouse dPAL 1 v3_214.xml", ignoreBlanks=FALSE, trim=FALSE))
current_xml_split <- xmlSApply(current_xml_raw,function(x) xmlApply(x,xmlValue))

testframe <- as.data.frame(topxml$SessionInformation)
textmatrix <- matrix(topxml$SessionInformation)
testmatrix2 <- unlist(paste(textmatrix,"\n"))
test <- data.frame(strsplit(as.character(textmatrix),"\n"))
test2 <- test[-c(1,4),]
test3 <- data.frame(t(test2))
rownames(test3) <- NULL
colnames(test3) <- c("Name","Value")
seqtest <- seq(2,nrow(test3),2)
test4 <- test3[seqtest, ]

## Read Session XML Data ##
current_xml_session_raw <- matrix(current_xml_split$SessionInformation)
current_xml_session_split <- data.frame(strsplit(as.character(current_xml_session_raw),"\n"))
current_xml_session_split_2 <- current_xml_session_split[-c(1,4),]
current_xml_session_split_3 <- data.frame(t(current_xml_session_split_2))
rownames(current_xml_session_split_3) <- NULL
colnames(current_xml_session_split_3) <- c("Name","Value")
blankseq <- seq(2,nrow(current_xml_session_split_3),2)
current_xml_session_factor <- current_xml_session_split_3[blankseq, ]
current_xml_session_frame <- data.frame(lapply(current_xml_session_factor, as.character), stringsAsFactors = FALSE)
current_xml_session_fix <- current_xml_session_frame
for(i in 1:nrow(current_xml_session_frame)){
  current_xml_session_frame[i, ] <- gsub("[\n]","",current_xml_session_frame[i, ])
}
xml_session_final <- current_xml_session_fix

## Read Data XML Data ##
current_xml_data_raw <- matrix(current_xml_split$MarkerData)

current_xml_data_measure <- data.frame()
current_xml_data_evaluation <- data.frame()
for(x in 1:nrow(current_xml_data_raw)){
  if(isTRUE(grepl("Measure",current_xml_data_raw[x]))){
    if(is_empty(current_xml_data_measure)){
      current_xml_data_measure <- current_xml_data_raw[x]
    }
    else{
      current_xml_data_measure <- rbind(current_xml_data_measure,current_xml_data_raw[x])
    }
  }
}

current_xml_data_measure_2 <- data.frame(strsplit(as.character(current_xml_data_measure),"Measure") , stringsAsFactors = FALSE)
current_xml_data_measure_3 <- data.frame(t(current_xml_data_measure_2))
rownames(current_xml_data_measure_3) <- NULL
colnames(current_xml_data_measure_3) <- c("Measure","Value")
current_xml_measure_frame <- data.frame(lapply(current_xml_data_measure_3, as.character), stringsAsFactors = FALSE)

xml_latency_headers <- data.frame(current_xml_measure_frame[ ,1])
xml_latency_headers <- data.frame(lapply(xml_latency_headers, as.character), stringsAsFactors = FALSE)
for(i in 1:nrow(xml_latency_headers)){
  xml_latency_headers[i, ] <- gsub("[\n]","",xml_latency_headers[i, ])
}

xml_latency_data <- data.frame(as.character(current_xml_measure_frame[ ,2]))
xml_latency_data_expanded <- data.frame(lapply(xml_latency_data, as.character), stringsAsFactors = FALSE)
colnames(xml_latency_data_expanded) = c("Latency")
xml_latency_data_expanded2 <- xml_latency_data_expanded
for(i in 1:nrow(xml_latency_data_expanded2)){
  xml_latency_data_expanded2[i, ] <- gsub("[\n]", "", xml_latency_data_expanded2[i, ])
}
xml_latency_data_expanded3 <- data.frame(separate(data = xml_latency_data_expanded, col = "Latency", into = c("1","2","3","4"), sep = "[\n]"))
xml_latency_data_cols <- xml_latency_data_expanded3[, 2:3]
colnames(xml_latency_data_cols) <- c("Start", "Total")
xml_latency_data_final <- data.frame(apply(xml_latency_data_cols, c(1,2), as.integer))
xml_latency_data_final2 <- cbind(xml_latency_headers[, 1], xml_latency_data_final)
colnames(xml_latency_data_final2) <- c("Measure", "Start Time", "End Time")
xml_latency_final3 <- data.frame(cbind(xml_latency_data_final2[ ,1] ,apply(xml_latency_data_final2[ ,2:3], c(1,2), function(x) x/1000000)))
xml_latency_final3[ ,1] <- xml_latency_data_final2[ ,1]
xml_latency_numbered <- xml_latency_final3
xml_measure_list <- as.vector(unique(xml_latency_numbered[ ,1]))
sequence_list <- vector()
measures_count_total <- count(xml_latency_numbered, vars = V1)
for(i in 1:length(xml_measure_list)){
  measure_count <- measures_count_total[which(measures_count_total$vars == xml_measure_list[i]) , 2]
  measure_seq <- seq.int(from = 1,to = as.integer(measure_count),by = 1)
  sequence_list <- c(sequence_list, measure_seq)

}
xml_latency_numbered$order <- sequence_list
xml_latency_numbered$measure <- paste(xml_latency_numbered$V1,xml_latency_numbered$order,sep = ".")
xml_latency_final3[ ,1] <- xml_latency_numbered$measure
for(x in 1:nrow(current_xml_data_raw)){
  if(isTRUE(grepl("Evaluation",current_xml_data_raw[x]))){
    if(is_empty(current_xml_data_evaluation)){
      current_xml_data_evaluation <- current_xml_data_raw[x]
    }
    else{
      current_xml_data_evaluation <- rbind(current_xml_data_evaluation,current_xml_data_raw[x])
    }
  }
}

current_xml_data_evaluation_2 <- data.frame(strsplit(as.character(current_xml_data_evaluation),"Evaluation") , stringsAsFactors = FALSE)
current_xml_data_evaluation_3 <- data.frame(t(current_xml_data_evaluation_2))
rownames(current_xml_data_evaluation_3) <- NULL
colnames(current_xml_data_evaluation_3) <- c("Measure","Value")
current_xml_evaluation_frame <- data.frame(lapply(current_xml_data_evaluation_3, as.character), stringsAsFactors = FALSE)
current_xml_evaluation_fix <- current_xml_evaluation_frame
for(i in 1:nrow(current_xml_evaluation_fix)){
  current_xml_evaluation_fix[i, ] <- gsub("[\n]","",current_xml_evaluation_fix[i, ])
}
xml_evaluation_data <- data.frame(apply(current_xml_evaluation_fix, c(1,2),as.integer))
xml_evaluation_data[ ,1] <- current_xml_evaluation_fix[ ,1]


## Generate Single Session Data ##
trial_session <- xml_session_final
trial_evaluation <- xml_evaluation_data
trial_measure <- xml_latency_final3

trial_session_t <- t(trial_session)
trial_evaluation_t <- t(trial_evaluation)
trial_measure_t <- t(trial_measure)

colnames(trial_session_t) <- trial_session_t[1, ]


colnames(trial_evaluation_t) <- trial_evaluation_t[1, ]

colnames(trial_measure_t) <- trial_measure_t[1, ]

trial_data <- data.frame(trial_session_t,trial_evaluation_t,trial_measure_t[c(1,3), ], check.names = FALSE)
trial_data <- trial_data[2, ]

if(nrow(Final_Data_File) == 0){
  Final_Data_File <- trial_data
}else{
  Final_Data_File <- rbind(Final_Data_File,trial_data, make.row.names = TRUE)
}

