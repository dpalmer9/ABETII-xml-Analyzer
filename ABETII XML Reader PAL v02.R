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
filelist <- tk_choose.files(caption="please select all xml files to analyze")


## For Loop (Each File) ##
for(i in 1:length(file_list)){}

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
current_xml_session_final <- current_xml_session_frame

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

