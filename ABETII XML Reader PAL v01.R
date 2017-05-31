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
current_xml_raw <- xmlParse(file_list[i])
current_xml_list <- xmlToList(current_xml_raw)
current_xml_details_list <- current_xml_list[2]
current_xml_data_list <- current_xml_list[3]
xml_test2 <- xmlRoot(xmlTreeParse("TCNLAB1_Sys5PALAPP4M_Mouse dPAL 1 v3_214.xml"))
topxml <- xmlSApply(xml_test2,function(x) xmlApply(x,xmlValue))
topxml2 <- t(topxml)