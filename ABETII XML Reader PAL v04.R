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

### Install Missing Packages & Library ###
list.of.packages <- c("tidyverse","tcltk","tcltk2", "tkrplot", "XML")
for(i in 1:length(list.of.packages)){
  Packagepresent <- require(list.of.packages[i], character.only = TRUE)
  if(Packagepresent == FALSE){
    install.packages(list.of.packages[i], dependencies = TRUE)
    require(list.of.packages[i],character.only = TRUE)
  }
}

## Import .xml data ##

filelist <- tk_choose.files(caption="please select all xml files to analyze")

## For Loop (Each File) ##
for(i in 1:length(file_list)){
  Final_Data_File <- data.frame()
  Final_File_List <- vector()
  ## Read XML Data ##
  #current_xml_raw <- xmlParse(file_list[i])
  current_xml_raw <- xmlRoot(xmlTreeParse("TCNLAB1_Sys5PALAPP4M_Mouse dPAL 1 v3_214.xml", ignoreBlanks=FALSE, trim=FALSE))
  current_xml_split <- xmlSApply(current_xml_raw,function(x) xmlApply(x,xmlValue))
  
  
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
  xml_latency_numbered <- data.frame(xml_latency_final3, stringsAsFactors = FALSE)
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
  
  trial_data <- data.frame(trial_session_t,trial_evaluation_t,trial_measure_t[c(1,3), ], check.names = FALSE, stringsAsFactors = FALSE)
  trial_data <- trial_data[2, ]
  trial_colnames <- colnames(trial_data)
  trial_colnames <- gsub(" ", "", trial_colnames)
  trial_colnames <- lapply(trial_colnames, function(x) make.names(x))
  for(a in 1:length(trial_colnames)){
    num_list = c("0","1","2","3","4","5","6","7","8","9")
    for(b in 1:length(num_list)){
      if(isTRUE(startsWith(trial_colnames[a],num_list[b]))){
        current_name <- trial_colnames[a]
        fixed_name <- paste("Data_",current_name,sep = "")
        trial_colnames[a] <- fixed_name
      }
    }
  }
  for(a in 1:length(trial_colnames)){
    bad_symbols = c("/", "-")
    for(b in 1:length(bad_symbols)){
      if(isTRUE(grepl(bad_symbols[b],trial_colnames[a]))){
        trial_colnames[a] <- gsub(bad_symbols[b],"_",trial_colnames[a])
      }
    }
  }
  
  for(a in 1:length(trial_colnames)){
    bad_bracket = c("[(]", "[)]", "[%]")
    for(b in 1:length(bad_bracket)){
      if(isTRUE(grepl(bad_bracket[b],trial_colnames[a]))){
        trial_colnames[a] <- gsub(bad_bracket[b],"",trial_colnames[a])
      }
    }
  }
  colnames(trial_data) <- trial_colnames
  assign(paste("trial_data",".",i, sep = ""), trial_data)
  Final_File_List <- c(Final_File_List, paste("trial_data",".",i, sep = ""))

  }

## Assign Final Column Sizing ##
measure_table <- as.data.frame(matrix(nrow = length(xml_measure_list), ncol = 2))
measure_table[ ,1] <- xml_measure_list
measure_table[ ,1] <- gsub(" ", "", measure_table[ ,1])
for(a in 1:nrow(measure_table)){
  measure_length_list <- vector()
  measure_min_list <- vector()
  for(b in 1:length(Final_File_List)){
    measure_length <- length(grep(measure_table[a,1],colnames(eval(parse(text = Final_File_List[b]))), value = FALSE))
    measure_length_list <- c(measure_length_list, measure_length)
  }
  measure_table[a,2] <- max(measure_length_list)
}
measure_min <- min(grep(measure_table[1,1],colnames(eval(parse(text = Final_File_List[1]))), value = FALSE))
measure_listing <- vector()
for(a in 1:nrow(measure_table)){
  current_seq <- seq(from = 1, to = measure_table[a,2], by = 1)
  current_seq <- lapply(current_seq, function(x) paste(measure_table[a,1]," .",x,sep = ""))
  measure_listing <- c(measure_listing, current_seq)
  
}
prior_colnames <- colnames(trial_data[ , 1:(measure_min - 1)])
col_list <- c(prior_colnames,measure_listing)
col_list2 <- gsub(" ", "", col_list)
for(a in 1:length(col_list2)){
  num_list = c("0","1","2","3","4","5","6","7","8","9")
  for(b in 1:length(num_list)){
    if(startsWith(col_list2[a],num_list[b])){
    }else{
      current_name <- col_list2[a]
      fixed_name <- paste("Data-",current_name,sep = "")
    }
  }
}

Final_Data_File <- as.data.frame(matrix(nrow = 0, ncol = length(col_list2)), stringsAsFactors = FALSE)
colnames(Final_Data_File) <- col_list2
Final_Data_File <- merge(x = Final_Data_File,y = eval(parse(text = Final_File_List[i])) )
# Final_Data_File <- merge(x = Final_Data_File,y = trial_data)

## GUI Main Window ##
window1 <- tktoplevel()
tktitle(window1) <- "ABET II XML Reader v03"


