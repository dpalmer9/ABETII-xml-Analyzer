###########################################################################################################
#                                                                                                         #
#                                                                                                         #
#                             ABET II XML - Analyzer                                                      #
#                               Version: .05a                                                             #
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

### List of Transformation Functions ###
table_column_matching <- function(master, adddata){
  master_col_list <- as.vector(colnames(master))
  master_max_row <- nrow(master)
  master_new_row <- length(adddata)
  for(c in 1:master_new_row){
    master <- add_row(master)
  }
  for(a in 1:length(master_col_list)){
    master_col <- which(colnames(master) == master_col_list[a])
    for(b in 1:length(adddata)){
      add_col <- which(colnames(eval(parse(text = adddata[b]))) == master_col_list[a])
      if(isTRUE(add_col != 0)){
        master[(b + master_max_row) , master_col] <- eval(parse(text = adddata[b]))[1,add_col]
      }else if(isTRUE(add_col == 0)){
        master[(b + master_max_row) , master_col] <- NA
      }
    }
  }
  return(master)
}

## Folder Check Function ##

folder_check <- function(){
  folder_selected <- tk_choose.dir()
  files_in_folder <- list.files(path = folder_selected)
  if(length(which(files_in_folder == "Experiment_Data.csv")) != 0){
    load_experiment_data(folder = folder_selected)
  }else if(length(which(files_in_folder == "Experiment_Data.csv")) == 0){
    generate_experiment_data(folder = folder_selected, files = files_in_folder)
  }
}

## For Loop (Each File) ##
generate_experiment_data <- function(folder, files){
  Final_Data_File <- data.frame()
  Final_File_List <- vector()
  for(i in 1:length(files)){
    ## Read XML Data ##
    current_xml_filepath <- paste(folder, files[i], sep = "/")
    current_xml_raw <- xmlRoot(xmlTreeParse(current_xml_filepath, ignoreBlanks=FALSE, trim=FALSE))
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
    for(x in 1:nrow(current_xml_session_frame)){
      current_xml_session_frame[x, ] <- gsub("[\n]","",current_xml_session_frame[x, ])
    }
    xml_session_final <- current_xml_session_fix
    
    ## Read Data XML Data ##
    current_xml_data_raw <- matrix(current_xml_split$MarkerData)
    
    current_xml_data_measure <- data.frame()
    current_xml_data_evaluation <- data.frame()
    
    ## Read Measure Data ##
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
    for(x in 1:nrow(xml_latency_headers)){
      xml_latency_headers[x, ] <- gsub("[\n]","",xml_latency_headers[x, ])
    }
    
    xml_latency_data <- data.frame(as.character(current_xml_measure_frame[ ,2]))
    xml_latency_data_expanded <- data.frame(lapply(xml_latency_data, as.character), stringsAsFactors = FALSE)
    colnames(xml_latency_data_expanded) = c("Latency")
    xml_latency_data_expanded2 <- xml_latency_data_expanded
    for(x in 1:nrow(xml_latency_data_expanded2)){
      xml_latency_data_expanded2[x, ] <- gsub("[\n]", "", xml_latency_data_expanded2[x, ])
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
    for(x in 1:length(xml_measure_list)){
      measure_count <- measures_count_total[which(measures_count_total$vars == xml_measure_list[x]) , 2]
      measure_seq <- seq.int(from = 1,to = as.integer(measure_count),by = 1)
      sequence_list <- c(sequence_list, measure_seq)
      
    }
    xml_latency_numbered$order <- sequence_list
    xml_latency_numbered$measure <- paste(xml_latency_numbered$V1,xml_latency_numbered$order,sep = ".")
    xml_latency_final3[ ,1] <- xml_latency_numbered$measure
    
    ## Read Evaluation Data ##
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
    for(x in 1:nrow(current_xml_evaluation_fix)){
      current_xml_evaluation_fix[x, ] <- gsub("[\n]","",current_xml_evaluation_fix[x, ])
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
    trial_colnames <- make.names(trial_colnames, unique = TRUE)
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
  
  table_column_matching <- function(master, adddata){
    master_col_list <- as.vector(colnames(master))
    master_max_row <- nrow(master)
    master_new_row <- length(adddata)
    for(c in 1:master_new_row){
      master <- add_row(master)
    }
    for(a in 1:length(master_col_list)){
      master_col <- which(colnames(master) == master_col_list[a])
      for(b in 1:length(adddata)){
        add_col <- which(colnames(eval(parse(text = adddata[b]))) == master_col_list[a])
        if(isTRUE(add_col != 0)){
          master[(b + master_max_row) , master_col] <- eval(parse(text = adddata[b]))[1,add_col]
        }else if(isTRUE(add_col == 0)){
          master[(b + master_max_row) , master_col] <- NA
        }
      }
    }
    return(master)
  }
  
  Final_Data_File <- table_column_matching(master = Final_Data_File, adddata = Final_File_List)
  
  
  write.csv(Final_Data_File, file = paste(folder,"Experiment_Data.csv", sep="/" ), col.names = TRUE, na = "")
  
}

## Data Editing Function ##

load_experiment_data <- function(folder){
  ## Read csv ##
  active_data <- read.csv(file = paste(folder,"Experiment_Data.csv",sep = "/"), header = TRUE)
  active_data_matrix <- as.matrix(active_data)
  active_data_matrix <- apply(active_data_matrix, c(1,2), as.character)
  active_data_tcl <- tclArray()
  for(a in 1:nrow(active_data)){
    for(b in 1:ncol(active_data)){
      #active_data_tcl[[a-1,b-1]] <- split(active_data_matrix[a,b], " ")[[1]]
      active_data_tcl[[a-1,b-1]] <- active_data_matrix[a,b][[1]]
      active_data_tcl[[a-1,b-1]] <- gsub("[{]","",active_data_tcl[[a-1,b-1]])
      active_data_tcl[[a-1,b-1]] <- gsub("[}]","",active_data_tcl[[a-1,b-1]])
    }
  }
  ## GUI ##
  loadwindow <- tktoplevel()
  tktitle(loadwindow) <-  "Data Viewer"
  
  loadwindow$env$active_table <- tk2table(loadwindow, variable = active_data_tcl, colwidth = 10, rows = nrow(active_data), 
                                          cols = ncol(active_data), selectmode = "extended", background = "white"
                                          ,titlerows = 1)
  tkgrid(loadwindow$env$active_table)
}

## GUI Main Window ##
window1 <- tktoplevel()
tktitle(window1) <- "ABET II XML Reader v03"

tkgrid(tk2label(window1, text = "ABET II XML Reader"))
tkgrid(tk2label(window1, text = "Created by: Daniel Palmer, PhD"))
tkgrid(tk2label(window1, text = "Daniel.Palmer321@gmail.com"))
tkgrid(tk2label(window1, text = "Select an Experiment Folder"))
window1$env$ButtonFolder <- tk2button(window1, text = "Select Folder", command = folder_check)
tkgrid(window1$env$ButtonFolder)

