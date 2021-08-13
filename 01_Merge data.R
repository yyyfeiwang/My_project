library(tidyverse)
library(Hmisc)
library(climdex.pcic) # Multithreading

setwd("C://Users/yifei/Desktop/2021/")

df_file_process <- function(file, set = "A") {
    # Read single file data
    df_file <- read.csv(
        paste0("train/training_set", set, "/", file), 
        stringsAsFactors = FALSE, sep = "|", na.strings = "NaN", 
        colClasses = rep("numeric", 41)
    ) %>% mutate(person = unlist(str_split(file, pattern = "[.]"))[1], set = set)
    return(df_file)
}

# Get all the data of training_setA

files_A <- dir("train/training_setA/")
# df_file <- df_file_process(files[1])
system.time({
    df_file_set_oA <- lapply(1:length(files_A), function(i){
        df_file <- df_file_process(files_A[i], set = "A")
    })
    df_file_set_oA <- do.call("rbind", df_file_set_oA)
})

# Get all the data of training_setB

files_B <- dir("train/training_setB/")
# df_file <- df_file_process(files[1])
system.time({
    df_file_set_oB <- lapply(1:length(files_B), function(i){
        df_file <- df_file_process(files_B[i], set = "B")
    })
    df_file_set_oB <- do.call("rbind", df_file_set_oB)
})

save(df_file_set_oA, df_file_set_oB, file = "training_set_o.RData")
