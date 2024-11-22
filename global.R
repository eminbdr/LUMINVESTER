library(rjson)
library(jsonlite)

library(qtl2)
library(readr)
library(stringi)
local_path <- "clean1.csv"
df <-read.csv(local_path,encoding = "utf-8",sep=";")

colnames(df)[3:length(df)] <- gsub("\\.", "-", gsub("X", "", colnames(df)[3:length(df)]))
sector <- unique(df[["sector"]])
province <- unique(df[["province"]])
years <- as.Date(paste(colnames(df)[3:length(df)],"-01",sep=""),format = "%Y-%m-%d")

lang <- fromJSON(paste(readLines("en.json", warn = FALSE), collapse = ""))

