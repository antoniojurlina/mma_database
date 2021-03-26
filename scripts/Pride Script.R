#### packages ####

library(rvest)
library(httr)
library(rjson)
library(stringr)
library(dplyr)
library(lubridate)
library(tidyr)
library(here)
library(readr)
library(htmltab)
library(tibble)

path <- here::here()
setwd(path)
main.dir <- getwd()

#### getting a list of all Pride events ####

all_events <- read_html("https://en.wikipedia.org/wiki/List_of_Pride_FC_events")

events_table <- html_table(all_events, fill = TRUE)[[1]]
events_table$`Japanese name` <- NULL
colnames(events_table) <- c("#", "Event", "Date", "Venue", "City", "Attendance")
events_table$Attendance[which(events_table$Attendance == "N/A")] <- NA
events_table$Attendance <- str_remove(events_table$Attendance, ",")
events_table$Attendance <- as.numeric(events_table$Attendance)
events_table$Date <- mdy(events_table$Date)

events_table <- separate(events_table, 
                         col = City, 
                         into = c("City", "State", "Country"), 
                         sep = ", ")

for(i in seq_along(events_table$Country)) {
  if(is.na(events_table$Country[i])) {
    events_table$Country[i] <- events_table$State[i]
    events_table$State[i] <- NA
  }
}

all_links <- all_events %>%
  xml_find_all(xpath = "//a/@href") %>%
  xml_text() %>%
  .[10:292]

event_links <- all_links %>%
  str_detect("Pride") %>%
  which() %>%
  all_links[.]

location_links <- all_links %>%
  str_detect("Pride") %>%
  which()
location_links <- all_links[-location_links]
a <- which(str_detect(location_links, "Arena"))
b <- which(str_detect(location_links, "Hall"))
c <- which(str_detect(location_links, "Center"))
d <- which(str_detect(location_links, "Dome"))
e <- which(str_detect(location_links, "Coliseum"))
f <- which(str_detect(location_links, "Stadium"))
g <- which(str_detect(location_links, "Fukuoka"))
h <- which(str_detect(location_links, "Budokan"))
i <- which(str_detect(location_links, "States"))
j <- which(str_detect(location_links, "Nevada"))
k <- which(str_detect(location_links, "Japan"))
venues <- c(a,b,c,d,e,f,g,h,i,j,k)
location_links <- location_links[-venues]
location_links[c(2,5)] <- "/wiki/Las_Vegas"

#### primary data frame (tibble) ####
Pride <- tibble("1", "2", "3", 4, 5, 6, 7, 8, 9, 10, 11, "12", "13", "14",
                   15, "16", "17", "18", 19, "2017-06-03", "21", "22", "23", "24")

Pride[, 20] <- "July 07, 2019"
Pride[, 20] <- mdy(Pride[, 20])

names(Pride) <- c("W/L", "Fighter1", "Fighter2", "Str1", "Str2", "Td1",         
                     "Td2", "Sub1", "Sub2", "Pass1", "Pass2", "Weight class",
                     "Outcome", "Method", "Round", "Time", "Event", "Promotion",
                     "Attendance","Date", "City", "State", "Country", "Enclosure") 

#### 1997 ####

stats <- events_table %>%
  nrow() %>%
  event_links[.] %>%
  paste0("https://en.wikipedia.org", .) %>%
  read_html()

table <- stats %>%
  html_table(fill = TRUE) %>%
  .[[7]]

table1 <- stats %>%
  html_table(fill = TRUE) %>%
  .[[5]]

names(table) <- table[1, ]
table <- table[-1, ]
table$Notes <- NULL

for(j in 1:nrow(table)) {
  if(table$`Weight class`[j] == "") {
    table$`Weight class`[j] <- "Open Weight"
  }
}

for(k in 1:nrow(table)){
  if(table[k, 3] == "def.") {
    table[k, 3] <- "win"
  }
  if(table[k, 3] == "vs.") {
    table[k, 3] <- "draw"
  }
  if(table[k, 5] == "No Contest"){
    table[k, 3] <- "nc"
  }
}

table <- table[-3, ]

row.names(table) <- 1:nrow(table)

table$Str1 <- NA
table$Str2 <- NA
table$Td1 <- NA
table$Td2 <- NA
table$Sub1 <- NA
table$Sub2 <- NA
table$Pass1 <- NA
table$Pass2 <- NA
table$Outcome <- NA
table$Enclosure <- "Ring"

for(m in 1:nrow(table)) {
  table$Outcome[m] <- str_split_fixed(table$Method[m], " ", n = 2)[1]
  table$Method[m] <- str_split_fixed(table$Method[m], " ", n = 2)[2]
}

for(l in 1:nrow(table)){
  if(table$Method[l] == "Contest") {
    table$Method[l] <- "No Contest"
  }
  if(table$Outcome[l] == "No") {
    table$Outcome[l] <- "No Contest"
  }
}

table$Method <- str_remove_all(table$Method, "\\(")
table$Method <- str_remove_all(table$Method, "\\)")

table$Date <- table1[which(str_detect(table1[, 1], "Date")), 2]
table$Date <- mdy(table$Date)

if(sum((str_detect(table1[, 1], "Attendance"))) != 0) {
  table$Attendance <- table1[which(str_detect(table1[, 1], "Attendance")), 2]
  table$Attendance <- as.numeric(str_remove(table$Attendance, ","))
} else {
  table$Attendance <- NA
}

table$Round <- as.numeric(table$Round)
table$Event <- names(table1)[1]
table$Promotion <- table1[which(str_detect(table1[, 1], "Promotion")), 2]
table$Location <- table1[which(str_detect(table1[, 1], "City")), 2]

location <- str_split_fixed(table$Location, ", ", n = 3)
table$City <- location[, 1]
table$State <- location[, 2]
table$Country <- location[, 3]
table$Location <- NULL

for(n in seq_along(table$Country)) {
  if(is.na(table$Country[n])) {
    table$Country[n] <- table$State[n]
    table$State[n] <- NA
  }
  if(table$Country[n] == "") {
    table$Country[n] <- table$State[n]
    table$State[n] <- NA
  }
}

event <- table
names(event) <- names(Pride)
event$`W/L` <- table[, 3]
event$Fighter1 <- table[, 2]
event$Fighter2 <- table[, 4]
event$Str1 <- table$Str1
event$Str2 <-table$Str2
event$Td1 <- table$Td1
event$Td2 <- table$Td2
event$Sub1 <- table$Sub1
event$Sub2 <- table$Sub2
event$Pass1 <- table$Pass1
event$Pass2 <- table$Pass2
event$`Weight class` <- table$`Weight class`
event$Outcome <- table$Outcome
event$Method <- table$Method
event$Round <- table$Round
event$Time <- table$Time
event$Event <- table$Event
event$Promotion <- table$Promotion
event$Attendance <- table$Attendance
event$Date <- table$Date
event$City <- table$City
event$State <- table$State
event$Country <- table$Country
event$Enclosure <- table$Enclosure

Pride <- bind_rows(Pride, event)

Pride <- Pride[-1, ]
rownames(Pride) <- 1:nrow(Pride)

#### 1998 ####

stats <- event_links[67] %>%
  paste0("https://en.wikipedia.org", .) %>%
  read_html()

vector <- c(7, 10, 13)

for(i in vector) {
  
  table <- stats %>%
    html_table(fill = TRUE) %>%
    .[[i]]
  
  table1 <- stats %>%
    html_table(fill = TRUE) %>%
    .[[i-2]]
  
  names(table) <- table[1, ]
  table <- table[-1, ]
  table$Notes <- NULL
  
  for(j in 1:nrow(table)) {
    if(table$`Weight class`[j] == "") {
      table$`Weight class`[j] <- "Open Weight"
    }
  }
  
  for(k in 1:nrow(table)){
    if(table[k, 3] == "def.") {
      table[k, 3] <- "win"
    }
    if(table[k, 3] == "vs.") {
      table[k, 3] <- "draw"
    }
    if(table[k, 5] == "No Contest"){
      table[k, 3] <- "nc"
    }
  }
  
  row.names(table) <- 1:nrow(table)
  
  table$Str1 <- NA
  table$Str2 <- NA
  table$Td1 <- NA
  table$Td2 <- NA
  table$Sub1 <- NA
  table$Sub2 <- NA
  table$Pass1 <- NA
  table$Pass2 <- NA
  table$Outcome <- NA
  table$Enclosure <- "Ring"
  
  for(m in 1:nrow(table)) {
    table$Outcome[m] <- str_split_fixed(table$Method[m], " ", n = 2)[1]
    table$Method[m] <- str_split_fixed(table$Method[m], " ", n = 2)[2]
  }
  
  for(l in 1:nrow(table)){
    if(table$Method[l] == "Contest") {
      table$Method[l] <- "No Contest"
    }
    if(table$Outcome[l] == "No") {
      table$Outcome[l] <- "No Contest"
    }
  }
  
  table$Method <- str_remove_all(table$Method, "\\(")
  table$Method <- str_remove_all(table$Method, "\\)")
  
  table$Date <- table1[which(str_detect(table1[, 1], "Date")), 2]
  table$Date <- mdy(table$Date)
  
  if(sum((str_detect(table1[, 1], "Attendance"))) != 0) {
    table$Attendance <- table1[which(str_detect(table1[, 1], "Attendance")), 2]
    table$Attendance <- as.numeric(str_remove(table$Attendance, ","))
  } else {
    table$Attendance <- NA
  }
  
  table$Round <- as.numeric(table$Round)
  table$Event <- names(table1)[1]
  table$Promotion <- table1[which(str_detect(table1[, 1], "Promotion")), 2]
  table$Location <- table1[which(str_detect(table1[, 1], "City")), 2]
  
  location <- str_split_fixed(table$Location, ", ", n = 3)
  table$City <- location[, 1]
  table$State <- location[, 2]
  table$Country <- location[, 3]
  table$Location <- NULL
  
  for(n in seq_along(table$Country)) {
    if(is.na(table$Country[n])) {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
    if(table$Country[n] == "") {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
  }
  
  event <- table
  names(event) <- names(Pride)
  event$`W/L` <- table[, 3]
  event$Fighter1 <- table[, 2]
  event$Fighter2 <- table[, 4]
  event$Str1 <- table$Str1
  event$Str2 <-table$Str2
  event$Td1 <- table$Td1
  event$Td2 <- table$Td2
  event$Sub1 <- table$Sub1
  event$Sub2 <- table$Sub2
  event$Pass1 <- table$Pass1
  event$Pass2 <- table$Pass2
  event$`Weight class` <- table$`Weight class`
  event$Outcome <- table$Outcome
  event$Method <- table$Method
  event$Round <- table$Round
  event$Time <- table$Time
  event$Event <- table$Event
  event$Promotion <- table$Promotion
  event$Attendance <- table$Attendance
  event$Date <- table$Date
  event$City <- table$City
  event$State <- table$State
  event$Country <- table$Country
  event$Enclosure <- table$Enclosure
  
  Pride <- bind_rows(Pride, event)

}

Pride <- Pride[-c(9, 12), ]
Pride$Country[which(Pride$Country == "")] <- "Japan"

#### 1999 ####

stats <- event_links[64] %>%
  paste0("https://en.wikipedia.org", .) %>%
  read_html()

vector <- c(7, 10, 13, 16)

for(i in vector) {
  
  table <- stats %>%
    html_table(fill = TRUE) %>%
    .[[i]]
  
  table1 <- stats %>%
    html_table(fill = TRUE) %>%
    .[[i-2]]
  
  names(table) <- table[1, ]
  table <- table[-1, ]
  table$Notes <- NULL
  
  rows <- c()
  for(j in 1:nrow(table)) {
    if(table[j, 1] == table[[j, 2]]) {
      rows <- c(rows, j) 
    }
  }
  
  for(k in 1:nrow(table)) {
    if(table[k, 1] == "Weight class") {
      rows <- c(rows, k) 
    }
  }
  
  for(j in 1:nrow(table)) {
    if(table$`Weight class`[j] == "") {
      table$`Weight class`[j] <- "Open Weight"
    }
  }
  
  for(k in 1:nrow(table)){
    if(table[k, 3] == "def.") {
      table[k, 3] <- "win"
    }
    if(table[k, 3] == "vs.") {
      table[k, 3] <- "draw"
    }
    if(table[k, 5] == "No Contest"){
      table[k, 3] <- "nc"
    }
  }
  
  if(length(rows) != 0) {
    table <- table[-rows, ]
  }
  
  row.names(table) <- 1:nrow(table)
  
  table$Str1 <- NA
  table$Str2 <- NA
  table$Td1 <- NA
  table$Td2 <- NA
  table$Sub1 <- NA
  table$Sub2 <- NA
  table$Pass1 <- NA
  table$Pass2 <- NA
  table$Outcome <- NA
  table$Enclosure <- "Ring"
  
  for(m in 1:nrow(table)) {
    table$Outcome[m] <- str_split_fixed(table$Method[m], " ", n = 2)[1]
    table$Method[m] <- str_split_fixed(table$Method[m], " ", n = 2)[2]
  }
  
  for(l in 1:nrow(table)){
    if(table$Method[l] == "Contest") {
      table$Method[l] <- "No Contest"
    }
    if(table$Outcome[l] == "No") {
      table$Outcome[l] <- "No Contest"
    }
  }
  
  table$Method <- str_remove_all(table$Method, "\\(")
  table$Method <- str_remove_all(table$Method, "\\)")
  
  table$Date <- table1[which(str_detect(table1[, 1], "Date")), 2]
  table$Date <- mdy(table$Date)
  
  if(sum((str_detect(table1[, 1], "Attendance"))) != 0) {
    table$Attendance <- table1[which(str_detect(table1[, 1], "Attendance")), 2]
    table$Attendance <- as.numeric(str_remove(table$Attendance, ","))
  } else {
    table$Attendance <- NA
  }
  
  table$Round <- as.numeric(table$Round)
  table$Event <- names(table1)[1]
  table$Promotion <- table1[which(str_detect(table1[, 1], "Promotion")), 2]
  table$Location <- table1[which(str_detect(table1[, 1], "City")), 2]
  
  location <- str_split_fixed(table$Location, ", ", n = 3)
  table$City <- location[, 1]
  table$State <- location[, 2]
  table$Country <- location[, 3]
  table$Location <- NULL
  
  for(n in seq_along(table$Country)) {
    if(is.na(table$Country[n])) {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
    if(table$Country[n] == "") {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
  }
  
  event <- table
  names(event) <- names(Pride)
  event$`W/L` <- table[, 3]
  event$Fighter1 <- table[, 2]
  event$Fighter2 <- table[, 4]
  event$Str1 <- table$Str1
  event$Str2 <-table$Str2
  event$Td1 <- table$Td1
  event$Td2 <- table$Td2
  event$Sub1 <- table$Sub1
  event$Sub2 <- table$Sub2
  event$Pass1 <- table$Pass1
  event$Pass2 <- table$Pass2
  event$`Weight class` <- table$`Weight class`
  event$Outcome <- table$Outcome
  event$Method <- table$Method
  event$Round <- table$Round
  event$Time <- table$Time
  event$Event <- table$Event
  event$Promotion <- table$Promotion
  event$Attendance <- table$Attendance
  event$Date <- table$Date
  event$City <- table$City
  event$State <- table$State
  event$Country <- table$Country
  event$Enclosure <- table$Enclosure
  
  Pride <- bind_rows(Pride, event)
  
}

Pride <- Pride[-c(37, 47, 48), ]
Pride$Country[which(Pride$Country == "")] <- "Japan"

#### 2000 ####

url <- event_links[60] %>%
  paste0("https://en.wikipedia.org", .)

vector <- c(7, 11, 15, 18, 21, 24)

for(i in vector) {
  
  table <- url %>%
    htmltab(i, rm_nodata_cols = F) %>%
    .[, -8] %>%
    `rownames<-` (seq_len(nrow(.)))
  
  table1 <- url %>%
    htmltab(i-2, rm_nodata_cols = F)
  
  for(j in 1:nrow(table)) {
    if(is.na(table[, 1][j])) {
      table[, 1][j] <- "Open Weight"
    }
    if(table[, 1][j] == "") {
       table[, 1][j] <- "Open Weight"
    }
  }
  
  for(k in 1:nrow(table)){
    if(table[k, 3] == "def.") {
       table[k, 3] <- "win"
    }
    if(table[k, 3] == "vs.") {
      table[k, 3] <- "draw"
    }
    if(table[k, 5] == "No Contest"){
      table[k, 3] <- "nc"
    }
  }
  
  table$Str1 <- NA
  table$Str2 <- NA
  table$Td1 <- NA
  table$Td2 <- NA
  table$Sub1 <- NA
  table$Sub2 <- NA
  table$Pass1 <- NA
  table$Pass2 <- NA
  table$Outcome <- NA
  table$Enclosure <- "Ring"
  
  for(m in 1:nrow(table)) {
    table$Outcome[m] <- str_split_fixed(table[, 5][m], " ", n = 2)[1]
    table[, 5][m] <- str_split_fixed(table[, 5][m], " ", n = 2)[2]
  }
  
  for(l in 1:nrow(table)){
    if(table[, 5][l] == "Contest") {
       table[, 5][l] <- "No Contest"
    }
    if(table$Outcome[l] == "No") {
       table$Outcome[l] <- "No Contest"
    }
  }
  
  table[, 5] <- str_remove_all(table[, 5], "\\(")
  table[, 5] <- str_remove_all(table[, 5], "\\)")
  
  table$Date <- table1[which(str_detect(table1[, 1], "Date")), 2]
  table$Date <- mdy(table$Date)
  
  if(sum((str_detect(table1[, 1], "Attendance"))) != 0) {
    table$Attendance <- table1[which(str_detect(table1[, 1], "Attendance")), 2]
    table$Attendance <- as.numeric(str_remove(table$Attendance, ","))
  } else {
    table$Attendance <- NA
  }
  
  table[, 6] <- as.numeric(table[, 6])
  table$Event <- str_split_fixed(names(table1)[1], " >> ", n = 3)[1]
  table$Promotion <- table1[which(str_detect(table1[, 1], "Promotion")), 2]
  table$Location <- table1[which(str_detect(table1[, 1], "City")), 2]
  
  location <- str_split_fixed(table$Location, ", ", n = 3)
  table$City <- location[, 1]
  table$State <- location[, 2]
  table$Country <- location[, 3]
  table$Location <- NULL
  
  for(n in seq_along(table$Country)) {
    if(is.na(table$Country[n])) {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
    if(table$Country[n] == "") {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
  }
  
  event <- table
  names(event) <- names(Pride)
  event$`W/L` <- table[, 3]
  event$Fighter1 <- table[, 2]
  event$Fighter2 <- table[, 4]
  event$Str1 <- table$Str1
  event$Str2 <-table$Str2
  event$Td1 <- table$Td1
  event$Td2 <- table$Td2
  event$Sub1 <- table$Sub1
  event$Sub2 <- table$Sub2
  event$Pass1 <- table$Pass1
  event$Pass2 <- table$Pass2
  event$`Weight class` <- table[, 1]
  event$Outcome <- table$Outcome
  event$Method <- table[, 5]
  event$Round <- table[, 6]
  event$Time <- table[, 7]
  event$Event <- table$Event
  event$Promotion <- table$Promotion
  event$Attendance <- table$Attendance
  event$Date <- table$Date
  event$City <- table$City
  event$State <- table$State
  event$Country <- table$Country
  event$Enclosure <- table$Enclosure
  
  Pride <- bind_rows(Pride, event)
  
}

Pride$Country[which(Pride$Country == "")] <- "Japan"

#### 2001 ####

url <- event_links[54] %>%
  paste0("https://en.wikipedia.org", .)

vector <- c(8, 11, 14, 17, 20, 23)

for(i in vector) {
  
  table <- url %>%
    htmltab(i, rm_nodata_cols = F) %>%
    .[, -8] %>%
    `rownames<-` (seq_len(nrow(.)))
  
  table1 <- url %>%
    htmltab(i-2, rm_nodata_cols = F)
  
  for(j in 1:nrow(table)) {
    if(is.na(table[, 1][j])) {
      table[, 1][j] <- "Open Weight"
    }
    if(table[, 1][j] == "") {
      table[, 1][j] <- "Open Weight"
    }
  }
  
  for(k in 1:nrow(table)){
    if(table[k, 3] == "def.") {
      table[k, 3] <- "win"
    }
    if(table[k, 3] == "vs.") {
      table[k, 3] <- "draw"
    }
    if(table[k, 5] == "No Contest"){
      table[k, 3] <- "nc"
    }
  }
  
  table$Str1 <- NA
  table$Str2 <- NA
  table$Td1 <- NA
  table$Td2 <- NA
  table$Sub1 <- NA
  table$Sub2 <- NA
  table$Pass1 <- NA
  table$Pass2 <- NA
  table$Outcome <- NA
  table$Enclosure <- "Ring"
  
  for(m in 1:nrow(table)) {
    table$Outcome[m] <- str_split_fixed(table[, 5][m], " ", n = 2)[1]
    table[, 5][m] <- str_split_fixed(table[, 5][m], " ", n = 2)[2]
  }
  
  for(l in 1:nrow(table)){
    if(table[, 5][l] == "Contest") {
      table[, 5][l] <- "No Contest"
    }
    if(table$Outcome[l] == "No") {
      table$Outcome[l] <- "No Contest"
    }
  }
  
  table[, 5] <- str_remove_all(table[, 5], "\\(")
  table[, 5] <- str_remove_all(table[, 5], "\\)")
  
  table$Date <- table1[which(str_detect(table1[, 1], "Date")), 2]
  table$Date <- mdy(table$Date)
  
  if(sum((str_detect(table1[, 1], "Attendance"))) != 0) {
    table$Attendance <- table1[which(str_detect(table1[, 1], "Attendance")), 2]
    table$Attendance <- as.numeric(str_remove(table$Attendance, ","))
  } else {
    table$Attendance <- NA
  }
  
  table[, 6] <- as.numeric(table[, 6])
  table$Event <- str_split_fixed(names(table1)[1], " >> ", n = 3)[1]
  table$Promotion <- table1[which(str_detect(table1[, 1], "Promotion")), 2]
  table$Location <- table1[which(str_detect(table1[, 1], "City")), 2]
  
  location <- str_split_fixed(table$Location, ", ", n = 3)
  table$City <- location[, 1]
  table$State <- location[, 2]
  table$Country <- location[, 3]
  table$Location <- NULL
  
  for(n in seq_along(table$Country)) {
    if(is.na(table$Country[n])) {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
    if(table$Country[n] == "") {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
  }
  
  event <- table
  names(event) <- names(Pride)
  event$`W/L` <- table[, 3]
  event$Fighter1 <- table[, 2]
  event$Fighter2 <- table[, 4]
  event$Str1 <- table$Str1
  event$Str2 <-table$Str2
  event$Td1 <- table$Td1
  event$Td2 <- table$Td2
  event$Sub1 <- table$Sub1
  event$Sub2 <- table$Sub2
  event$Pass1 <- table$Pass1
  event$Pass2 <- table$Pass2
  event$`Weight class` <- table[, 1]
  event$Outcome <- table$Outcome
  event$Method <- table[, 5]
  event$Round <- table[, 6]
  event$Time <- table[, 7]
  event$Event <- table$Event
  event$Promotion <- table$Promotion
  event$Attendance <- table$Attendance
  event$Date <- table$Date
  event$City <- table$City
  event$State <- table$State
  event$Country <- table$Country
  event$Enclosure <- table$Enclosure
  
  Pride <- bind_rows(Pride, event)
  
}

Pride$Country[which(Pride$Country == "")] <- "Japan"
Pride$Promotion <- "Pride Fighting Championships"

#### 2002 ####

url <- event_links[48] %>%
  paste0("https://en.wikipedia.org", .)

vector <- c(8, 11, 14, 17, 20, 23, 26, 29, 32, 35)

for(i in vector) {
  
  table <- url %>%
    htmltab(i, rm_nodata_cols = F) %>%
    .[, -8] %>%
    `rownames<-` (seq_len(nrow(.)))
  
  table1 <- url %>%
    htmltab(i-2, rm_nodata_cols = F)
  
  for(j in 1:nrow(table)) {
    if(is.na(table[, 1][j])) {
      table[, 1][j] <- "Open Weight"
    }
    if(table[, 1][j] == "") {
      table[, 1][j] <- "Open Weight"
    }
  }
  
  for(k in 1:nrow(table)){
    if(table[k, 3] == "def.") {
      table[k, 3] <- "win"
    }
    if(table[k, 3] == "vs.") {
      table[k, 3] <- "draw"
    }
    if(table[k, 5] == "No Contest"){
      table[k, 3] <- "nc"
    }
  }
  
  table$Str1 <- NA
  table$Str2 <- NA
  table$Td1 <- NA
  table$Td2 <- NA
  table$Sub1 <- NA
  table$Sub2 <- NA
  table$Pass1 <- NA
  table$Pass2 <- NA
  table$Outcome <- NA
  table$Enclosure <- "Ring"
  
  for(m in 1:nrow(table)) {
    table$Outcome[m] <- str_split_fixed(table[, 5][m], " ", n = 2)[1]
    table[, 5][m] <- str_split_fixed(table[, 5][m], " ", n = 2)[2]
  }
  
  for(l in 1:nrow(table)){
    if(table[, 5][l] == "Contest") {
      table[, 5][l] <- "No Contest"
    }
    if(table$Outcome[l] == "No") {
      table$Outcome[l] <- "No Contest"
    }
  }
  
  table[, 5] <- str_remove_all(table[, 5], "\\(")
  table[, 5] <- str_remove_all(table[, 5], "\\)")
  
  table$Date <- table1[which(str_detect(table1[, 1], "Date")), 2]
  table$Date <- mdy(table$Date)
  
  if(sum((str_detect(table1[, 1], "Attendance"))) != 0) {
    table$Attendance <- table1[which(str_detect(table1[, 1], "Attendance")), 2]
    table$Attendance <- as.numeric(str_remove(table$Attendance, ","))
  } else {
    table$Attendance <- NA
  }
  
  table[, 6] <- as.numeric(table[, 6])
  table$Event <- str_split_fixed(names(table1)[1], " >> ", n = 3)[1]
  table$Promotion <- table1[which(str_detect(table1[, 1], "Promotion")), 2]
  table$Location <- table1[which(str_detect(table1[, 1], "City")), 2]
  
  location <- str_split_fixed(table$Location, ", ", n = 3)
  table$City <- location[, 1]
  table$State <- location[, 2]
  table$Country <- location[, 3]
  table$Location <- NULL
  
  for(n in seq_along(table$Country)) {
    if(is.na(table$Country[n])) {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
    if(table$Country[n] == "") {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
  }
  
  event <- table
  names(event) <- names(Pride)
  event$`W/L` <- table[, 3]
  event$Fighter1 <- table[, 2]
  event$Fighter2 <- table[, 4]
  event$Str1 <- table$Str1
  event$Str2 <-table$Str2
  event$Td1 <- table$Td1
  event$Td2 <- table$Td2
  event$Sub1 <- table$Sub1
  event$Sub2 <- table$Sub2
  event$Pass1 <- table$Pass1
  event$Pass2 <- table$Pass2
  event$`Weight class` <- table[, 1]
  event$Outcome <- table$Outcome
  event$Method <- table[, 5]
  event$Round <- table[, 6]
  event$Time <- table[, 7]
  event$Event <- table$Event
  event$Promotion <- table$Promotion
  event$Attendance <- table$Attendance
  event$Date <- table$Date
  event$City <- table$City
  event$State <- table$State
  event$Country <- table$Country
  event$Enclosure <- table$Enclosure
  
  Pride <- bind_rows(Pride, event)
  
}

Pride$Country[which(Pride$Country == "")] <- "Japan"
Pride$Promotion <- "Pride Fighting Championships"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "rio Nogueira"))] <- "Antonio Rogerio Nogueira"
Pride$Fighter1[which(str_detect(Pride$Fighter2, "rio Nogueira"))] <- "Antonio Rogerio Nogueira"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "igo Nogueira"))] <- "Antonio Rodrigo Nogueira"
Pride$Fighter1[which(str_detect(Pride$Fighter2, "igo Nogueira"))] <- "Antonio Rodrigo Nogueira"

#### 2003 ####

url <- event_links[38] %>%
  paste0("https://en.wikipedia.org", .)

vector <- c(8, 11, 14, 17, 20, 24)

for(i in vector) {
  
  table <- url %>%
    htmltab(i, rm_nodata_cols = F) %>%
    .[, -8] %>%
    `rownames<-` (seq_len(nrow(.)))
  
  table1 <- url %>%
    htmltab(i-2, rm_nodata_cols = F)
  
  for(j in 1:nrow(table)) {
    if(is.na(table[, 1][j])) {
      table[, 1][j] <- "Open Weight"
    }
    if(table[, 1][j] == "") {
      table[, 1][j] <- "Open Weight"
    }
  }
  
  for(k in 1:nrow(table)){
    if(table[k, 3] == "def.") {
      table[k, 3] <- "win"
    }
    if(table[k, 3] == "vs.") {
      table[k, 3] <- "draw"
    }
    if(!is.na(table[k, 5])) {
      if(table[k, 5] == "No Contest"){
        table[k, 3] <- "nc"
      }
    }
  }
  
  table$Str1 <- NA
  table$Str2 <- NA
  table$Td1 <- NA
  table$Td2 <- NA
  table$Sub1 <- NA
  table$Sub2 <- NA
  table$Pass1 <- NA
  table$Pass2 <- NA
  table$Outcome <- NA
  table$Enclosure <- "Ring"
  
  for(m in 1:nrow(table)) {
    table$Outcome[m] <- str_split_fixed(table[, 5][m], " ", n = 2)[1]
    table[, 5][m] <- str_split_fixed(table[, 5][m], " ", n = 2)[2]
  }
  
  for(l in 1:nrow(table)){
    if(table[, 5][l] == "Contest") {
      table[, 5][l] <- "No Contest"
    }
    if(table$Outcome[l] == "No") {
      table$Outcome[l] <- "No Contest"
    }
  }
  
  table[, 5] <- str_remove_all(table[, 5], "\\(")
  table[, 5] <- str_remove_all(table[, 5], "\\)")
  
  table$Date <- table1[which(str_detect(table1[, 1], "Date")), 2]
  table$Date <- mdy(table$Date)
  
  if(sum((str_detect(table1[, 1], "Attendance"))) != 0) {
    table$Attendance <- table1[which(str_detect(table1[, 1], "Attendance")), 2]
    table$Attendance <- as.numeric(str_remove(table$Attendance, ","))
  } else {
    table$Attendance <- NA
  }
  
  table[, 6] <- as.numeric(table[, 6])
  table$Event <- str_split_fixed(names(table1)[1], " >> ", n = 3)[1]
  table$Promotion <- table1[which(str_detect(table1[, 1], "Promotion")), 2]
  table$Location <- table1[which(str_detect(table1[, 1], "City")), 2]
  
  location <- str_split_fixed(table$Location, ", ", n = 3)
  table$City <- location[, 1]
  table$State <- location[, 2]
  table$Country <- location[, 3]
  table$Location <- NULL
  
  for(n in seq_along(table$Country)) {
    if(is.na(table$Country[n])) {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
    if(table$Country[n] == "") {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
  }
  
  event <- table
  names(event) <- names(Pride)
  event$`W/L` <- table[, 3]
  event$Fighter1 <- table[, 2]
  event$Fighter2 <- table[, 4]
  event$Str1 <- table$Str1
  event$Str2 <-table$Str2
  event$Td1 <- table$Td1
  event$Td2 <- table$Td2
  event$Sub1 <- table$Sub1
  event$Sub2 <- table$Sub2
  event$Pass1 <- table$Pass1
  event$Pass2 <- table$Pass2
  event$`Weight class` <- table[, 1]
  event$Outcome <- table$Outcome
  event$Method <- table[, 5]
  event$Round <- table[, 6]
  event$Time <- table[, 7]
  event$Event <- table$Event
  event$Promotion <- table$Promotion
  event$Attendance <- table$Attendance
  event$Date <- table$Date
  event$City <- table$City
  event$State <- table$State
  event$Country <- table$Country
  event$Enclosure <- table$Enclosure
  
  Pride <- bind_rows(Pride, event)
  
}

Pride$Country[which(Pride$Country == "")] <- "Japan"
Pride$Promotion <- "Pride Fighting Championships"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "rio Nogueira"))] <- "Antonio Rogerio Nogueira"
Pride$Fighter1[which(str_detect(Pride$Fighter2, "rio Nogueira"))] <- "Antonio Rogerio Nogueira"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "igo Nogueira"))] <- "Antonio Rodrigo Nogueira"
Pride$Fighter1[which(str_detect(Pride$Fighter2, "igo Nogueira"))] <- "Antonio Rodrigo Nogueira"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "Schembri"))] <- "Antonio Schembri"
Pride$Fighter1[which(str_detect(Pride$Fighter2, "Schembri"))] <- "Antonio RSchembri"


#### 2004 ####

url <- event_links[32] %>%
  paste0("https://en.wikipedia.org", .)

vector <- c(8, 11, 14, 18, 21, 25, 28, 32, 35, 38)

for(i in vector) {
  
  table <- url %>%
    htmltab(i, rm_nodata_cols = F) %>%
    .[, -8] %>%
    `rownames<-` (seq_len(nrow(.)))
  
  table1 <- url %>%
    htmltab(i-2, rm_nodata_cols = F)
  
  for(j in 1:nrow(table)) {
    if(is.na(table[, 1][j])) {
      table[, 1][j] <- "Open Weight"
    }
    if(table[, 1][j] == "") {
      table[, 1][j] <- "Open Weight"
    }
  }
  
  for(k in 1:nrow(table)){
    if(table[k, 3] == "def.") {
      table[k, 3] <- "win"
    }
    if(table[k, 3] == "vs.") {
      table[k, 3] <- "draw"
    }
    if(!is.na(table[k, 5])) {
      if(table[k, 5] == "No Contest"){
        table[k, 3] <- "nc"
      }
    }
  }
  
  table$Str1 <- NA
  table$Str2 <- NA
  table$Td1 <- NA
  table$Td2 <- NA
  table$Sub1 <- NA
  table$Sub2 <- NA
  table$Pass1 <- NA
  table$Pass2 <- NA
  table$Outcome <- NA
  table$Enclosure <- "Ring"
  
  for(m in 1:nrow(table)) {
    table$Outcome[m] <- str_split_fixed(table[, 5][m], " ", n = 2)[1]
    table[, 5][m] <- str_split_fixed(table[, 5][m], " ", n = 2)[2]
  }
  
  for(l in 1:nrow(table)){
    if(table[, 5][l] == "Contest") {
      table[, 5][l] <- "No Contest"
    }
    if(table$Outcome[l] == "No") {
      table$Outcome[l] <- "No Contest"
    }
  }
  
  table[, 5] <- str_remove_all(table[, 5], "\\(")
  table[, 5] <- str_remove_all(table[, 5], "\\)")
  
  table$Date <- table1[which(str_detect(table1[, 1], "Date")), 2]
  table$Date <- mdy(table$Date)
  
  if(sum((str_detect(table1[, 1], "Attendance"))) != 0) {
    table$Attendance <- table1[which(str_detect(table1[, 1], "Attendance")), 2]
    table$Attendance <- as.numeric(str_remove(table$Attendance, ","))
  } else {
    table$Attendance <- NA
  }
  
  table[, 6] <- as.numeric(table[, 6])
  table$Event <- str_split_fixed(names(table1)[1], " >> ", n = 3)[1]
  table$Promotion <- table1[which(str_detect(table1[, 1], "Promotion")), 2]
  table$Location <- table1[which(str_detect(table1[, 1], "City")), 2]
  
  location <- str_split_fixed(table$Location, ", ", n = 3)
  table$City <- location[, 1]
  table$State <- location[, 2]
  table$Country <- location[, 3]
  table$Location <- NULL
  
  for(n in seq_along(table$Country)) {
    if(is.na(table$Country[n])) {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
    if(table$Country[n] == "") {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
  }
  
  event <- table
  names(event) <- names(Pride)
  event$`W/L` <- table[, 3]
  event$Fighter1 <- table[, 2]
  event$Fighter2 <- table[, 4]
  event$Str1 <- table$Str1
  event$Str2 <-table$Str2
  event$Td1 <- table$Td1
  event$Td2 <- table$Td2
  event$Sub1 <- table$Sub1
  event$Sub2 <- table$Sub2
  event$Pass1 <- table$Pass1
  event$Pass2 <- table$Pass2
  event$`Weight class` <- table[, 1]
  event$Outcome <- table$Outcome
  event$Method <- table[, 5]
  event$Round <- table[, 6]
  event$Time <- table[, 7]
  event$Event <- table$Event
  event$Promotion <- table$Promotion
  event$Attendance <- table$Attendance
  event$Date <- table$Date
  event$City <- table$City
  event$State <- table$State
  event$Country <- table$Country
  event$Enclosure <- table$Enclosure
  
  Pride <- bind_rows(Pride, event)
  
}

Pride$Country[which(Pride$Country == "")] <- "Japan"
Pride$Promotion <- "Pride Fighting Championships"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "rio Nogueira"))] <- "Antonio Rogerio Nogueira"
Pride$Fighter2[which(str_detect(Pride$Fighter2, "rio Nogueira"))] <- "Antonio Rogerio Nogueira"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "igo Nogueira"))] <- "Antonio Rodrigo Nogueira"
Pride$Fighter2[which(str_detect(Pride$Fighter2, "igo Nogueira"))] <- "Antonio Rodrigo Nogueira"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "Schembri"))] <- "Antonio Schembri"
Pride$Fighter2[which(str_detect(Pride$Fighter2, "Schembri"))] <- "Antonio Schembri"

#### 2005 ####

url <- event_links[22] %>%
  paste0("https://en.wikipedia.org", .)

vector <- c(8, 11, 14, 18, 21, 25, 28, 32, 35, 38)

for(i in vector) {
  
  table <- url %>%
    htmltab(i, rm_nodata_cols = F) %>%
    .[, -8] %>%
    `rownames<-` (seq_len(nrow(.)))
  
  table1 <- url %>%
    htmltab(i-2, rm_nodata_cols = F)
  
  for(j in 1:nrow(table)) {
    if(is.na(table[, 1][j])) {
      table[, 1][j] <- "Open Weight"
    }
    if(table[, 1][j] == "") {
      table[, 1][j] <- "Open Weight"
    }
  }
  
  for(k in 1:nrow(table)){
    if(table[k, 3] == "def.") {
      table[k, 3] <- "win"
    }
    if(table[k, 3] == "vs.") {
      table[k, 3] <- "draw"
    }
    if(!is.na(table[k, 5])) {
      if(table[k, 5] == "No Contest"){
        table[k, 3] <- "nc"
      }
    }
  }
  
  table$Str1 <- NA
  table$Str2 <- NA
  table$Td1 <- NA
  table$Td2 <- NA
  table$Sub1 <- NA
  table$Sub2 <- NA
  table$Pass1 <- NA
  table$Pass2 <- NA
  table$Outcome <- NA
  table$Enclosure <- "Ring"
  
  for(m in 1:nrow(table)) {
    table$Outcome[m] <- str_split_fixed(table[, 5][m], " ", n = 2)[1]
    table[, 5][m] <- str_split_fixed(table[, 5][m], " ", n = 2)[2]
  }
  
  for(l in 1:nrow(table)){
    if(table[, 5][l] == "Contest") {
      table[, 5][l] <- "No Contest"
    }
    if(table$Outcome[l] == "No") {
      table$Outcome[l] <- "No Contest"
    }
  }
  
  table[, 5] <- str_remove_all(table[, 5], "\\(")
  table[, 5] <- str_remove_all(table[, 5], "\\)")
  
  table$Date <- table1[which(str_detect(table1[, 1], "Date")), 2]
  table$Date <- mdy(table$Date)
  
  if(sum((str_detect(table1[, 1], "Attendance"))) != 0) {
    table$Attendance <- table1[which(str_detect(table1[, 1], "Attendance")), 2]
    table$Attendance <- as.numeric(str_remove(table$Attendance, ","))
  } else {
    table$Attendance <- NA
  }
  
  table[, 6] <- as.numeric(table[, 6])
  table$Event <- str_split_fixed(names(table1)[1], " >> ", n = 3)[1]
  table$Promotion <- table1[which(str_detect(table1[, 1], "Promotion")), 2]
  table$Location <- table1[which(str_detect(table1[, 1], "City")), 2]
  
  location <- str_split_fixed(table$Location, ", ", n = 3)
  table$City <- location[, 1]
  table$State <- location[, 2]
  table$Country <- location[, 3]
  table$Location <- NULL
  
  for(n in seq_along(table$Country)) {
    if(is.na(table$Country[n])) {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
    if(table$Country[n] == "") {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
  }
  
  event <- table
  names(event) <- names(Pride)
  event$`W/L` <- table[, 3]
  event$Fighter1 <- table[, 2]
  event$Fighter2 <- table[, 4]
  event$Str1 <- table$Str1
  event$Str2 <-table$Str2
  event$Td1 <- table$Td1
  event$Td2 <- table$Td2
  event$Sub1 <- table$Sub1
  event$Sub2 <- table$Sub2
  event$Pass1 <- table$Pass1
  event$Pass2 <- table$Pass2
  event$`Weight class` <- table[, 1]
  event$Outcome <- table$Outcome
  event$Method <- table[, 5]
  event$Round <- table[, 6]
  event$Time <- table[, 7]
  event$Event <- table$Event
  event$Promotion <- table$Promotion
  event$Attendance <- table$Attendance
  event$Date <- table$Date
  event$City <- table$City
  event$State <- table$State
  event$Country <- table$Country
  event$Enclosure <- table$Enclosure
  
  Pride <- bind_rows(Pride, event)
  
}

Pride$Country[which(Pride$Country == "")] <- "Japan"
Pride$Promotion <- "Pride Fighting Championships"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "rio Nogueira"))] <- "Antonio Rogerio Nogueira"
Pride$Fighter2[which(str_detect(Pride$Fighter2, "rio Nogueira"))] <- "Antonio Rogerio Nogueira"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "igo Nogueira"))] <- "Antonio Rodrigo Nogueira"
Pride$Fighter2[which(str_detect(Pride$Fighter2, "igo Nogueira"))] <- "Antonio Rodrigo Nogueira"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "Schembri"))] <- "Antonio Schembri"
Pride$Fighter2[which(str_detect(Pride$Fighter2, "Schembri"))] <- "Antonio Schembri"

#### 2006 ####

url <- event_links[12] %>%
  paste0("https://en.wikipedia.org", .)

vector <- c(8, 11, 14, 18, 22, 26, 30, 34)

for(i in vector) {
  
  table <- url %>%
    htmltab(i, rm_nodata_cols = F) %>%
    .[, -8] %>%
    `rownames<-` (seq_len(nrow(.)))
  
  table1 <- url %>%
    htmltab(i-2, rm_nodata_cols = F)
  
  for(j in 1:nrow(table)) {
    if(is.na(table[, 1][j])) {
      table[, 1][j] <- "Open Weight"
    }
    if(table[, 1][j] == "") {
      table[, 1][j] <- "Open Weight"
    }
  }
  
  for(k in 1:nrow(table)){
    if(table[k, 3] == "def.") {
      table[k, 3] <- "win"
    }
    if(table[k, 3] == "vs.") {
      table[k, 3] <- "draw"
    }
    if(!is.na(table[k, 5])) {
      if(table[k, 5] == "No Contest"){
        table[k, 3] <- "nc"
      }
    }
  }
  
  table$Str1 <- NA
  table$Str2 <- NA
  table$Td1 <- NA
  table$Td2 <- NA
  table$Sub1 <- NA
  table$Sub2 <- NA
  table$Pass1 <- NA
  table$Pass2 <- NA
  table$Outcome <- NA
  table$Enclosure <- "Ring"
  
  for(m in 1:nrow(table)) {
    table$Outcome[m] <- str_split_fixed(table[, 5][m], " ", n = 2)[1]
    table[, 5][m] <- str_split_fixed(table[, 5][m], " ", n = 2)[2]
  }
  
  for(l in 1:nrow(table)){
    if(table[, 5][l] == "Contest") {
      table[, 5][l] <- "No Contest"
    }
    if(table$Outcome[l] == "No") {
      table$Outcome[l] <- "No Contest"
    }
  }
  
  table[, 5] <- str_remove_all(table[, 5], "\\(")
  table[, 5] <- str_remove_all(table[, 5], "\\)")
  
  table$Date <- table1[which(str_detect(table1[, 1], "Date")), 2]
  table$Date <- mdy(table$Date)
  
  if(sum((str_detect(table1[, 1], "Attendance"))) != 0) {
    table$Attendance <- table1[which(str_detect(table1[, 1], "Attendance")), 2]
    table$Attendance <- as.numeric(str_remove(table$Attendance, ","))
  } else {
    table$Attendance <- NA
  }
  
  table[, 6] <- as.numeric(table[, 6])
  table$Event <- str_split_fixed(names(table1)[1], " >> ", n = 3)[1]
  table$Promotion <- table1[which(str_detect(table1[, 1], "Promotion")), 2]
  table$Location <- table1[which(str_detect(table1[, 1], "City")), 2]
  
  location <- str_split_fixed(table$Location, ", ", n = 3)
  table$City <- location[, 1]
  table$State <- location[, 2]
  table$Country <- location[, 3]
  table$Location <- NULL
  
  for(n in seq_along(table$Country)) {
    if(is.na(table$Country[n])) {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
    if(table$Country[n] == "") {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
  }
  
  event <- table
  names(event) <- names(Pride)
  event$`W/L` <- table[, 3]
  event$Fighter1 <- table[, 2]
  event$Fighter2 <- table[, 4]
  event$Str1 <- table$Str1
  event$Str2 <-table$Str2
  event$Td1 <- table$Td1
  event$Td2 <- table$Td2
  event$Sub1 <- table$Sub1
  event$Sub2 <- table$Sub2
  event$Pass1 <- table$Pass1
  event$Pass2 <- table$Pass2
  event$`Weight class` <- table[, 1]
  event$Outcome <- table$Outcome
  event$Method <- table[, 5]
  event$Round <- table[, 6]
  event$Time <- table[, 7]
  event$Event <- table$Event
  event$Promotion <- table$Promotion
  event$Attendance <- table$Attendance
  event$Date <- table$Date
  event$City <- table$City
  event$State <- table$State
  event$Country <- table$Country
  event$Enclosure <- table$Enclosure
  
  Pride <- bind_rows(Pride, event)
  
}

Pride$Country[which(Pride$Country == "")] <- "Japan"
Pride$Promotion <- "Pride Fighting Championships"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "rio Nogueira"))] <- "Antonio Rogerio Nogueira"
Pride$Fighter2[which(str_detect(Pride$Fighter2, "rio Nogueira"))] <- "Antonio Rogerio Nogueira"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "igo Nogueira"))] <- "Antonio Rodrigo Nogueira"
Pride$Fighter2[which(str_detect(Pride$Fighter2, "igo Nogueira"))] <- "Antonio Rodrigo Nogueira"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "Schembri"))] <- "Antonio Schembri"
Pride$Fighter2[which(str_detect(Pride$Fighter2, "Schembri"))] <- "Antonio Schembri"

#### Pride 32 ####

url <- event_links[5] %>%
  paste0("https://en.wikipedia.org", .)

vector <- c(3)

for(i in vector) {
  
  table <- url %>%
    htmltab(i, rm_nodata_cols = F) %>%
    .[, -8] %>%
    `rownames<-` (seq_len(nrow(.)))
  
  table1 <- url %>%
    htmltab(i-2, rm_nodata_cols = F)
  
  for(j in 1:nrow(table)) {
    if(is.na(table[, 1][j])) {
      table[, 1][j] <- "Open Weight"
    }
    if(table[, 1][j] == "") {
      table[, 1][j] <- "Open Weight"
    }
  }
  
  for(k in 1:nrow(table)){
    if(table[k, 3] == "def.") {
      table[k, 3] <- "win"
    }
    if(table[k, 3] == "vs.") {
      table[k, 3] <- "draw"
    }
    if(!is.na(table[k, 5])) {
      if(table[k, 5] == "No Contest"){
        table[k, 3] <- "nc"
      }
    }
  }
  
  table$Str1 <- NA
  table$Str2 <- NA
  table$Td1 <- NA
  table$Td2 <- NA
  table$Sub1 <- NA
  table$Sub2 <- NA
  table$Pass1 <- NA
  table$Pass2 <- NA
  table$Outcome <- NA
  table$Enclosure <- "Ring"
  
  for(m in 1:nrow(table)) {
    table$Outcome[m] <- str_split_fixed(table[, 5][m], " ", n = 2)[1]
    table[, 5][m] <- str_split_fixed(table[, 5][m], " ", n = 2)[2]
  }
  
  for(l in 1:nrow(table)){
    if(table[, 5][l] == "Contest") {
      table[, 5][l] <- "No Contest"
    }
    if(table$Outcome[l] == "No") {
      table$Outcome[l] <- "No Contest"
    }
  }
  
  table[, 5] <- str_remove_all(table[, 5], "\\(")
  table[, 5] <- str_remove_all(table[, 5], "\\)")
  
  table$Date <- table1[which(str_detect(table1[, 1], "Date")), 2]
  table$Date <- mdy(table$Date)
  

  table$Attendance <- 11727
  
  table[, 6] <- as.numeric(table[, 6])
  table$Event <- str_split_fixed(names(table1)[1], " >> ", n = 3)[1]
  table$Promotion <- table1[which(str_detect(table1[, 1], "Promotion")), 2]
  table$Location <- table1[which(str_detect(table1[, 1], "City")), 2]
  
  location <- str_split_fixed(table$Location, ", ", n = 3)
  table$City <- location[, 1]
  table$State <- location[, 2]
  table$Country <- location[, 3]
  table$Location <- NULL
  
  for(n in seq_along(table$Country)) {
    if(is.na(table$Country[n])) {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
    if(table$Country[n] == "") {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
  }
  
  event <- table
  names(event) <- names(Pride)
  event$`W/L` <- table[, 3]
  event$Fighter1 <- table[, 2]
  event$Fighter2 <- table[, 4]
  event$Str1 <- table$Str1
  event$Str2 <-table$Str2
  event$Td1 <- table$Td1
  event$Td2 <- table$Td2
  event$Sub1 <- table$Sub1
  event$Sub2 <- table$Sub2
  event$Pass1 <- table$Pass1
  event$Pass2 <- table$Pass2
  event$`Weight class` <- table[, 1]
  event$Outcome <- table$Outcome
  event$Method <- table[, 5]
  event$Round <- table[, 6]
  event$Time <- table[, 7]
  event$Event <- table$Event
  event$Promotion <- table$Promotion
  event$Attendance <- table$Attendance
  event$Date <- table$Date
  event$City <- table$City
  event$State <- table$State
  event$Country <- table$Country
  event$Enclosure <- table$Enclosure
  
  Pride <- bind_rows(Pride, event)
  
}

#### Pride Shockwave 2006 ####

url <- event_links[3] %>%
  paste0("https://en.wikipedia.org", .)

vector <- c(3)

for(i in vector) {
  
  table <- url %>%
    htmltab(i, rm_nodata_cols = F) %>%
    .[, -8] %>%
    `rownames<-` (seq_len(nrow(.)))
  
  table1 <- url %>%
    htmltab(i-2, rm_nodata_cols = F)
  
  for(j in 1:nrow(table)) {
    if(is.na(table[, 1][j])) {
      table[, 1][j] <- "Open Weight"
    }
    if(table[, 1][j] == "") {
      table[, 1][j] <- "Open Weight"
    }
  }
  
  for(k in 1:nrow(table)){
    if(table[k, 3] == "def.") {
      table[k, 3] <- "win"
    }
    if(table[k, 3] == "vs.") {
      table[k, 3] <- "draw"
    }
    if(!is.na(table[k, 5])) {
      if(table[k, 5] == "No Contest"){
        table[k, 3] <- "nc"
      }
    }
  }
  
  table$Str1 <- NA
  table$Str2 <- NA
  table$Td1 <- NA
  table$Td2 <- NA
  table$Sub1 <- NA
  table$Sub2 <- NA
  table$Pass1 <- NA
  table$Pass2 <- NA
  table$Outcome <- NA
  table$Enclosure <- "Ring"
  
  for(m in 1:nrow(table)) {
    table$Outcome[m] <- str_split_fixed(table[, 5][m], " ", n = 2)[1]
    table[, 5][m] <- str_split_fixed(table[, 5][m], " ", n = 2)[2]
  }
  
  for(l in 1:nrow(table)){
    if(table[, 5][l] == "Contest") {
      table[, 5][l] <- "No Contest"
    }
    if(table$Outcome[l] == "No") {
      table$Outcome[l] <- "No Contest"
    }
  }
  
  table[, 5] <- str_remove_all(table[, 5], "\\(")
  table[, 5] <- str_remove_all(table[, 5], "\\)")
  
  table$Date <- table1[which(str_detect(table1[, 1], "Date")), 2]
  table$Date <- mdy(table$Date)
  
  if(sum((str_detect(table1[, 1], "Attendance"))) != 0) {
    table$Attendance <- table1[which(str_detect(table1[, 1], "Attendance")), 2]
    table$Attendance <- as.numeric(str_remove(table$Attendance, ","))
  } else {
    table$Attendance <- NA
  }
  
  table[, 6] <- as.numeric(table[, 6])
  table$Event <- str_split_fixed(names(table1)[1], " >> ", n = 3)[1]
  table$Promotion <- table1[which(str_detect(table1[, 1], "Promotion")), 2]
  table$Location <- table1[which(str_detect(table1[, 1], "City")), 2]
  
  location <- str_split_fixed(table$Location, ", ", n = 3)
  table$City <- location[, 1]
  table$State <- location[, 2]
  table$Country <- location[, 3]
  table$Location <- NULL
  
  for(n in seq_along(table$Country)) {
    if(is.na(table$Country[n])) {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
    if(table$Country[n] == "") {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
  }
  
  event <- table
  names(event) <- names(Pride)
  event$`W/L` <- table[, 3]
  event$Fighter1 <- table[, 2]
  event$Fighter2 <- table[, 4]
  event$Str1 <- table$Str1
  event$Str2 <-table$Str2
  event$Td1 <- table$Td1
  event$Td2 <- table$Td2
  event$Sub1 <- table$Sub1
  event$Sub2 <- table$Sub2
  event$Pass1 <- table$Pass1
  event$Pass2 <- table$Pass2
  event$`Weight class` <- table[, 1]
  event$Outcome <- table$Outcome
  event$Method <- table[, 5]
  event$Round <- table[, 6]
  event$Time <- table[, 7]
  event$Event <- table$Event
  event$Promotion <- table$Promotion
  event$Attendance <- table$Attendance
  event$Date <- table$Date
  event$City <- table$City
  event$State <- table$State
  event$Country <- table$Country
  event$Enclosure <- table$Enclosure
  
  Pride <- bind_rows(Pride, event)
  
}

Pride$Country[which(Pride$Country == "")] <- "Japan"
Pride$Promotion <- "Pride Fighting Championships"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "rio Nogueira"))] <- "Antonio Rogerio Nogueira"
Pride$Fighter2[which(str_detect(Pride$Fighter2, "rio Nogueira"))] <- "Antonio Rogerio Nogueira"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "igo Nogueira"))] <- "Antonio Rodrigo Nogueira"
Pride$Fighter2[which(str_detect(Pride$Fighter2, "igo Nogueira"))] <- "Antonio Rodrigo Nogueira"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "Schembri"))] <- "Antonio Schembri"
Pride$Fighter2[which(str_detect(Pride$Fighter2, "Schembri"))] <- "Antonio Schembri"

#### Pride 33 ####

url <- event_links[2] %>%
  paste0("https://en.wikipedia.org", .)

vector <- c(3)

for(i in vector) {
  
  table <- url %>%
    htmltab(i, rm_nodata_cols = F) %>%
    .[, -8] %>%
    `rownames<-` (seq_len(nrow(.)))
  
  table1 <- url %>%
    htmltab(i-2, rm_nodata_cols = F)
  
  for(j in 1:nrow(table)) {
    if(is.na(table[, 1][j])) {
      table[, 1][j] <- "Open Weight"
    }
    if(table[, 1][j] == "") {
      table[, 1][j] <- "Open Weight"
    }
  }
  
  for(k in 1:nrow(table)){
    if(table[k, 3] == "def.") {
      table[k, 3] <- "win"
    }
    if(table[k, 3] == "vs.") {
      table[k, 3] <- "draw"
    }
    if(!is.na(table[k, 5])) {
      if(table[k, 5] == "No Contest"){
        table[k, 3] <- "nc"
      }
    }
  }
  
  table$Str1 <- NA
  table$Str2 <- NA
  table$Td1 <- NA
  table$Td2 <- NA
  table$Sub1 <- NA
  table$Sub2 <- NA
  table$Pass1 <- NA
  table$Pass2 <- NA
  table$Outcome <- NA
  table$Enclosure <- "Ring"
  
  for(m in 1:nrow(table)) {
    table$Outcome[m] <- str_split_fixed(table[, 5][m], " ", n = 2)[1]
    table[, 5][m] <- str_split_fixed(table[, 5][m], " ", n = 2)[2]
  }
  
  for(l in 1:nrow(table)){
    if(table[, 5][l] == "Contest") {
      table[, 5][l] <- "No Contest"
    }
    if(table$Outcome[l] == "No") {
      table$Outcome[l] <- "No Contest"
    }
  }
  
  table[, 5] <- str_remove_all(table[, 5], "\\(")
  table[, 5] <- str_remove_all(table[, 5], "\\)")
  
  table$Date <- table1[which(str_detect(table1[, 1], "Date")), 2]
  table$Date <- mdy(table$Date)
  
  
  table$Attendance <- 12911
  
  table[, 6] <- as.numeric(table[, 6])
  table$Event <- str_split_fixed(names(table1)[1], " >> ", n = 3)[1]
  table$Promotion <- table1[which(str_detect(table1[, 1], "Promotion")), 2]
  table$Location <- table1[which(str_detect(table1[, 1], "City")), 2]
  
  location <- str_split_fixed(table$Location, ", ", n = 3)
  table$City <- location[, 1]
  table$State <- location[, 2]
  table$Country <- location[, 3]
  table$Location <- NULL
  
  for(n in seq_along(table$Country)) {
    if(is.na(table$Country[n])) {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
    if(table$Country[n] == "") {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
  }
  
  event <- table
  names(event) <- names(Pride)
  event$`W/L` <- table[, 3]
  event$Fighter1 <- table[, 2]
  event$Fighter2 <- table[, 4]
  event$Str1 <- table$Str1
  event$Str2 <-table$Str2
  event$Td1 <- table$Td1
  event$Td2 <- table$Td2
  event$Sub1 <- table$Sub1
  event$Sub2 <- table$Sub2
  event$Pass1 <- table$Pass1
  event$Pass2 <- table$Pass2
  event$`Weight class` <- table[, 1]
  event$Outcome <- table$Outcome
  event$Method <- table[, 5]
  event$Round <- table[, 6]
  event$Time <- table[, 7]
  event$Event <- table$Event
  event$Promotion <- table$Promotion
  event$Attendance <- table$Attendance
  event$Date <- table$Date
  event$City <- table$City
  event$State <- table$State
  event$Country <- table$Country
  event$Enclosure <- table$Enclosure
  
  Pride <- bind_rows(Pride, event)
  
}

#### Pride 34 ####

url <- event_links[1] %>%
  paste0("https://en.wikipedia.org", .)

vector <- c(3)

for(i in vector) {
  
  table <- url %>%
    htmltab(i, rm_nodata_cols = F) %>%
    .[, -8] %>%
    `rownames<-` (seq_len(nrow(.)))
  
  table1 <- url %>%
    htmltab(i-2, rm_nodata_cols = F)
  
  for(j in 1:nrow(table)) {
    if(is.na(table[, 1][j])) {
      table[, 1][j] <- "Open Weight"
    }
    if(table[, 1][j] == "") {
      table[, 1][j] <- "Open Weight"
    }
  }
  
  for(k in 1:nrow(table)){
    if(table[k, 3] == "def.") {
      table[k, 3] <- "win"
    }
    if(table[k, 3] == "vs.") {
      table[k, 3] <- "draw"
    }
    if(!is.na(table[k, 5])) {
      if(table[k, 5] == "No Contest"){
        table[k, 3] <- "nc"
      }
    }
  }
  
  table$Str1 <- NA
  table$Str2 <- NA
  table$Td1 <- NA
  table$Td2 <- NA
  table$Sub1 <- NA
  table$Sub2 <- NA
  table$Pass1 <- NA
  table$Pass2 <- NA
  table$Outcome <- NA
  table$Enclosure <- "Ring"
  
  for(m in 1:nrow(table)) {
    table$Outcome[m] <- str_split_fixed(table[, 5][m], " ", n = 2)[1]
    table[, 5][m] <- str_split_fixed(table[, 5][m], " ", n = 2)[2]
  }
  
  for(l in 1:nrow(table)){
    if(table[, 5][l] == "Contest") {
      table[, 5][l] <- "No Contest"
    }
    if(table$Outcome[l] == "No") {
      table$Outcome[l] <- "No Contest"
    }
  }
  
  table[, 5] <- str_remove_all(table[, 5], "\\(")
  table[, 5] <- str_remove_all(table[, 5], "\\)")
  
  table$Date <- table1[which(str_detect(table1[, 1], "Date")), 2]
  table$Date <- mdy(table$Date)
  
  if(sum((str_detect(table1[, 1], "Attendance"))) != 0) {
    table$Attendance <- table1[which(str_detect(table1[, 1], "Attendance")), 2]
    table$Attendance <- as.numeric(str_remove(table$Attendance, ","))
  } else {
    table$Attendance <- NA
  }
  
  table[, 6] <- as.numeric(table[, 6])
  table$Event <- str_split_fixed(names(table1)[1], " >> ", n = 3)[1]
  table$Promotion <- table1[which(str_detect(table1[, 1], "Promotion")), 2]
  table$Location <- table1[which(str_detect(table1[, 1], "City")), 2]
  
  location <- str_split_fixed(table$Location, ", ", n = 3)
  table$City <- location[, 1]
  table$State <- location[, 2]
  table$Country <- location[, 3]
  table$Location <- NULL
  
  for(n in seq_along(table$Country)) {
    if(is.na(table$Country[n])) {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
    if(table$Country[n] == "") {
      table$Country[n] <- table$State[n]
      table$State[n] <- NA
    }
  }
  
  event <- table
  names(event) <- names(Pride)
  event$`W/L` <- table[, 3]
  event$Fighter1 <- table[, 2]
  event$Fighter2 <- table[, 4]
  event$Str1 <- table$Str1
  event$Str2 <-table$Str2
  event$Td1 <- table$Td1
  event$Td2 <- table$Td2
  event$Sub1 <- table$Sub1
  event$Sub2 <- table$Sub2
  event$Pass1 <- table$Pass1
  event$Pass2 <- table$Pass2
  event$`Weight class` <- table[, 1]
  event$Outcome <- table$Outcome
  event$Method <- table[, 5]
  event$Round <- table[, 6]
  event$Time <- table[, 7]
  event$Event <- table$Event
  event$Promotion <- table$Promotion
  event$Attendance <- table$Attendance
  event$Date <- table$Date
  event$City <- table$City
  event$State <- table$State
  event$Country <- table$Country
  event$Enclosure <- table$Enclosure
  
  Pride <- bind_rows(Pride, event)
  
}

#### Cleaning data ####

Pride$Country[which(Pride$Country == "")] <- "Japan"
Pride$Promotion <- "Pride Fighting Championships"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "rio Nogueira"))] <- "Antonio Rogerio Nogueira"
Pride$Fighter2[which(str_detect(Pride$Fighter2, "rio Nogueira"))] <- "Antonio Rogerio Nogueira"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "igo Nogueira"))] <- "Antonio Rodrigo Nogueira"
Pride$Fighter2[which(str_detect(Pride$Fighter2, "igo Nogueira"))] <- "Antonio Rodrigo Nogueira"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "Schembri"))] <- "Antonio Schembri"
Pride$Fighter2[which(str_detect(Pride$Fighter2, "Schembri"))] <- "Antonio Schembri"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "cio Rua"))] <- "Mauricio Rua"
Pride$Fighter2[which(str_detect(Pride$Fighter2, "cio Rua"))] <- "Mauricio Rua"
Pride$Fighter1[which(str_detect(Pride$Fighter1, "cio Werdum"))] <- "Fabricio Werdum"
Pride$Fighter2[which(str_detect(Pride$Fighter2, "cio Werdum"))] <- "Fabricio Werdum"

Pride$Method <- str_to_title(Pride$Method)
Pride$Method[which(str_detect(Pride$Method, "Kimura"))] <- "Kimura"
Pride$Method[which(str_detect(Pride$Method, "Unanimous"))] <- "Unanimous"
Pride$Method[which(str_detect(Pride$Method, "Contest"))] <- "No Contest"
Pride$Method[which(str_detect(Pride$Method, "Naked Choke"))] <- "Rear-Naked Choke"
Pride$Method[which(str_detect(Pride$Method, "Armbar"))] <- "Armbar"
Pride$Method[which(str_detect(Pride$Method, "Guillotine Choke"))] <- "Guillotine Choke"
Pride$Method[which(str_detect(Pride$Method, " Triangle Choke"))] <- " Triangle Choke"
Pride$Method[which(str_detect(Pride$Method, "Arm-Triangle Choke"))] <- "Arm-Triangle Choke"
Pride$Method[which(str_detect(Pride$Method, "Time"))] <- "Time Expired"

Pride$Method[which(Pride$Method == "")] <- Pride$Outcome[which(Pride$Method == "")]
Pride$Method[which(Pride$Method == "")] <- NA
Pride$Outcome[which(Pride$Outcome == "")] <- NA

Pride$Time <- ms(Pride$Time)

dates <- unique(events_table$Date)

for(i in seq_along(dates)) {
  Pride$Attendance[which(Pride$Date == dates[i])] <- events_table$Attendance[which(events_table$Date == dates[i])]
}

Pride <- Pride %>%
  arrange(Date)

save(Pride, file = "Pride.RData")



