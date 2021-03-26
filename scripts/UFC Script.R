#### packages ####
library(rvest)
library(httr)
library(rjson)
library(stringr)
library(dplyr)
library(lubridate)
library(tidyr)
library(here)
library(ggplot2)
library(ggmap)
library(maptools)
library(maps)
library(readr)
library(gganimate)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(magick)

path <- here::here()
setwd(path)
main.dir <- getwd()

#### getting a list of all UFC events ####
all_events <- read_html("http://ufcstats.com/statistics/events/search?query=UFC&page=all")
  
events_table <- html_table(all_events, fill = TRUE)[[1]][-1,]
rownames(events_table) <- seq_along(events_table$Name)
events_table$Date <- 1
colnames(events_table) <- c("Name", "Location", "Date")
events_table$Name <- str_replace_all(events_table$Name, "[\r\n]" , "")
events_table$Name <- str_trim(events_table$Name)
events_table$Date <- 1

for(i in seq_along(events_table$Name)) {
  events_table$Date[i] <- events_table$Name[i] %>%
    str_split("  ") %>%
    unlist() %>%
    .[[38]]
  
  events_table$Name[i] <- events_table$Name[i] %>%
    str_split("  ") %>%
    unlist() %>%
    .[[1]]
}

events_table$Date <- mdy(events_table$Date)

events_table <- separate(events_table, 
         col = Location, 
         into = c("City", "State", "Country"), 
         sep = ", ")

for(i in seq_along(events_table$Country)) {
  if(is.na(events_table$Country[i])) {
    events_table$Country[i] <- events_table$State[i]
    events_table$State[i] <- NA
  }
}  

event_links <- all_events %>%
  xml_find_all(xpath = "//a/@href") %>%
  xml_text() %>%
  .[7:(length(.)-16)]

#### primary data frame ####
UFC <- tibble("1", "2", "3", 4, 5, 6, 7, 8, 9, 10, 11, "12", "13", "14",
                   15, "16", "17", "18", 19, "2017-06-03", "21", "22", "23", "24")

UFC[, 20] <- "July 07, 2019"
UFC[, 20] <- mdy(UFC[, 20])

names(UFC) <- c("W/L", "Fighter1", "Fighter2", "Str1", "Str2", "Td1",         
                "Td2", "Sub1", "Sub2", "Pass1", "Pass2", "Weight class",
                "Outcome", "Method", "Round", "Time", "Event", "Promotion",
                "Attendance","Date", "City", "State", "Country", "Enclosure")


#### getting individual event statistics ####

past_events_table <- events_table %>%
  filter(Date < Sys.Date())
past_event_links <- event_links[which(events_table$Date < Sys.Date())]

for(i in length(past_event_links):1) {
  
  stats <- read_html(past_event_links[i])
  
  table <- stats %>%
    html_table() %>%
    .[[1]]
  
  table$`W/L` <- str_sub(table$`W/L`, -4)
  
  table$Str <- table$Str %>%
    str_replace_all("[\r\n]" , "") %>%
    str_trim()
  table$Td <- table$Td %>%
    str_replace_all("[\r\n]" , "") %>%
    str_trim()
  table$Sub <- table$Sub %>%
    str_replace_all("[\r\n]" , "") %>%
    str_trim()
  table$Pass <- table$Pass %>%
    str_replace_all("[\r\n]" , "") %>%
    str_trim()
  table$Method <- table$Method %>%
    str_replace_all("[\r\n]" , "") %>%
    str_trim()
  
  methods_list <- str_split(table$Method, "  ")
  methods <-c(1:nrow(table))
  for(j in seq_along(methods)){
    methods[j] <- methods_list[[j]][length(methods_list[[j]])]
  }
  
  table <- separate(table, 
                    col = Str, 
                    into = c("Str1", "Str2"))
  table <- separate(table, 
                    col = Td, 
                    into = c("Td1", "Td2"))
  table <- separate(table, 
                    col = Sub, 
                    into = c("Sub1", "Sub2"))
  table <- separate(table, 
                    col = Pass, 
                    into = c("Pass1", "Pass2"))
  table <- separate(table, 
                    col = Method, 
                    into = c("Outcome", "Method"),
                    sep = " ")
  table <- separate(table,
                    col = Fighter,
                    into = c("Fighter1", "Fighter2"))
  table$Method <- methods
  table$Str1 <- as.numeric(table$Str1)
  table$Str2 <- as.numeric(table$Str2)
  table$Td1 <- as.numeric(table$Td1)
  table$Td2 <- as.numeric(table$Td2)
  table$Sub1 <- as.numeric(table$Sub1)
  table$Sub2 <- as.numeric(table$Sub2)
  table$Pass1 <- as.numeric(table$Pass1)
  table$Pass2 <- as.numeric(table$Pass2)
  table$Round <- as.numeric(table$Round)
  
  fighters <- stats %>%
    xml_find_all(xpath = "//body//section//div//div//table//tbody//tr//td//p//a") %>%
    xml_text() %>%
    str_replace_all("[\r\n]" , "") %>%
    str_trim()
  
  if(any(fighters == "draw")) {
    remove <- which(fighters == "draw")
    fighters <- fighters[-which(fighters == "draw")]
  }
  
  if(any(fighters == "win")) {
    remove <- which(fighters == "win")
    fighters <- fighters[-which(fighters == "win")]
  }
  
  if(any(fighters == "nc")) {
    remove <- which(fighters == "nc")
    fighters <- fighters[-which(fighters == "nc")]
  }
  
  even_indexes<-seq(2,length(fighters),2)
  odd_indexes<-seq(1,length(fighters) - 1,2)
  
  table$Fighter1 <- fighters[odd_indexes]
  table$Fighter2 <- fighters[even_indexes]
  
  overview <- stats %>%
    xml_find_all(xpath = "//body//section//div//div//div//ul//li") %>%
    xml_text() %>%
    str_replace_all("[\r\n]" , "") %>%
    str_replace("Date: ", "") %>%
    str_replace("Location: ", "") %>%
    str_replace("Attendance: ", "") %>%
    str_trim()
  
  overview[3] <- str_replace(overview[3], ",", "")
  
  event <- stats %>%
    xml_find_all(xpath = "//body//section//div//h2") %>%
    xml_text() %>%
    str_replace_all("[\r\n]" , "") %>%
    str_trim()
  
  table$Event <- event
  table$Promotion <- "Ultimate Fighting Championship"
  table$Attendance <- as.numeric(overview[3])
  table$Date <- mdy(overview[1])
  table$Location <- overview[2]
  table$Enclosure <- "Octagon"
  
  table <- separate(table, 
                    col = Location, 
                    into = c("City", "State", "Country"), 
                    sep = ", ")
  
  for(k in seq_along(table$Country)) {
    if(is.na(table$Country[k])) {
      table$Country[k] <- table$State[k]
      table$State[k] <- NA
    }
  } 
  
  UFC <- bind_rows(UFC, table)

} # end of main for loop

UFC <- UFC[-1, ]
UFC$Time <- ms(UFC$Time)
rownames(UFC) <- 1:nrow(UFC)

csv <- "Geocoded location data.csv"

geo_data <- read_csv(csv)

geo_data <- separate(geo_data, 
                     col = Location, 
                     into = c("City", "State", "Country"), 
                     sep = ", ")

geo_data$City[which(geo_data$City == "NA")] <- NA
geo_data$State[which(geo_data$State == "NA")] <- NA
geo_data$Country[which(geo_data$Country == "NA")] <- NA

UFC <- right_join(UFC, geo_data, by = c("City", "State", "Country"))

UFC <- UFC %>%
  arrange(Date)

save(UFC, file = "UFC.RData")

#### Plots ####

ufc <- readPNG('ufc (red).png')
load("MMA.RData")
MMA$Attendance[8] <- 2801

ggplot(MMA, aes(x = Date, y = Attendance)) +
  geom_point()


mapWorld <- borders("world", colour="gray80", fill="gray80") # create a layer of borders
WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify


plot_data <- MMA %>%
  select(Event, Attendance, Date, Latitude, Longitude, City, State, Country) %>%
  group_by(Event, Date, Latitude, Longitude, City, State, Country) %>%
  summarize(Attendance = mean(Attendance)) %>%
  arrange(Date)
x_limits <- c(-190)
y_limits <- c(-30)
  
mp <- NULL    
mp <- ggplot(plot_data, aes(x = Longitude, y = Latitude, size = Attendance)) + 
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "#ebebeb", colour = "#ebebeb", size=0.5) +
  geom_point(colour="#b20101",
             alpha = 0.6) +
  labs(title = '{frame_time}',
       size = "Attendance") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        title = element_text(face = "bold", 
                             colour = "#ebebeb"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = '#666d74', colour = '#666d74'),
        plot.background = element_rect(fill = '#242b31', colour = '#242b31'),
        legend.background = element_rect(colour = "#666d74", fill = "#666d74"),
        legend.text = element_text(colour = "#b20101",
                                   size = 15,
                                   face = "bold"),
        legend.title = element_text(color = "#b20101", face = "bold", size = 25),
        legend.position = c(0.12, 0.3),
        legend.key = element_rect(colour = "#666d74", 
                                  fill = "#666d74")) +
  guides(colour = guide_legend(override.aes = list(size=20))) +
  annotation_raster(ufc, ymin = -40, ymax = -60 ,xmin = -45, xmax = 15) +
  transition_time(Date) +
  shadow_mark()

anim_mp <- animate(mp, height = 624, width = 1300, fps = 32, duration = 20, detail = 100)
anim_save("UFC History.gif", anim_mp)


points <- c(1, 108, 174, 306, 338)
relevant_attendance <- plot_data$Attendance[points]

lp <- NULL
lp <- ggplot(plot_data, aes(x = Date, y = Attendance, label = Attendance)) +
  geom_point(colour="red4", 
             size = 1, 
             shape = 21, 
             fill = "red4") +
  geom_line(colour = "red4", alpha = 0.4) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "red4", face = "bold"),
        axis.line.x = element_line(colour = "red4", size = 1.5),
        axis.line.y = element_blank(),
        axis.ticks.y =element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = 'gray70', colour = 'gray70')) +
  transition_reveal(Date) +
  shadow_mark()

  
anim_mp <- animate(mp, height = 500, width = 1000, fps = 10, duration = 10)
anim_lp <- animate(lp, height = 100, width = 1000, fps = 10, duration = 60)

mp_a <- image_read(anim_mp)
lp_a <- image_read(anim_lp)

new_gif <- image_append(c(mp_a[1], lp_a[1]), stack = TRUE)
for(i in 2:480){
  combined <- image_append(c(mp_a[i], lp_a[i]), stack = TRUE)
  new_gif <- c(new_gif, combined)
}

anim_save("UFC History.gif", anim_mp)
anim_save("UFC Attendance.gif", anim_lp)
anim_save("Final Plot.gif", new_gif)


