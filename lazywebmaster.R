#A handy dandy script for parsing a google doc filled with interesting email announcements 
library("googlesheets")
library("dplyr")
library("stringr")
library("lubridate")

#Register the sheet with google sheets. 
email_sheet <- gs_title("Lazy Webmaster")

currentDate <- Sys.Date()

#read in the sheet as a dataframe
email_data <- email_sheet %>%
  gs_read() %>% 
  mutate(startDate = mdy(`Start Date`), 
         endDate   = mdy(`End Date`)) %>% 
  filter(startDate < currentDate & endDate > currentDate)
  

# Function that takes two strings: message text and links and assembles 
# a string of valid html with the links plugged into the message text. 
make_message <- function(message_text, link_text){
  #first parse links into a vector
  links_vec <- strsplit(link_text, ",")[[1]]
  
  #replace all square brackets with the link format in html
  message <- gsub("\\[", "<a href = ", message_text) %>% 
    gsub("\\]", "</a>", .)

  #Now we will step through the string, finding each link point, then inserting the link, then move onto the next. 
  
  #first, grab the link identifier (the {num} thingy) (if there is one). 
  link_num <- str_extract(message, "\\{[0-9]\\}") #gives me number inside of match
  
  #if we have links to run through do it until we've done all of them. 
  while( !is.na(link_num) ){
    #Find the corresponding link in links_vec using grepl, take out the id and then strip whitespace from the url
    url <- links_vec[grepl(link_num, links_vec, fixed = T)] %>% 
      gsub("\\{[0-9]\\}", "", .) %>% 
      trimws()
    
    message  <- sub(link_num, sprintf("'%s'>", url), message, fixed = T)
    
    #go get a new link to parse
    link_num <- str_extract(message, "\\{[0-9]\\}")
  }
  message
}

#Initialize a vector to hold the parsed html
html_vec <- c()

#loop through each row in the google sheet
for(rowNum in 1:dim(email_data)[1]){

  #grab the message and links and generate the message html
  message_text <- email_data$`Message Text`[rowNum]
  link_text    <- email_data$Links[rowNum]
  message_html <- make_message(message_text, link_text)
  
  #Grab the Group name
  group_name <- email_data$`Club/Person's Name`[rowNum]
  
  result   <- sprintf("<h2> %s </h2> \n <p> %s </p> \n",group_name, message_html )
  html_vec <- c(html_vec, result)
  cat(result)
}

fileConn <- file("email.html")
writeLines(html_vec, fileConn)
close(fileConn)
