#A handy dandy script for parsing a google doc filled with interesting email announcements 
library("googlesheets")
library("dplyr")
library("stringr")

#Register the sheet with google sheets. 
email_sheet <- gs_title("Lazy Webmaster")

#read in the sheet as a dataframe
email_data <- email_sheet %>%
  gs_read()

# Function that takes two strings: message text and links and assembles 
# a string of html with the links plugged into the message text. 

# message_text = email_data$`Message Text`[2]
# link_text = email_data$Links[2]

#takes two strings, the content of the message cell and the links cell and turns into into valid linked html. 
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


