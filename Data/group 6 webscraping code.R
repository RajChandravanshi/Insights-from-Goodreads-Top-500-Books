# loading libraries
library(tidyverse) 
library(rvest)





#creating the links for different pages 

links<- c()
for(i in 1:5)
{
  site <- paste0("https://www.goodreads.com/list/show/1.Best_Books_Ever?page=",i,"")
  links[i] <- site
}
links
links[1]

# Initialize an empty data frame to store results
df <- data.frame()






for (link in links) {
  

  page <- read_html(link)
  
  # Extracting ranks
  Rank <- as.numeric(page %>%
                       html_elements(".number") %>%
                       html_text())
  
  # Extracting titles
  Titles <- page %>%
    html_elements(".js-tooltipTrigger a") %>%
    html_attr("title")
  
  # Extracting author names
  authorName <- page %>%
    html_elements(".authorName span[itemprop=name]") %>%
    html_text()
  

  
  
  
  
  # Extracting ratings
  Rating <- page %>%
    html_elements(".minirating") %>%
    html_text()
  
  # Initialize vectors for avg_rating and number of raters
  avg_rating <- rep(0,100)
  people_who_rate <- rep(0,100)
  for(j in 1:100)
  {
    rate <- as.numeric(strsplit(Rating[j]," ")[[1]]) # split the string basis on " " (space)
    avg_rating[j] <- rate[!is.na(rate)]
    
    people_who_rate[j] <- strsplit(Rating[j]," ")[[1]][6]
    if(people_who_rate[j] == "rating"){
      people_who_rate[j] <- strsplit(Rating[j]," ")[[1]][8]
    }
    
    
  }
  length <- rep(0,100)
  for(i in 1:100)
  {
    length[i] <- length(strsplit(people_who_rate,",")[[i]])
  }
  rater <- rep(0,100)
  for(i in 1:100)
  {
    rater[i] <- as.numeric(paste0(trimws(strsplit(people_who_rate, ",")[[i]][1:length[i]]), collapse = ""))
  }
  
  
  
  
  
  
  
  
  # Extract scores and votes
  score_vote <- page %>% 
    html_elements(".smallText.uitext a") %>% 
    html_text()
  #Score_ vote columns cantain score and votes both
  # Extract socre and votes columns from score_vote column in numeric datatype 
  score <- rep(0,100)
  voter <- rep(0,100)
  for(i in 1:200)
  {
    score[i] <- score_vote[2*i-1]
    voter[i] <- score_vote[2*i]
  }
  score1 <- score[!is.na(score)]
  voter1 <- voter[!is.na(voter)]
  
  length_score = rep(0,100)
  for(i in 1:100)
  {
    length_score[i] = length(strsplit(score1,"[:, ]+")[[i]][-1])
  }
  score <- rep(0,100)
  for(i in 1:100)
  {
    score[i] <- as.numeric(paste0(trimws(strsplit(score1, "[:, ]+")[[i]][1:length_score[i]+1]), collapse = ""))
  }
  x <- rep(0,100)
  for(i in 1:100)
  {
    x[i] <- strsplit(voter1," ")[[i]][1]
  }
  length_voter = rep(0,100)
  
  for(i in 1:100)
  {
    length_voter[i] = length(strsplit(x,",")[[i]])
  }
  
  voter <- rep(0,100)
  for(i in 1:100)
  {
    voter[i] <- as.numeric(paste0(trimws(strsplit(voter1, "[:, ]+")[[i]][1:length_voter[i]]), collapse = ""))
  }
  
  
  
  
  
  # Creating a temporary data frame for the current link's data
  temp_result <- data.frame(
    Rank = Rank,
    Title = Titles,
    authorName = authorName,
    avg_rating = avg_rating,
    rater = rater,
    score = score,
    voter = voter,
    stringsAsFactors = FALSE
  )
  
  # Appending the temporary result to the main results data frame
  df <- bind_rows(df,temp_result)
}
df









# extracting links to individual books

books_link <- list()
for(i in 1:length(links))
{  
  books_link[[i]] <- read_html(links[i]) %>% html_elements(".js-tooltipTrigger a") %>% html_attr("href")
}
books_link
update_link <- function(links) {
  lapply(links, function(vector) {
    paste0("https://goodreads.com/", vector)
  })
}
updated_link <- update_link(books_link)
updated_link <- unlist(updated_link)
updated_link








# extracting various data from the individual books links 

links <- updated_link

links
num_books <- length(links)
price <-numeric(length = num_books)     # some dont have price available but amazon or somethings else so will keep them as NA
publish <-numeric(length = num_books)   #first published on date
pages <-numeric(length = num_books)    #pages and type of hardcover both
num_genres <- 5
genres <- matrix(0, nrow = num_books, ncol = num_genres) #top k genres
reviews <- numeric(length = num_books)  #number of reviews 
followers <- numeric(length = num_books) #number of followers of the author
stars <- matrix(0, nrow = num_books, ncol = 5) # percent per number of star rating recieved
str <- "https://www.goodreads.com"

for(i in 1:500){
  tryCatch({
    # Add a delay to avoid overwhelming the server
    Sys.sleep(1)
  html <- read_html(paste("", links[i], sep = ""))
  
  review <- html %>% html_elements(".BookActions__button .ButtonGroup .Button__container button[role = link] span.Button__labelItem")%>% html_text()
  price[i] <- review[1]
  
  publish[i] <- html %>% html_elements("p[data-testid = publicationInfo]")%>% html_text()
  
  pages[i]<- html %>% html_elements("p[data-testid = pagesFormat]")%>% html_text()
  
  genre <- html %>% html_elements("div[data-testid = genresList] span.Button__labelItem")%>% html_text()
  genres[i,]<-genre[1:num_genres]
  
  review <- html %>% html_elements(".RatingStatistics__meta")%>% html_text()

  
  follower <- html %>% html_elements("span.u-dot-before")%>% html_text()
  reviews[i] <- follower[1]
  followers[i] <- follower[2]
  
  stars[i,5]<- html %>% html_elements("div[data-testid=labelTotal-5 ]")%>% html_text()
  stars[i,4]<- html %>% html_elements("div[data-testid=labelTotal-4 ]")%>% html_text()
  stars[i,3]<- html %>% html_elements("div[data-testid=labelTotal-3 ]")%>% html_text()
  stars[i,2]<- html %>% html_elements("div[data-testid=labelTotal-2 ]")%>% html_text()
  stars[i,1]<- html %>% html_elements("div[data-testid=labelTotal-1 ]")%>% html_text()
  

  
  cat(sprintf("Processed %d of 500\n", i))
  }, error = function(e) {
  cat(sprintf("Error processing %d: %s\n", i, as.character(e)))
  })
}





#cleaning pages

pages
T_pages <- rep(0,length(pages))
for(i in 1:500)
{
  T_pages[[i]] <- as.numeric(strsplit(pages[i]," ")[[1]][1])
}
T_pages


extract <- strsplit(pages , split = ",")
extract <- sapply(extract , function(a) a[length(a)] )
cover_type <- extract






#code for combining

df
str(df)

df$price <- price
df$First_published <- publish
df$pages <- T_pages
df$cover_type <- cover_type
df$reviews <- reviews
df$followers <- followers
df$top_genre <- genres[,1]
df$second_genre <- genres[,2]
df$third_genre <- genres[,3]
df$fourth_genre <- genres[,4]
df$fifth_genre<- genres[,5]
df$Five_stars <- stars[,5]
df$Four_stars <- stars[,4]
df$Three_stars <- stars[,3]
df$Two_stars <- stars[,2]
df$One_stars <- stars[,1]





# converting to tibble and viewing data

str(df)
data <- tibble(df)
data









# cleaning stars columns

five_s <- strsplit(df$Five_stars , split = "%")
five_s <- sapply(five_s , function(a) a[1] )
five_s <- strsplit(five_s , split = "\\(")
five_s <- sapply(five_s , function(a) a[2] )
df$Five_stars <- sapply(five_s , function(a) a %>% as.numeric() )

four_s <- strsplit(df$Four_stars , split = "%")
four_s <- sapply(four_s , function(a) a[1] )
four_s <- strsplit(four_s , split = "\\(")
four_s <- sapply(four_s , function(a) a[2] )
df$Four_stars <- sapply(four_s , function(a) a %>% as.numeric() )

three_s <- strsplit(df$Three_stars , split = "%")
three_s <- sapply(three_s , function(a) a[1] )
three_s <- strsplit(three_s , split = "\\(")
three_s <- sapply(three_s , function(a) a[2] )
df$Three_stars <- sapply(three_s , function(a) a %>% as.numeric() )

two_s <- strsplit(df$Two_stars , split = "%")
two_s <- sapply(two_s , function(a) a[1] )
two_s <- strsplit(two_s , split = "\\(")
two_s <- sapply(two_s , function(a) a[2] )
two_s <- strsplit(two_s , split = "\\<")
two_s <- sapply(two_s , function(a) a[length(a)] )
df$Two_stars <- sapply(two_s , function(a) a %>% as.numeric() )

one_s <- strsplit(df$One_stars , split = "%")
one_s <- sapply(one_s , function(a) a[1] )
one_s <- strsplit(one_s , split = "\\(")
one_s <- sapply(one_s , function(a) a[2] )
one_s <- strsplit(one_s , split = "\\<")
one_s <- sapply(one_s , function(a) a[length(a)] )
df$One_stars <- sapply(one_s , function(a) a %>% as.numeric() )



#cleaning reviews column

rev <- strsplit(df$reviews , split = "r")
rev <- sapply(rev , function(a) a[1] %>%strsplit( split = ",") )
rev <- sapply(rev, function(a) {
  if(length(a) == 2) {
    paste0(a[1], a[2])
  } else {
    paste0(a[1], "")
  }
})
rev<- as.numeric(substr(rev , start=1 , stop=nchar(rev) - 1))
df$reviews  <-rev



# cleaning prices column

library(stringr)
extract <- df$price
extract <- str_extract(extract, "\\d+\\.\\d+")
extract
df$price <- extract



#cleaning when first published column

extract <- df$First_published
extract <- substring(extract , first=17)
extract[1:10]
df$First_published <- extract



#cleaning number of followers of authors column

fol <- strsplit(df$followers , split = "f")
fol <- sapply(fol , function(a) a[1] )
fol<- substr(fol , start=1 , stop=nchar(fol) - 1)
fol <- sapply(fol , function(a) gsub("[,]", "", a) )


fol <- sapply(fol, function(a) {
  if( substr(a, nchar(a), nchar(a)) == 'k') {
     gsub("k", "", a) %>% as.numeric(a)*1000
  }else{
    as.numeric(a)
  }
})

df$followers <- fol







# extracting the avg rating of author 

links<- c()
for(i in 1:5)
{
  site <- paste0("https://www.goodreads.com/list/show/1.Best_Books_Ever?page=",i,"")
  links[i] <- site
}
links

links_a <- c()


for (i in 1:5) {
  
  page <- read_html(links[i])
  
  extract <- page %>% html_elements("a.authorName") %>% html_attr("href")
  links_a <- c(links_a ,extract)
}

num_a <- length(links_a)
author_avg_rating <-numeric(length = num_a)

for(i in 1:500){
  tryCatch({
    # Add a delay to avoid overwhelming the server
    Sys.sleep(1)

    html <- read_html(links_a[i])
    
    author_avg_rating[i] <- html %>% html_elements("span.average")%>% html_text()
    
    cat(sprintf("Processed %d of 500\n", i))
  }, error = function(e) {
    cat(sprintf("Error processing %d: %s\n", i, as.character(e)))
  })
}
author_avg_rating

df$author_avg_rating <- author_avg_rating





# extracting the images of books covers however not including it as part of the data

books_link <- list()
for(i in 1:length(links))
{  
  books_link[[i]] <- read_html(links[i]) %>% html_elements(".js-tooltipTrigger a") %>% html_attr("href")
}
books_link
update_link <- function(links) {
  lapply(links, function(vector) {
    paste0("https://goodreads.com/", vector)
  })
}
updated_link <- update_link(books_link)
updated_link <- unlist(updated_link)
updated_link


links <- updated_link
num_books <- length(links)
img_links <-numeric(length = num_books)  

for(i in 1:500){
  html <- read_html(paste("", links[i], sep = ""))
  
  extract <- html %>% html_elements("img.ResponsiveImage")%>%html_attr("src")
  img_links[i] <- extract[1]
}

img_links[1:500]

install.packages("imager")
library(imager)

image_list <- list()

for (i in 1:500) {
  img <- load.image(img_links[i])  # Load the image
  resized_img <- resize(img, size_x = 500, size_y = 700)
  image_list[[i]] <- resized_img             # Store it in the list
}

plot(image_list[[1]])




# converting to tibble and viewing data

str(df)
data <- tibble(df)
data
view(tibble(df))
