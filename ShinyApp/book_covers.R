
library(rvest)
library(httr)
# you must install.packages httr and rvest

# loading the links of pages
links<- c()
for(i in 1:5)
{
  site <- paste0("https://www.goodreads.com/list/show/1.Best_Books_Ever?page=",i,"")
  links[i] <- site
}

# extract the book links
books_link <- list()
for(i in 1:length(links))
{
  books_link[[i]] <- read_html(links[i]) %>% html_elements(".js-tooltipTrigger a") %>% html_attr("href")
}
update_link <- function(links) {
  lapply(links, function(vector) {
    paste0("https://goodreads.com/", vector)
  })
}
updated_link <- update_link(books_link)
updated_link <- unlist(updated_link)


image_urls <- vector("list", length(updated_link)) # create a empty list for image link

# Loop through the links to scrape image URLs
for (i in 1:100) {
  image_url <- read_html(updated_link[i]) %>%
    html_elements(".BookCover__image img") %>%
    html_attr("src")
  
  # Store the image URL in the vector
  image_urls[[i]] <- image_url
}

for (i in 101:200) {
  image_url <- read_html(updated_link[i]) %>%
    html_elements(".BookCover__image img") %>%
    html_attr("src")
  
  # Store the image URL in the vector
  image_urls[[i]] <- image_url
}

for (i in 201:300) {
  image_url <- read_html(updated_link[i]) %>%
    html_elements(".BookCover__image img") %>%
    html_attr("src")
  
  # Store the image URL in the vector
  image_urls[[i]] <- image_url
}

for (i in 301:400) {
  image_url <- read_html(updated_link[i]) %>%
    html_elements(".BookCover__image img") %>%
    html_attr("src")
  
  # Store the image URL in the vector
  image_urls[[i]] <- image_url
}

for (i in 401:500) {
  image_url <- read_html(updated_link[i]) %>%
    html_elements(".BookCover__image img") %>%
    html_attr("src")
  
  # Store the image URL in the vector
  image_urls[[i]] <- image_url
}

image_urls <- unlist(image_urls) # unlist the list of images

images <- rep(0, length(updated_link)) # In the image_urls one link apppers two time so we get one one link 

for(i in 1:length(image_urls))
{
  if(i %% 2 != 0){
    images[i] <- image_urls[i]
  }
}

images <- na.omit(images)
images <- images[images != 0]


dir.create("book_covers", showWarnings = FALSE) # creating a folder
for (i in 1:length(images)) {
  image_url <- images[i]
  if (!grepl("^https?://", image_url)) {
    cat("Invalid URL:", image_url, "\n")
    next  # Skip to the next iteration
  }
  
  
  destfile <- file.path("book_covers", paste0("cover_", i, ".jpg")) # image name 
  
  tryCatch({
    response <- GET(image_url)
    if (status_code(response) == 200) {
      writeBin(content(response, "raw"), destfile)
      cat("Downloaded:", destfile, "\n")
    } else {
      cat("Failed to download:", image_url, "Status Code:", status_code(response), "\n")
    }
  }, error = function(e) {
    cat("Error downloading:", image_url, "Error:", conditionMessage(e), "\n")
  })
}

