library(httr)
library(rvest)

dir.create("AuthorImages", showWarnings = FALSE)

links <- paste0("https://www.goodreads.com/list/show/1.Best_Books_Ever?page=", 1:5)

books_link <- lapply(links, function(link) {
  read_html(link) %>%
    html_elements(".js-tooltipTrigger a") %>%
    html_attr("href")
})

update_link <- function(links) {
  lapply(links, function(vector) {
    paste0("https://goodreads.com", vector)
  })
}
updated_link <- unlist(update_link(books_link))

Author_pagelink <- lapply(links, function(link) {
  read_html(link) %>%
    html_elements(".authorName") %>%
    html_attr("href")
})
Author_pagelink <- unique(unlist(Author_pagelink))

length(Author_pagelink)

sum(is.na(Author_pagelink))

Author_pagelink <- na.omit(Author_pagelink)

df <- read.csv("data_final_500.csv")
df <- df[, -1]  # Remove the first column 


authorNames <- unique(df$authorName)
length(authorNames)

image_urls <- vector("list", length(Author_pagelink))
for (i in 1:272) {
  image_url <- read_html(Author_pagelink[i]) %>%
    html_elements(".leftContainer.authorLeftContainer img") %>%
    html_attr("src")
  
  
  image_urls[[i]] <- ifelse(length(image_url) > 0, image_url[1], NA)
}
# Some error with i = 273 
for (i in 274:length(image_urls)) {
  image_url <- read_html(Author_pagelink[i]) %>%
    html_elements(".leftContainer.authorLeftContainer img") %>%
    html_attr("src")
  
  
  image_urls[[i]] <- ifelse(length(image_url) > 0, image_url[1], NA)
}
length(image_urls)
image_urls <- na.omit(unlist(image_urls))
authorNames <- authorNames[1:length(image_urls)]



for (i in seq_along(image_urls)) {
  image_url <- as.character(image_urls[i])  
  authorName_current <- authorNames[i]  
  
  if (!grepl("^https?://", image_url)) {
    cat("Invalid URL:", image_url, "\n")
    next 
  }
  
  
  destfile <- file.path("AuthorImages", paste0(authorName_current, ".jpg"))
  

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
