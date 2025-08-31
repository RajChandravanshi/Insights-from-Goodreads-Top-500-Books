library(shiny)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(wordcloud2)
library(shinythemes)
library(reactable)

# Make sure all the libraries installed in your system
# Load the data
group_6 <- read.csv('data_final_500.csv')
head(group_6)

group_6 <- group_6[, -1]  # Remove the first column because it is not needed

group_6 <- group_6 %>%
  mutate(cover_type = ifelse(cover_type == "Kindle Edition", 
                             " ebook", cover_type)) 

summary_data <- group_6 %>%
  group_by(top_genre) %>%
  summarise(
    count = n_distinct(Rank), 
    Avg_rank = mean(Rank, na.rm = TRUE)  # summarise the based on top_genre and arrenge i accending order 
  ) %>%
  arrange(count)
colors <- colorRampPalette(brewer.pal(9, "YlGnBu"))(100)
cover_counts <- group_6 %>%
  group_by(cover_type) %>%
  summarise(count2 = n_distinct(Rank))
# for multiple colors because there are more the 25 top_genre
# Filtered data reactive expression based on inputs
publishtion_year <- numeric(length = length(group_6$Rank))  

for(i in 1:length(group_6$First_published)) {
  
  publishtion_year[i] <- as.numeric(strsplit(group_6$First_published[i], ",")[[1]][2])
}

# Add the 'publishtion_year' column to 'group_6'
group_6$publishtion_year <- publishtion_year


group_6 <- group_6 %>%
  mutate(medium_of_publication = ifelse(cover_type %in% c(" Kindle Edition", " ebook"), "digital", "physical"))



correlation_value <- cor(group_6$avg_rating, group_6$author_avg_rating, use = "complete.obs") # correlation value between avg_rating of the book and author_avg_rating based on complete.obs
correlation_value2 <- cor(group_6$pages, group_6$avg_rating, use = "complete.obs")
correlation_value3 <- cor(group_6$rater, group_6$pages, use = "complete.obs")

# UI
ui <- fluidPage(theme = shinytheme("darkly"),
                navbarPage("Book Analysis Dashboard",
                           tabPanel("Author Information",
                                    sidebarLayout(
                                      sidebarPanel(
                                        width = 3,
                                        h2("Author Selection"),
                                        selectInput("author", "Select an Author:", 
                                                    choices = unique(group_6$authorName), 
                                                    selected = unique(group_6$authorName)[1])
                                      ),
                                      mainPanel(
                                        imageOutput("authorImage", width = "600px", height = "400px"),
                                        h3("Books by Selected Author"),
                                        reactableOutput("bookTable"),  # Updated here
                                        width = 9
                                      )
                                    )
                           ),
                           tabPanel("Book Details",
                                    sidebarLayout(
                                      sidebarPanel(
                                        width = 3,
                                        h2("Book Selection"),
                                        selectInput("book", "Select a Book:", 
                                                    choices = unique(group_6$Title), 
                                                    selected = unique(group_6$Title)[1])
                                      ),
                                      mainPanel(
                                        fluidRow(
                                          column(12,
                                                 h3(textOutput("book_analysis_title")),
                                                 imageOutput("bookCover", width = "300px", height = "400px"),
                                                 reactableOutput("book_details"),  # Updated here
                                                 plotOutput("ratingBarPlot")
                                          )
                                        )
                                      )
                                    )
                           ),
                           
                           tabPanel("Data Visualization",
                                    sidebarLayout(
                                      sidebarPanel(
                                        width = 3,
                                        h2("Plot Selection"),
                                        radioButtons("scatter_choice", "Select Plot Type:",
                                                     choices = c(
                                                       "Reader Count vs. Ranking" = "Rater vs Ranking",
                                                       "Page Count vs. Average Rating" = "Number Of Pages vs Avg Rating",
                                                       "Reader Count vs. Price" = "Number Of raters vs price of the book",
                                                       "Genre Distribution" = "Bar Plot of the genra",
                                                       "Book vs. Author Ratings" = "Avg_rating of books vs Avg_rating of Author",
                                                       "Cover Type Distribution" = "Countplot of books cover_type",
                                                       "Medium of publication" = "MOP"
                                                     ),
                                                     selected = "Rater vs Ranking"
                                        ),
                                        # Add separate button for wordcloud
                                        actionButton("show_wordcloud", "Show Genre Word Cloud")
                                      ),
                                      mainPanel(
                                        # Add conditional panels
                                        conditionalPanel(
                                          condition = "input.show_wordcloud % 2 == 0",
                                          plotOutput("scatterPlot"),
                                          textOutput("plotInsight")  # Add text output for insights here
                                        ),
                                        conditionalPanel(
                                          condition = "input.show_wordcloud % 2 == 1",
                                          wordcloud2Output("wordcloud")
                                        )
                                      )
                                    )
                           ),
                           
                           tabPanel("Statistical Analysis",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("genre", "Select Genres:", 
                                                    choices = unique(group_6$top_genre), 
                                                    selected = "Fiction", 
                                                    multiple = TRUE
                                        ),
                                        sliderInput("ratingRange", "Average Rating Range:",
                                                    min = min(group_6$avg_rating, na.rm = TRUE),
                                                    max = max(group_6$avg_rating, na.rm = TRUE),
                                                    value = c(3, 5)
                                        ),
                                        sliderInput("PublicationRange", "Publication year Range:",
                                                    min = min(publishtion_year),
                                                    max = max(publishtion_year),
                                                    value = c(1800, max(publishtion_year) )
                                        ),
                                        
                                        # New inputs for Variable Relationships tab
                                        conditionalPanel(
                                          condition = "input.tabset === 'Variable Relationships'",
                                          selectInput("xVar", "Select X-axis Variable:",
                                                      choices = c(
                                                        "Price" = "price",
                                                        "Average Rating" = "avg_rating",
                                                        "Number of Ratings" = "rater",
                                                        "Number of Pages" = "pages",
                                                        "Author Average Rating" = "author_avg_rating",
                                                        "Rank" = "Rank",
                                                        "Publishtion year" = "publishtion_year"
                                                      ),
                                                      selected = "price"
                                          ),
                                          selectInput("yVar", "Select Y-axis Variable:",
                                                      choices = c(
                                                        "Number of Ratings" = "rater",
                                                        "Price" = "price",
                                                        "Average Rating" = "avg_rating",
                                                        "Number of Pages" = "pages",
                                                        "Author Average Rating" = "author_avg_rating",
                                                        "Rank" = "Rank",
                                                        "Publishtion year" = "publishtion_year"
                                                      ),
                                                      selected = "rater"
                                          )
                                        ),
                                        actionButton("refresh", "Update View")
                                      ),
                                      mainPanel(
                                        tabsetPanel(id = "tabset",
                                                    tabPanel("Genre Ratings", plotOutput("ratingGenrePlot")),
                                                    tabPanel("Rating Distribution", plotOutput("ratingDistPlot")),
                                                    tabPanel("Variable Relationships", 
                                                             plotOutput("pricePopularityPlot"),
                                                             textOutput("variableInsight")  # Add text output for insights here
                                                    )
                                        )
                                      )
                                      
                                    )
                           )
                )
)
server <- function(input, output, session) {
  # Reactive filtered data
  filteredData <- reactive({
    req(input$genre, input$ratingRange , input$PublicationRange )
    group_6 %>%
      filter(top_genre %in% input$genre,
             avg_rating >= input$ratingRange[1],
             avg_rating <= input$ratingRange[2],
             publishtion_year >= input$PublicationRange[1],
             publishtion_year <= input$PublicationRange[2])
  })
  
  # Books by Author table
  # Books by Author table
  output$bookTable <- renderReactable({
    req(input$author)
    filtered_books <- subset(group_6, authorName == input$author)
    
    if (nrow(filtered_books) == 0) {
      return(reactable(data.frame(Message = "No books found for this author")))
    }
    
    reactable(
      filtered_books[, c("Rank", "Title", "authorName", "avg_rating", "price", 
                         "First_published", "pages", "author_avg_rating")],
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      defaultPageSize = 5,
      theme = reactableTheme(
        backgroundColor = "#2b2b2b",
        color = "white",
        borderColor = "#444444",
        stripedColor = "#3c3c3c",
        highlightColor = "#3c3c3c",
        cellPadding = "8px",
        style = list(fontFamily = "Arial, sans-serif"),
        headerStyle = list(
          backgroundColor = "#1b1b1b",
          color = "#ffffff",
          fontWeight = "bold"
        )
      )
    )
  })
  
  
  # Reactive expression for plot insights
  output$plotInsight <- renderText({
    switch(input$scatter_choice,
           "Rater vs Ranking" = "This plot shows the relationship between the number of ratings a book has received and its ranking. Generally, books with a higher number of ratings tend to have a better ranking (closer to the top), we can interpret this as book with higher rating  must've been read more and are very popular hence better ranking. Outliers may indicate books with high ratings but lower popularity, or very popular books with lower rankings.",
           
           "Number Of Pages vs Avg Rating" = "This plot shows how the number of pages in a book correlates with its average rating. A positive correlation suggests readers prefer longer books more than the shorter ones ",
           
           "Number Of raters vs price of the book" = "One might have expected that books with lower prices may have more ratings due to higher accessibility, while more expensive books might have fewer ratings.However this can be observed to be not true as we observe that the number of ratings is almost uncorrelated to the price of the book , which may indicate that for these top ranked books people are willing to pay money even for an expensive book. 
                                Also we can observe that for a given price the number of raters(the people reading it after purchasing) is not affected if the medium of publication is digital or physical .",
           
           "Bar Plot of the genra" = "This bar plot illustrates the distribution of book genres by count, showing which genres are most represented.We get to see that the four most most popular genres of Fiction , Fantasy , Classics and Young adult makes almost 75% of the top rated books. The color scale shows the average rank for each genre, revealing which genres are highly ranked on average.We also get to see that the top genre of Fiction has a good average ranking too .Genres with fewer books may indicate niche categories or limited offerings.",
           
           "Avg_rating of books vs Avg_rating of Author" = "This plot compares the average rating of books to the average rating of their authors. A strong positive correlation can be seen which would indicate that books by highly-rated authors also tend to have high ratings, showing the influence of author reputation on reader perception.",
           
           "Countplot of books cover_type" = "This bar plot shows the distribution of books by cover type. The counts of each cover type may reflect reader preferences, trends in publishing formats, and the popularity of digital versus physical formats. The plot might suggest that the physical format books are more popular than the digital books , however this might be a biased result because if a reader has to read online he may consider downloading a pirated version of the book if available and hence we will not be able to get a count of that.",
           
           "MOP" = "One might have thought that for a given price the people might rate the book with physical cover higher than the digital books as reading a physical book has a certain feel to it however we observe that it does not seem to be true as for a given price the medium of publication does not affect the avg rating much."
    )
  })
  
  # Author image
  output$authorImage <- renderImage({
    req(input$author)
    img_path <- file.path("Authorimages", paste0(input$author, ".jpg"))
    
    if (file.exists(img_path)) {
      list(src = img_path,
           alt = paste("Image of", input$author),
           width = 300,
           height = 300)
    } else {
      list(src = "Authorimages/placeholder.jpg",
           alt = "Image not available",
           width = 300,
           height = 300)
    }
  }, deleteFile = FALSE)
  
  # Book analysis title
  output$book_analysis_title <- renderText({
    req(input$book)
    paste("Analysis of Book:", input$book)
  })
  
  # Book cover
  output$bookCover <- renderImage({
    req(input$book)
    book_info <- subset(group_6, Title == input$book)
    
    if (nrow(book_info) > 0) {
      rank <- book_info$Rank[1]
      img_path <- file.path("book_covers", paste0("cover_", rank, ".jpg"))
      
      if (file.exists(img_path)) {
        list(src = img_path,
             alt = paste("Cover of", input$book),
             width = 300,
             height = 400)
      } else {
        list(src = "book_covers/placeholder.jpg",
             alt = "Cover not available",
             width = 300,
             height = 400)
      }
    } else {
      list(src = "book_covers/placeholder.jpg",
           alt = "Book not found",
           width = 300,
           height = 400)
    }
  }, deleteFile = FALSE)
  
  
  # Book details table
  output$book_details <- renderReactable({
    req(input$book)
    book_info <- subset(group_6, Title == input$book)
    
    if (nrow(book_info) == 0) {
      return(reactable(data.frame(Message = "No data found for this book")))
    }
    
    reactable(
      book_info[, c("Rank", "Title", "authorName", "top_genre", "avg_rating", 
                    "rater", "price", "First_published", "pages", "cover_type", 
                    "author_avg_rating")],
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      defaultPageSize = 5,
      theme = reactableTheme(
        backgroundColor = "#2b2b2b",
        color = "white",
        borderColor = "#444444",
        stripedColor = "#3c3c3c",
        highlightColor = "#3c3c3c",
        cellPadding = "8px",
        style = list(fontFamily = "Arial, sans-serif"),
        headerStyle = list(
          backgroundColor = "#1b1b1b",
          color = "#ffffff",
          fontWeight = "bold"
        )
      )
    )
  })
  # Rating bar plot
  output$ratingBarPlot <- renderPlot({
    req(input$book)
    book_ratings <- subset(group_6, Title == input$book)
    
    if (nrow(book_ratings) > 0) {
      ratings_data <- data.frame(
        Rating = factor(c("Five Stars", "Four Stars", "Three Stars", "Two Stars", "One Star"),
                        levels = c("Five Stars", "Four Stars", "Three Stars", "Two Stars", "One Star")),
        Count = c(
          book_ratings$Five_stars[1],
          book_ratings$Four_stars[1],
          book_ratings$Three_stars[1],
          book_ratings$Two_stars[1],
          book_ratings$One_stars[1]
        )
      )
      
      ggplot(ratings_data, aes(x = Rating, y = Count, fill = Rating)) +
        geom_bar(stat = "identity") +
        labs(title = paste("Rating Distribution for", input$book),
             y = "Number of Ratings",
             x = "Rating Category") +
        theme_minimal() +
        scale_fill_brewer(palette = "Set3") +
        geom_text(aes(label = paste(Count, "%")), vjust = -0.5, size = 5, color = "black")
    }
  })
  output$wordcloud <- renderWordcloud2({
    genre_freq <- as.data.frame(table(group_6$top_genre))
    colnames(genre_freq) <- c("word", "freq")
    
    wordcloud2(genre_freq, 
               size = 1,
               color = colorRampPalette(brewer.pal(9, "Set3"))(nrow(genre_freq)),
               backgroundColor = "black")
  })
  
  output$scatterPlot <- renderPlot({
    req(input$scatter_choice)
    
    base_theme <- theme_minimal() +
      theme(
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "gray40"),
        panel.grid.minor = element_line(color = "gray20"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white")
      )
    
    plot <- switch(
      input$scatter_choice,
      "Rater vs Ranking" = {
        ggplot(group_6, aes(x = rater, y = (501 - Rank))) +
          geom_point(color = "cyan", size = 2, alpha = 0.7) +
          labs(x = "Number of Ratings", y = "Ranking", 
               title = "Ratings Count vs Book Ranking") +
          coord_cartesian(xlim = c(0, max(group_6$rater))) +
          coord_cartesian(xlim = c(0, 5*10^6))+
          base_theme
      },
      "Number Of Pages vs Avg Rating" = {
        ggplot(group_6, aes(x = pages, y = avg_rating)) +
          geom_point(color = "lightblue", size = 2, alpha = 0.7) +
          geom_smooth(method = "lm", se = FALSE, color = "yellow") +
          labs(x = "Number of Pages", y = "Average Rating",
               title = "Page Count vs Average Rating") +
          coord_cartesian(xlim = c(0, 1500), ylim = c(3, 5)) +
          annotate("text", x = 100, y = 4.8,
                   label = paste("Correlation:", round(cor(group_6$pages, group_6$avg_rating, use = "complete.obs"), 2)),
                   color = "yellow", size = 5) +
          base_theme
      },
      "Number Of raters vs price of the book" = {
        ggplot(group_6, aes(x = price, y = rater , color = medium_of_publication)) +
          geom_point( size = 2, alpha = 0.7) +
          labs(x = "Book Price", y = "Number of Ratings",
               title = "Price vs Number of Ratings") +
          scale_y_log10() + 
          coord_cartesian(xlim = c(0, 30)) +
          base_theme
      },
      "Bar Plot of the genra" = {
        ggplot(summary_data, aes(x = reorder(top_genre, -count), y = count, fill = Avg_rank)) +
          geom_bar(stat = "identity", width = 0.7) +
          scale_fill_gradientn(colors = colors, name = "Average Rank") +
          labs(title = "Genre Distribution", x = "Genre", y = "Count") +
          geom_text(aes(label = count), vjust = -0.5, color = "white", fontface = "bold") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))+
          base_theme
      },
      "Avg_rating of books vs Avg_rating of Author" = {
        ggplot(group_6, aes(x = author_avg_rating, y = avg_rating)) +
          geom_point(color = "lightgreen", size = 2, alpha = 0.7) +
          geom_smooth(method = "lm", se = FALSE, color = "red") +
          labs(title = "Book Rating vs Author Average Rating",
               x = "Author Average Rating", y = "Book Rating") +
          coord_cartesian(xlim = c(3, 5), ylim = c(3, 5)) +
          annotate("text", x = 3.2, y = 4.8,
                   label = paste("Correlation:", round(cor(group_6$avg_rating, group_6$author_avg_rating, use = "complete.obs"), 2)),
                   color = "yellow", size = 5) +
          base_theme
      },
      "Countplot of books cover_type" = {
        ggplot(cover_counts, aes(x = reorder(cover_type, -count2), y = count2, fill = cover_type)) +
          geom_bar(stat = "identity") +
          labs(title = "Distribution of Book Cover Types", x = "Cover Type", y = "Count") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          geom_text(aes(label = count2), vjust = -0.5, color = "white") +
          base_theme
      },
      "MOP" = {
        ggplot(group_6, aes(x = price, y = avg_rating , color = medium_of_publication)) +
          geom_point( size = 2, alpha = 0.7) +
          labs(x = "price", y = "avg_rating", 
                title = "seeing the effect of medium of publication on the avg rating for a certain price of the book") +
          coord_cartesian(xlim = c(0, 30))+
          base_theme
      }
    )
    
    print(plot)
  })
  
  
  # Statistical analysis plots
  output$ratingGenrePlot <- renderPlot({
    req(filteredData())
    ggplot(filteredData(), aes(x = reorder(top_genre, -avg_rating), y = avg_rating, fill = top_genre)) +
      geom_boxplot(outlier.color = "yellow", outlier.size = 2, alpha = 0.7) +
      scale_fill_brewer(palette = "Set3") +
      labs(title = "Average Rating by Genre",
           x = "Genre", y = "Average Rating") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "white"),
        axis.text.y = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        legend.position = "none"
      )
  })
  
  output$ratingDistPlot <- renderPlot({
    req(filteredData())
    ggplot(filteredData(), aes(x = avg_rating)) +
      geom_histogram(binwidth = 0.1, fill = "dodgerblue", color = "black", alpha = 0.8) +
      labs(title = "Distribution of Average Ratings",
           x = "Average Rating", y = "Frequency") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold")
      )
  })
  
  output$variableInsight <- renderText({
    req(input$xVar, input$yVar)
    
    # Define insights based on xVar and yVar selection
    if (input$xVar == "price" && input$yVar == "rater") {
      return("One might have expected that books with lower prices may have more ratings due to higher accessibility, while more expensive books might have fewer ratings.However this can be observed to be not true as we observe that the number of ratings is almost uncorrelated to the price of the book , which may indicate that for these top ranked books people are willing to pay money even for an expensive book.")
    } else if (input$xVar == "pages" && input$yVar == "avg_rating") {
      return("A positive correlation here could imply that readers prefer longer, more detailed books, while a negative correlation might indicate a preference for shorter reads.For mostly all genres we get to see positive correlation which indicates that people generally like a longer book.")
    } else if (input$xVar == "author_avg_rating" && input$yVar == "avg_rating") {
      return(" A strong positive correlation here would suggest that books by highly-rated authors tend to have high ratings, indicating author reputation may influence book ratings.For mostly all genres we get to see a positive correlation")
    } else if (input$xVar == "Rank" && input$yVar == "rater") {
      return("This plot shows the relationship between Book Rank and Number of Ratings. Typically, highly ranked books (lower rank values) have more ratings, indicating greater popularity and reader engagement.")
    } else {
      return("Explore how the selected variables interact. Look for patterns and correlations that might reveal reader preferences or market trends.")
    }
  })
  
  
  output$pricePopularityPlot <- renderPlot({
    req(filteredData(), input$xVar, input$yVar)
    
    # Get readable labels for the axes
    x_label <- names(which(c(
      "Price" = "price",
      "Average Rating" = "avg_rating",
      "Number of Ratings" = "rater",
      "Number of Pages" = "pages",
      "Author Average Rating" = "author_avg_rating",
      "Rank" = "Rank"
    ) == input$xVar))
    
    y_label <- names(which(c(
      "Price" = "price",
      "Average Rating" = "avg_rating",
      "Number of Ratings" = "rater",
      "Number of Pages" = "pages",
      "Author Average Rating" = "author_avg_rating",
      "Rank" = "Rank"
    ) == input$yVar))
    
    ggplot(filteredData(), aes_string(x = input$xVar, y = input$yVar, color = "top_genre")) +
      geom_point(size = 3, alpha = 0.7) +
      scale_color_brewer(palette = "Set2") +
      labs(title = paste(y_label, "vs", x_label, "by Genre"),
           x = x_label, y = y_label, color = "Genre") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white")
      )
  })
  
  
  
  
}
# Run the app 
shinyApp(ui = ui, server = server)









