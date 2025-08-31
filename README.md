# 📚 Insights from Goodreads Top 500 Books  
*MTH-208 Data Science Lab 1 – Group Project*  

---

## 📖 Project Overview  
This project analyzes the **Top 500 Books dataset scraped from Goodreads**, one of the largest platforms for book ratings and reviews.  
The goal is to perform **Exploratory Data Analysis (EDA)** and build an **interactive R Shiny web application** that provides insights into:  

- ⭐ **Top Rated Books**  
- 📈 **Top Sales / Most Reviewed Books**  
- 📚 **Popular Genres and Trends**  
- ✍️ **Author Popularity and Contributions**  
- 💰 **Relationship between Price and Ratings**  
- 📖 **Correlation between Reviews, Pages, and Ratings**  

---

## 🎯 Objectives  
1. Scrape and preprocess Goodreads Top 500 Books dataset using **R**.  
2. Perform **EDA** to identify key patterns and trends.  
3. Develop an **interactive R Shiny Dashboard** for data exploration.  
4. Understand **reader preferences, rating distributions, and genre dynamics**.  
5. Derive insights into **factors that contribute to a book’s popularity**.  

---

## 🗂️ Dataset Description  

The dataset consists of **500 records** of top-rated books on Goodreads with the following columns:  

| Column Name          | Description |
|-----------------------|-------------|
| **Rank**             | Ranking of the book in Goodreads Top 500 |
| **Title**            | Book title |
| **authorName**       | Author of the book |
| **avg_rating**       | Average rating (out of 5) |
| **rater**            | Number of people who rated the book |
| **score**            | Goodreads score based on engagement |
| **voter**            | Number of voters contributing to the score |
| **price**            | Price of the book (if available) |
| **First_published**  | Year of first publication |
| **pages**            | Number of pages |
| **reviews**          | Number of written reviews |
| **followers**        | Author's followers on Goodreads |
| **top_genre**        | Primary genre of the book |
| **second_genre**     | Secondary genre |
| **third_genre**      | Tertiary genre |
| **fourth_genre**     | Fourth genre |
| **fifth_genre**      | Fifth genre |
| **Five_stars**       | Count of 5-star ratings |
| **Four_stars**       | Count of 4-star ratings |
| **Three_stars**      | Count of 3-star ratings |
| **Two_stars**        | Count of 2-star ratings |
| **One_stars**        | Count of 1-star ratings |
| **cover_type**       | Type of book cover (Hardcover / Paperback / eBook) |
| **author_avg_rating**| Average rating across all books by the author |

---

## 🛠️ Methodology  

### 🔎 Data Collection  
- Scraped Goodreads Top 500 books using **R (`rvest`, `dplyr`, `stringr`)**.  
- Extracted book-level and author-level attributes.  

### 🔧 Data Preprocessing  
- Cleaned missing and inconsistent values.  
- Standardized genre labels.  
- Converted text-based ratings/reviews into numeric fields.  
- Prepared dataset for EDA and Shiny visualization.  

### 📊 EDA & Visualization  
- **Top Rated Books**: Analyzed distribution of `avg_rating`.  
- **Most Reviewed Books**: Sorted by `reviews` count.  
- **Genre vs Rating**: Average ratings per genre.  
- **Price vs Rating**: Relationship between book cost and user perception.  
- **Author Analysis**: Prolific authors with multiple titles in Top 500.  
- **Correlation Analysis**: Studied relationships between `pages`, `reviews`, `ratings`, and `price`.  

### 🖥️ R Shiny Dashboard  
The R Shiny app is divided into multiple interactive tabs:  
- **Top Rated Books** – Books with the highest ratings.  
- **Top Sales / Most Reviewed** – Most popular books by engagement.  
- **Genres** – Genre distribution, frequency, and rating comparison.  
- **Authors** – Author-wise book listings and statistics.  
- **Price vs Rating** – Scatter plot of price against rating.  
- **EDA Summary** – Basic statistics of dataset.  

---

## 📊 Results & Insights  

- **Top Rated Books**: Classics and Fantasy dominate, with average ratings above 4.5.  
- **Most Reviewed Books**: Titles adapted into movies/TV shows (e.g., *Harry Potter*, *Game of Thrones*) lead in reviews.  
- **Genres**: Fiction, Fantasy, and Classics are the most popular categories.  
- **Author Popularity**: Authors like J.K. Rowling, George R.R. Martin, and Jane Austen have multiple books in the Top 500.  
- **Price vs Rating**: No strong correlation; high ratings are distributed across different price ranges.  
- **Correlation Insights**:    
  - Highly reviewed books usually belong to popular genres.  
  - Author average rating is a strong indicator of consistent book quality.  

---
