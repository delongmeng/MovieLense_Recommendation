# MovieLense Recommendation System

Recommendation system represents a classical application of machine learning technology. 
For example, for a movie recommendation system, the goal is to predict how a given user will rate a specific movie based on how the user rate other movies and how the movie is rated by other users. 
In this project, I will combine several machine learning strategies to construct a movie recommendation system based on the "MovieLens" dataset. 
The full MovieLens dataset, which can be found here: https://grouplens.org/datasets/movielens/latest/, is rather large. 
To make the computation easier, in this project I use the "10M" version of MovieLens dataset instead (https://grouplens.org/datasets/movielens/10m/). 
This 10M MovieLens dataset has around 10 million ratings and 100,000 tag applications applied to 10,000 movies by 72,000 users.

## Part 1

In Part 1 (the report: [MovieLens_MF.html](http://htmlpreview.github.io/?https://github.com/delongmeng/MovieLense_Recommendation/blob/master/MovieLens_MF.html)), I explored the MovieLense dataset, and built a series of machine learning models to predict the ratings. The final model is a matrix factorization (MF) model using the `recosystem` R package.

## Part 2

In Part 2 (the report: [MovieLens_recommenderlab.html](http://htmlpreview.github.io/?https://github.com/delongmeng/MovieLense_Recommendation/blob/master/MovieLens_recommenderlab.html)), we
- Analylized top movies  
- Tried User-based collaborative filtering (UBCF) and Item-based collaborative filtering (IBCF) algorithms using the `recommenderlab` R package
- Build a Shiny (web application)[https://chengwei112.shinyapps.io/MovieRecommend/] to recommend movies to users providing 2 methods (general top movies or user-based recommendations)