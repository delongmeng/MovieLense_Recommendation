

##################################
# Create edx set, validation set #
##################################

## used to install essential packages.
## Once installed, don't need to run these code anymore.
# if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
# if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
# if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
# install.packages("lubridate")
# install.packages("recosystem")
# install.packages("kableExtra")
# install.packages("tinytex")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(stringr)
library(recosystem)
library(kableExtra)
library(tinytex)

# tinytex::tl_sizes() 
## used to install the TinyTeX environment.
## Once installed, don't need to run these code anymore.


## Data downloading and preparationg ##

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
head(ratings)
dim(ratings)
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
head(movies)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")
head(movielens)

# Validation set will be 10% of MovieLens data #
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set #
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)






















####################
# Data Exploration #
####################



head(edx)
dim(edx) # 9000055       6
n_distinct(edx$movieId) # 10677
n_distinct(edx$title) # 10676: there might be movies of different IDs with the same title
n_distinct(edx$userId) # 69878
n_distinct(edx$movieId)*n_distinct(edx$userId) # 746087406
n_distinct(edx$movieId)*n_distinct(edx$userId)/dim(edx)[1] # 83
mean_ratings<-mean(edx$rating) #3.512465

# convert timestamp to year
edx_1 <- edx %>% mutate(year_rated = year(as_datetime(timestamp)))

# extract the release year of the movie
# edx_1 has year_rated, year_released, age_at_rating, and titles without year information
edx_1 <- edx_1 %>% mutate(title = str_replace(title,"^(.+)\\s\\((\\d{4})\\)$","\\1__\\2" )) %>% 
  separate(title,c("title","year_released"),"__") %>%
  select(-timestamp) 
edx_1 <- edx_1 %>% mutate(age_at_rating= as.numeric(year_rated)-as.numeric(year_released))

# separate genres
# edx_2: the mixture of genres is split into different rows
edx_2 <- edx_1 %>% separate_rows(genres,sep = "\\|") %>% mutate(value=1)
dim(edx_2)  # 23371423        9
n_distinct(edx_2$genres)  # 20: there are 20 differnt types of genres
genres_rating <- edx_2 %>% group_by(genres) %>% summarize(n=n())
# get the information of 20 different types of genres, and numbers of movie ratings in each type.
# Note that the first type is "(no genres listed)", which is not really a type, but just for 7 movies, genres info was not provided 
# figure out the only one movie with no genres infomation is movieId 8606 title "Pull My Daisy" released in 1958
edx_1 %>% filter(genres=="(no genres listed)") %>% group_by(movieId, title, year_released,genres) %>% summarize(n = n())
# this shows number of movies in each genres. Note that a movie can belong to different genres
genres_rating %>% filter(genres!="(no genres listed)") %>% 
  ggplot(aes(reorder(genres,n),n))+ 
  geom_bar(stat = "identity")+
  coord_flip(y=c(0,4000000))+
  labs(x="Genres",y="Ratings Times per Genres")
genres_movie <- edx_2 %>% group_by(movieId, title, year_released,genres) %>% summarize(n=n()) %>% group_by(genres) %>% summarize(n=n())
genres_movie %>% filter(genres!="(no genres listed)") %>% 
  ggplot(aes(reorder(genres,n),n))+ 
  geom_bar(stat = "identity")+
  coord_flip(y=c(0,5500))+
  labs(x="Genres",y="Movie Numbers per Genres")
genres_analysis <- edx_2 %>% group_by(genres) %>% 
  summarize(rating_times_per_genres=n(),
            movies_number_per_genres=n_distinct(movieId),
            users_number_per_genres=n_distinct(userId),
            average_ratings_per_genres=mean(rating),
            sd_ratings_per_genres=sd(rating),
            rating_times_per_movie=n()/n_distinct(movieId)) %>%
  filter(genres!="(no genres listed)") %>% 
  arrange(desc(rating_times_per_genres))
genres_analysis
# The Drama genres is rated the most while the IMAX genres is the least rated
genres_analysis %>% 
  ggplot(aes(reorder(genres,rating_times_per_genres),rating_times_per_genres))+ 
  geom_bar(stat = "identity")+
  coord_flip(y=c(0,4000000))+
  labs(x="Genres",y="Ratings Times per Genres")

# This doesn't necessarily mean the people prefer to rate the Drama movies over other types, because it could be that this simply reflects there are more movies that belong to Drama.
# So we check the numbers of movies of each genres:
genres_analysis %>% 
  ggplot(aes(reorder(genres,movies_number_per_genres),movies_number_per_genres))+ 
  geom_bar(stat = "identity")+
  coord_flip(y=c(0,5500))+
  labs(x="Genres",y="Movie Numbers per Genres")
# We see the same trend
# now let's check the average numbers of ratings per movie in each genres
genres_analysis %>% 
  ggplot(aes(reorder(genres,rating_times_per_movie),rating_times_per_movie))+ 
  geom_bar(stat = "identity")+
  coord_flip(y=c(0,2000))+
  labs(x="Genres",y="Rating Times per Movie in Each Genres")
# Now we see a different trend: Interestingly, Adventure and Sci-Fi are more tented to be rated although they don't include as many movies compared to Drama.
# What about the value of ratings themselves, which we actually care about?
genres_analysis %>% 
  ggplot(aes(reorder(genres,average_ratings_per_genres),average_ratings_per_genres))+ 
  geom_bar(stat = "identity")+
  coord_flip(y=c(0,5))+
  labs(x="Genres",y="Average Ratings per Genres")+
  geom_hline(yintercept=mean_ratings,col="red",linetype="dashed")
# Now we find that some genres tend to have higher ratings than the average (such as Film-Noir) and some tend to have lower ratings (such as Horror).
# However, overall the genres effect seems to be rather minor.
# Another way to visulize the data: instead of only looking at average, we want to also see the distributioin. So we calculate both mean and sd of each genres and generate the 95% CI.
genres_analysis %>% 
  ggplot(aes(x=reorder(genres,average_ratings_per_genres),y=average_ratings_per_genres, ymin=average_ratings_per_genres-2*sd_ratings_per_genres,ymax=average_ratings_per_genres+2*sd_ratings_per_genres))+ 
  geom_point()+
  geom_errorbar()+
  coord_flip(y=c(0,7.5))+
  labs(x="Genres",y="95% CI of Ratings per Genres")+
  geom_hline(yintercept=mean_ratings,col="red",linetype="dashed")

# We can also spread genres to the "wide" format, because one rating record corresponds to one userId and one movieId, and shouldn't be duplicated by genres
# edx_3 is the final version for exploration of the effects of movie year, age, rating year, and genres on ratings
edx_3 <- edx_2 %>% spread(genres, value, fill=0) %>% select(-"(no genres listed)")
dim(edx_3) #  9000055      26
# We can visualize the genres of each movie. Here we look at the first 10 movies.
small <- edx_3[1:10, 8:26]
rownames(small)<-edx_3$movieId[1:10]
par(mar=c(6,3,0.5,0.5),mgp=c(5,1,0),bg="white")
image(1:19,1:10,t(as.matrix(small)),xlab="Genres",ylab="",axes=F)
abline(h=0:10+0.50, v=0:19+0.50, col = "grey")
mtext("Movie ID", side=2, line=2, adj=0.5)
mtext(text=rownames(small), side=2, line=0.3, at=seq(1,10,1), las=1, cex=0.8)
mtext(text=colnames(small), side=1, line=0.3, at=seq(1,19,1), las=2, cex=0.8)













head(edx_3)
edx_3 %>% ggplot(aes(rating))+
  geom_histogram(binwidth=0.25)+
  scale_x_continuous(breaks = seq(0.5,5,0.5))+
  geom_vline(xintercept=mean_ratings,col="red",linetype="dashed")

# we can see the overall distribution of all of the ratings. it is screwed to the right. All half stars are less frenquient than full stars.

# dim(edx) # 9000055       6
n_distinct(edx$movieId) # 10677
n_distinct(edx$title) # 10676: there might be movies of different IDs with the same title
n_distinct(edx$userId) # 69878
n_distinct(edx$movieId)*n_distinct(edx$userId) # 746087406
n_distinct(edx$movieId)*n_distinct(edx$userId)/dim(edx)[1] # 83

# As shown above, this edx dataset has 10677 distinct movies and 69878 distinct users. 
# If every user rated on every movie, we would have 10677*69878 = 746087406 ratings.
# However, we only have 9000055 ratings, which is only 1/83 of the number of all possible ratings.
# We can visualize 100 random samples of users and 100 random samples of movies to see how sparse this dataset is.
set.seed(1, sample.kind="Rounding")
random_users <- sample(unique(edx_3$userId), 100)
edx_3 %>% filter(userId %in% random_users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% 
  select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")







# Does age of movie affect rating?
age_analysis <- edx_3 %>% group_by(age_at_rating) %>% 
  summarize(rating_times_per_age=n(),
            movies_number_per_age=n_distinct(movieId),
            average_ratings_per_age=mean(rating),
            rating_times_per_movie=n()/n_distinct(movieId))

# number of ratings by age of movies
age_analysis %>% 
  ggplot(aes(age_at_rating,rating_times_per_age))+ 
  geom_bar(stat = "identity")+
  labs(x="Ages of Movie at Rating",y="Ratings Times")

# number of movies per age group
age_analysis %>% 
  ggplot(aes(age_at_rating, movies_number_per_age))+ 
  geom_bar(stat = "identity")+
  labs(x="Ages of Movie at Rating",y="Movie Numbers")

# number of ratings by age of movies normalized by movie numbers per age group
age_analysis %>% 
  ggplot(aes(age_at_rating, rating_times_per_movie))+ 
  geom_bar(stat = "identity")+
  labs(x="Ages of Movie at Rating",y="Rating Numbers per Movie")

# Now let's look at the ratings:
age_analysis %>% 
  ggplot(aes(age_at_rating, average_ratings_per_age,size=rating_times_per_age)) +
  guides(size=FALSE) + 
  geom_point(alpha=1/5, color="red") +
  xlab("Ages of Movie at Rating")+
  ylab("Average Ratings")+
  geom_smooth() +
  geom_hline(yintercept=mean_ratings,col="red",linetype="dashed")

age_analysis %>% 
  filter(age_at_rating<20 &age_at_rating>10)%>%
  ggplot(aes(age_at_rating, average_ratings_per_age,size=rating_times_per_age)) +
  guides(size=FALSE) + 
  geom_point(alpha=1/5, color="red") +
  xlab("Ages of Movie at Rating")+
  ylab("Average Ratings")+
  geom_smooth() +
  geom_hline(yintercept=mean_ratings,col="red",linetype="dashed")

age_analysis %>% 
  filter(age_at_rating>80)%>%
  ggplot(aes(age_at_rating, average_ratings_per_age,size=rating_times_per_age)) +
  guides(size=FALSE) + 
  geom_point(alpha=1/5, color="red") +
  xlab("Ages of Movie at Rating")+
  ylab("Average Ratings")+
  geom_smooth() +
  geom_hline(yintercept=mean_ratings,col="red",linetype="dashed")

# Interestingly, movies with less than 18 years tend to have around average or slightly lower than average ratings, while older movies tend to have much greater than average ratings (with only handful exceptions).
# This could be because old movies that are still largely watched are usually those with good reputation and are widely recommended. In another word, they have been already selected. On the other hand, newer movies have not been judged enough by customers.

# I noticed that there're some movies with negative "age":
# edx_3 %>% filter(age_at_rating < 0) %>% group_by(movieId, title,year_released, year_rated) %>% summarize(n = n())
# edx %>% group_by(movieId, title,timestamp) %>% filter(movieId==779) %>% summarize(n = n())
# Why is it?
# Age=0 means that the movie was watched/rated at the same year when it was released. 
# Negative Ages seem to be impossible, because it means some movies were rated even before it was even formally released.
# Maybe the ratings were based on news reports about the movie or movie trailers.
# It could happen as long as there's no restriction in the rating system that a movie must be rated after it's formally released.







































#####################
# Model Development #
#####################


# define RMSE: residual mean squared error
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
options(digits=6)
options(pillar.sigfig = 6)


## Model 1 ##
## first model: use average ratings for all movies regardless of user

mu <- mean(edx$rating)
mu # 3.51247

naive_rmse <- RMSE(validation$rating, mu)
naive_rmse # 1.0612

rmse_results <- data_frame(Model = "Just the average", RMSE = naive_rmse)
rmse_results



## Model 2 ##
## Modeling Age Effects: adding b_a to represent ratings on movies with certain age
age_effect<- edx_1 %>% 
  group_by(age_at_rating) %>%
  summarize(b_a = mean(rating)-mu)
age_effect %>% qplot(b_a, geom ="histogram", bins = 10, data = ., color = I("black"))

validation_1 <- validation %>% 
  mutate(year_rated = year(as_datetime(timestamp)))%>% 
  mutate(title = str_replace(title,"^(.+)\\s\\((\\d{4})\\)$","\\1__\\2" )) %>% 
  separate(title,c("title","year_released"),"__") %>%
  select(-timestamp) %>%
  mutate(age_at_rating= as.numeric(year_rated)-as.numeric(year_released))

predicted_ratings_2 <- mu + validation_1 %>% 
  left_join(age_effect, by='age_at_rating') %>%
  pull(b_a)
model_2_rmse <- RMSE(validation$rating,predicted_ratings_2) # 1.05239
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Age Effect Model",  
                                     RMSE = model_2_rmse))
rmse_results
## We can see that Age Effect Model did not improve the RMSE much. So I give up using Ages of movies as a predictor later.




## Model 3 ##
## modeling movie effects: adding b_i to represent average ranking for movie_i

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))
predicted_ratings_3 <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
model_3_rmse <- RMSE(validation$rating,predicted_ratings_3) # 0.943909
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Movie Effect Model",  
                                     RMSE = model_3_rmse))
rmse_results






## Model 4 ##
## user effects: adding b_u to represent average ranking for user_u

user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings_4 <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
model_4_rmse <- RMSE(validation$rating,predicted_ratings_4) # 0.865349
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Movie + User Effects Model",  
                                     RMSE = model_4_rmse))
rmse_results












## Model 5 ##
## regularization of movie effect: control the total variability of the movie effects taking the number of ratings made for a specific movie into account

# 5.1 perform cross validation to determine the parameter lambda

# use 10-fold cross validation to pick a lambda for movie effects regularization
# split the data into 10 parts
set.seed(2019, sample.kind = "Rounding")
cv_splits <- createFolds(edx$rating, k=10, returnTrain =TRUE)

# define a matrix to store the results of cross validation
rmses <- matrix(nrow=10,ncol=51)
lambdas <- seq(0, 5, 0.1)

# perform 10-fold cross validation to determine the optimal lambda
for(k in 1:10) {
  train_set <- edx[cv_splits[[k]],]
  test_set <- edx[-cv_splits[[k]],]
  
  # Make sure userId and movieId in test set are also in the train set
  test_final <- test_set %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  # Add rows removed from validation set back into edx set
  removed <- anti_join(test_set, test_final)
  train_final <- rbind(train_set, removed)
  
  mu <- mean(train_final$rating)
  just_the_sum <- train_final %>% 
    group_by(movieId) %>% 
    summarize(s = sum(rating - mu), n_i = n())
  
  rmses[k,] <- sapply(lambdas, function(l){
    predicted_ratings <- test_final %>% 
      left_join(just_the_sum, by='movieId') %>% 
      mutate(b_i = s/(n_i+l)) %>%
      mutate(pred = mu + b_i) %>%
      pull(pred)
    return(RMSE(predicted_ratings, test_final$rating))
  })
}

rmses
rmses_cv <- colMeans(rmses)
rmses_cv
qplot(lambdas,rmses_cv)
lambdas[which.min(rmses_cv)]   #2.2

# 5.2 model generation and prediction
# Regularized Movie Effect Model
# lambda <- lambdas[which.min(rmses_cv)]
lambda <- 2.2
mu <- mean(edx$rating)
movie_reg_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 
predicted_ratings_5 <- validation %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
model_5_rmse <- RMSE(predicted_ratings_5, validation$rating)   # 0.943852 not too much improved
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized Movie Effect Model",  
                                     RMSE = model_5_rmse))
rmse_results 








## Model 6 ##
## regularization of both movie and user effects
## use the same lambda for both movie and user effects

# 6.1 perform cross validation to determine the parameter lambda
# define a matrix to store the results of cross validation
lambdas <- seq(0, 8, 0.1)
rmses_2 <- matrix(nrow=10,ncol=length(lambdas))
# perform 10-fold cross validation to determine the optimal lambda
for(k in 1:10) {
  train_set <- edx[cv_splits[[k]],]
  test_set <- edx[-cv_splits[[k]],]
  
  # Make sure userId and movieId in test set are also in the train set
  test_final <- test_set %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  # Add rows removed from validation set back into edx set
  removed <- anti_join(test_set, test_final)
  train_final <- rbind(train_set, removed)
  
  mu <- mean(train_final$rating)
  
  rmses_2[k,] <- sapply(lambdas, function(l){
    b_i <- train_final %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+l))
    b_u <- train_final %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    predicted_ratings <- 
      test_final %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      pull(pred)
    return(RMSE(predicted_ratings, test_final$rating))
  })
}

rmses_2
rmses_2_cv <- colMeans(rmses_2)
rmses_2_cv
qplot(lambdas,rmses_2_cv)
lambdas[which.min(rmses_2_cv)]   #4.9


# 6.2 model generation and prediction
# Regularized Movie Effect and User Effect Model
# lambda <- lambdas[which.min(rmses_2_cv)]
lambda <- 4.9
mu <- mean(edx$rating)
b_i_reg <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda))
b_u_reg <- edx %>% 
    left_join(b_i_reg, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
predicted_ratings_6 <- 
    validation %>% 
    left_join(b_i_reg, by = "movieId") %>%
    left_join(b_u_reg, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
model_6_rmse <- RMSE(predicted_ratings_6, validation$rating)   # 0.864818
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized Movie + User Effect Model",  
                                     RMSE = model_6_rmse))
rmse_results 








## Model 7 ##
## regularization of movie and user effects: use dfferent lambdas
## optimizing lambda_u (user effect) with fixed lambda_i (movie effect)

# 7.1 perform cross validation to determine the parameter lambda_u for a given lambda_i

# define a matrix to store the results of cross validation
lambda_i <- 2.2
lambdas_u <- seq(0, 8, 0.1)
rmses_3 <- matrix(nrow=10,ncol=length(lambdas_u))

# perform 10-fold cross validation to determine the optimal lambda
for(k in 1:10) {
  train_set <- edx[cv_splits[[k]],]
  test_set <- edx[-cv_splits[[k]],]
  
  # Make sure userId and movieId in test set are also in the train set
  test_final <- test_set %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  # Add rows removed from validation set back into edx set
  removed <- anti_join(test_set, test_final)
  train_final <- rbind(train_set, removed)
  
  mu <- mean(train_final$rating)
  
  rmses_3[k,] <- sapply(lambdas_u, function(l){
    b_i <- train_final %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+lambda_i))
    b_u <- train_final %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    predicted_ratings <- 
      test_final %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      pull(pred)
    return(RMSE(predicted_ratings, test_final$rating))
  })
}
rmses_3
rmses_3_cv <- colMeans(rmses_3)
rmses_3_cv
qplot(lambdas_u,rmses_3_cv)
lambdas_u[which.min(rmses_3_cv)]   #5

# 7.2 model generation and prediction
# Regularized Movie and User Effect Model with fixed lambda for Movie Effect
lambda_i <- 2.2
# lambda_u <-lambdas_u[which.min(rmses_3_cv)] 
lambda_u <- 5
mu <- mean(edx$rating)
b_i_reg <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda_i))
b_u_reg <- edx %>% 
  left_join(b_i_reg, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda_u))
predicted_ratings_7 <- 
  validation %>% 
  left_join(b_i_reg, by = "movieId") %>%
  left_join(b_u_reg, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
model_7_rmse <- RMSE(predicted_ratings_7, validation$rating)   # 0.86485
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized Movie + User Effect Model Version 2",  
                                     RMSE = model_7_rmse))
rmse_results 










## Model 8 ##
## regularization of movie and user effects: use dfferent lambdas
## optimizing lambda_i (movie effect) with fixed lambda_u (user effect) 

# 8.1 perform cross validation to determine the parameter lambda_i for a given lambda_u

# define a matrix to store the results of cross validation
lambdas_i <- seq(0, 8, 0.1)
lambda_u <- 5
rmses_4 <- matrix(nrow=10,ncol=length(lambdas_i))

# perform 10-fold cross validation to determine the optimal lambda
for(k in 1:10) {
  train_set <- edx[cv_splits[[k]],]
  test_set <- edx[-cv_splits[[k]],]
  
  # Make sure userId and movieId in test set are also in the train set
  test_final <- test_set %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  # Add rows removed from validation set back into edx set
  removed <- anti_join(test_set, test_final)
  train_final <- rbind(train_set, removed)
  
  mu <- mean(train_final$rating)
  
  rmses_4[k,] <- sapply(lambdas_i, function(l){
    b_i <- train_final %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+l))
    b_u <- train_final %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+lambda_u))
    predicted_ratings <- 
      test_final %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      pull(pred)
    return(RMSE(predicted_ratings, test_final$rating))
  })
}

rmses_4
rmses_4_cv <- colMeans(rmses_4)
rmses_4_cv
qplot(lambdas_i,rmses_4_cv)
lambdas_i[which.min(rmses_4_cv)]   #4.6

# 8.2 model generation and prediction
# Regularized Movie and User Effect Model with fixed lambda for User Effect
# lambda_i <- lambdas_i[which.min(rmses_4_cv)]
lambda_i <- 4.6
lambda_u <- 5
mu <- mean(edx$rating)
b_i_reg <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda_i))
b_u_reg <- edx %>% 
  left_join(b_i_reg, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda_u))
predicted_ratings_8 <- 
  validation %>% 
  left_join(b_i_reg, by = "movieId") %>%
  left_join(b_u_reg, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
model_8_rmse <- RMSE(predicted_ratings_8, validation$rating)   # 0.864819
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized Movie + User Effect Model Version 3",  
                                     RMSE = model_8_rmse))
rmse_results 










## Model 9 ##
## Matrix Factorization based on the residuals of the baseline model

# 9.1 best baseline model
### Note: Models 1-8 are all baseline models based on movie effect and user effect
### I compared the rmses and determined to go with "Regularized Movie + User Effect Model" (model 6) as the baseline model prior to further exploration
# naive_rmse   # 1.0612
# model_2_rmse # 1.05239
# model_3_rmse # 0.943909
# model_4_rmse # 0.865349
# model_5_rmse # 0.943852
# model_6_rmse # 0.864818
# model_7_rmse # 0.86485
# model_8_rmse # 0.864819

# As a reminder the following is the code of Model 6 I used for calculating rmse based on the "validation" set
# lambda <- 4.9
# mu <- mean(edx$rating) # 3.51247
# b_i_reg <- edx %>% 
#   group_by(movieId) %>%
#   summarize(b_i = sum(rating - mu)/(n()+lambda))
# b_u_reg <- edx %>% 
#   left_join(b_i_reg, by="movieId") %>%
#   group_by(userId) %>%
#   summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
# predicted_ratings_6 <- 
#   validation %>% 
#   left_join(b_i_reg, by = "movieId") %>%
#   left_join(b_u_reg, by = "userId") %>%
#   mutate(pred = mu + b_i + b_u) %>%
#   pull(pred)
# model_6_rmse <- RMSE(predicted_ratings_6, validation$rating)   # 0.864818


# 9.2 calculating the residuals
# However, to calculate the residual we cann't use the "validation" set. Instead, we need to still use the training set "edx".
# what about rmse based on the "edx" set (training set)?
lambda <- 4.9
mu <- mean(edx$rating) # 3.51247
b_i_reg <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))
b_u_reg <- edx %>%
  left_join(b_i_reg, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

predicted_ratings_6_edx <- 
  edx %>% 
  left_join(b_i_reg, by = "movieId") %>%
  left_join(b_u_reg, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
model_6_rmse_edx <- RMSE(predicted_ratings_6_edx, edx$rating)   # 0.857039
model_6_rmse_edx

# all right, now let's get the residual of the prediction and then perform matrix factorization on the residual
# residual_baseline_edx <- edx$rating-predicted_ratings_6_edx
edx_residual <- edx %>% 
  left_join(b_i_reg, by = "movieId") %>%
  left_join(b_u_reg, by = "userId") %>%
  mutate(residual = rating - mu - b_i - b_u) %>%
  select(userId, movieId, residual)
validation_for_mf <- validation %>% 
  select(userId, movieId, rating)


# 9.3 use the recosystem library to perform the matrix factorization

# as matrix
edx_for_mf <- as.matrix(edx_residual)
validation_for_mf <- as.matrix(validation_for_mf)

# write edx_for_mf and validation_for_mf tables on disk
write.table(edx_for_mf , file = "trainset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(validation_for_mf, file = "validset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)

# use data_file() to specify a data set from a file in the hard disk. 
set.seed(2019) 
train_set <- data_file("trainset.txt")
valid_set <- data_file("validset.txt")

# build a recommender object
r <-Reco()

# tuning training set
opts <- r$tune(train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                     costp_l1 = 0, costq_l1 = 0,
                                     nthread = 1, niter = 10))
opts

# training the recommender model
r$train(train_set, opts = c(opts$min, nthread = 1, niter = 20))

# Making prediction on validation set and calculating RMSE:
pred_file <- tempfile()
r$predict(valid_set, out_file(pred_file))  
predicted_residuals_mf <- scan(pred_file)
predicted_ratings_mf <- predicted_ratings_5 + predicted_residuals_mf
rmse_mf <- RMSE(predicted_ratings_mf,validation$rating) # 0.786256
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Matrix Factorization",  
                                     RMSE = rmse_mf))
rmse_results 

# from the summarized rmses of different models, we can see that matrix factorization largely improved the accuracy of the prediction
final_rmses <- rmse_results 
final_rmses

kable(final_rmses) %>%
  kable_styling(bootstrap_options = "striped" , full_width = F , position = "center") %>%
  kable_styling(bootstrap_options = "bordered", full_width = F , position ="center") %>%
  column_spec(1,bold = F ) %>%
  column_spec(2,bold =T )



