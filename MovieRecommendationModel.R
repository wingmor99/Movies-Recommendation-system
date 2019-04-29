library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)



dl <- tempfile()
download.file("https://drive.google.com/uc?export=download&id=1sqwFxdPPC3o9iBry6GOp7d7zM-u-sQh8", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-1m/ratings.dat"))),
                      col.names = c("UserID", "MovieID", "Rating", "Timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-1m/movies.dat")), "\\::", 3)
colnames(movies) <- c("MovieID", "MoviesNames", "Genres")
movies <- as.data.frame(movies) %>% mutate(MovieID = as.numeric(levels(MovieID))[MovieID],
                                            MoviesNames = as.character(MoviesNames),
                                            Genres = as.character(Genres))
ratings$Timestamp <- NULL
movies$MovieID <- as.character(movies$MovieID)
ratings$UserID <- as.character(ratings$UserID)
ratings$MovieID <- as.character(ratings$MovieID)
ratings$Rating <- as.character(ratings$Rating)
all_data <- left_join(ratings, movies, by = "MovieID") %>% mutate(year = str_sub(MoviesNames, -5, -2))

# adjust the data type
all_data$UserID <- as.numeric(all_data$UserID)
all_data$MovieID <- as.numeric(all_data$MovieID)
all_data$Rating <- as.numeric(all_data$Rating)
all_data$year <- as.numeric(all_data$year)

sep_data <- all_data %>% separate_rows(Genres, sep = "\\|") 


# plot the ratings change with year
all_data %>% group_by(year) %>%
  summarize(Rating = mean(Rating)) %>%
  ggplot(aes(year, Rating)) +
  geom_point() +
  geom_smooth()

# plot the rating of each genres 
sep_data %>% group_by(Genres) %>%
  summarize(n = n(), avg = mean(Rating), se = sd(Rating)/sqrt(n())) %>%
  mutate(Genres = reorder(Genres, avg)) %>%
  ggplot(aes(x = Genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Using bayes predict the ratings based on the genres
# Reload the movies file

#movies <- read.delim("movies.dat", header = FALSE) %>% 
#separate(., V1, into = c("MovieID", "MoviesNames", "Genres"), sep = "::") 

# Prepossing the dataset
nameVector <- c("Action", "Adventure", "Animation", "Children's", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western")
movies[,nameVector] <- 0
for (i in 1:length(nameVector)) {
  movies[which(str_detect(movies$Genres, nameVector[i])), i + 3] <- 1
}
movies$Genres <- NULL
movies$MoviesNames <- NULL
movies$MovieID <- as.numeric(movies$MovieID)

# Reload the ratings.dat file
# ratings <- read.delim("ratings.dat", header = FALSE) %>%
#  separate(., V1, into = c("UserID", "MovieID", "Rating", "Timestamp"), sep = "::")
ratings$Rating <- as.numeric(ratings$Rating)
ratings$MovieID <- as.numeric(ratings$MovieID)
ratings$UserID <- as.numeric(ratings$UserID)

# Calculate the average rating of each movie
movierating <- ratings %>% group_by(MovieID) %>% summarize(rating = mean(Rating)) 

# Combine the movierating and movies dataset
all_data <- left_join(movierating, movies, by = "MovieID")

all_data$MovieID <- NULL

rating_group <- all_data

# set the ratings label
rating_group$rating[(which(rating_group$rating <= 5 & rating_group$rating >= 4.5))] <- 8
rating_group$rating[(which(rating_group$rating < 4.5 & rating_group$rating >= 4))] <- 7
rating_group$rating[(which(rating_group$rating < 4 & rating_group$rating >= 3.5))] <- 6
rating_group$rating[(which(rating_group$rating < 3.5 & rating_group$rating >= 3))] <- 5
rating_group$rating[(which(rating_group$rating < 3 & rating_group$rating >= 2.5))] <- 4
rating_group$rating[(which(rating_group$rating < 2.5 & rating_group$rating >= 2))] <- 3
rating_group$rating[(which(rating_group$rating < 2 & rating_group$rating >= 1.5))] <- 2
rating_group$rating[(which(rating_group$rating < 1.5 & rating_group$rating >= 1))] <- 1

# split the traindata and testdata
set.seed(4)
trainvector <- sample(c(1:nrow(rating_group)), nrow(rating_group) * 0.8)
traindata <- rating_group[trainvector,]
testdata <- rating_group[-trainvector,]

# find the sucess of each rating group
bioBayes <- function(df) {
  for (i in 1:8) {
    group <- df[df$rating == i, ] %>% colSums()
    group[1] <- group[1] / i
    if (i == 1) {
      occrMatrix <- group
    }
    if (i > 1) {
      occrMatrix <- rbind(occrMatrix, group)
    }
  }
  rownames(occrMatrix) <- c("rating1", "rating2", "rating3", "rating4", "rating5", "rating6", "rating7", "rating8")
  
  return(occrMatrix)
}

# Bayes model
model <- bioBayes(traindata)

# Find errors of the model
error_funciton <- function(model, test_data) {
  # initialize the error_matrix
  error_matrix <- matrix(rep(0, 64), 8, 8)
  # initialize the error_prediction_number
  error_prediction_number <- c()
  # initialize the prediction_results
  prediction_results <- c()
  priors <- model[, 1] / sum(model[, 1])
  
  for (i in 1:nrow(test_data)) {
    # slice data pice by pice
    initial_vector <- data.frame(test_data[i, 2:19])
    posterior <- rep(0, 8)
    
    # get the posterior probability for each digit
    for (j in 1:8) {
      # get the means vector and standard_diviation vector
      s <- model[j, 2:19] / model[[j, 1]]
      
      # find the pixels which contains information and get the mean and sd
      nonzero <- s > 0
      initial <- initial_vector[nonzero]
      
      #################
      likelihood <- 1
      for (l in which(initial == 0)) {
        likelihood <- likelihood * (1 - s[[l]])
      }
      for (k in which(initial == 1)) {
        likelihood <- likelihood * s[[k]]
      }
      likelihood <- log(likelihood)
      posterior[j] <- likelihood + log(priors[[j]])
    }
    actual <- test_data[[i , 1 ]]
    predicted <- which.max(posterior)
    
    # save the predicted to the result
    prediction_results <- c(prediction_results, predicted)
    
    # add 1 in the error_matrix
    error_matrix[actual, predicted] <- error_matrix[actual, predicted] + 1
    
    # mark the index if the predicted is wrong
    if (actual != predicted) {
      error_prediction_number <- c(error_prediction_number, i)
    }
    
  }
  return(list(error_matrix, error_prediction_number, prediction_results))
  
}

error <- error_funciton(model, testdata)
error[1]
(accuracy <- sum(diag(error[[1]])) / sum(error[[1]]))

# Boosting the model
boosting_model <- function(train_data, test_data, model_function, error_funciton, iter) {
  
  # train models from errors from last model
  
  #initialize the model_list, iteration_train_data
  model_list <- list()
  iteration_train_data <- train_data
  
  for (i in 1:iter) {
    iteration_models <- model_function(iteration_train_data)
    model_list[[i]] <- iteration_models
    
    # use ALL TRAIN DATA test the iteration_models 
    iteration_model_result <- error_funciton(iteration_models, train_data)
    
    # renew the model data
    iteration_train_data <- train_data[iteration_model_result[[2]], ]
  }
  
  
  # get the results
  #initialize the results_matrix
  results_matrix <- matrix(0)
  
  for (i in 1:iter) {
    # get the results for each iteration
    iteration_results <- model_list[[i]] %>% error_funciton(., test_data) %>% .[[3]]
    
    # save each iteration results in row
    if (i == 1) {
      results_matrix <- matrix(iteration_results, nrow = 1)
    } else {
      results_matrix <- rbind(results_matrix, iteration_results)
    }
  }
  
  # get the final result, boosted_error_matrix, and accuracy
  final_result <- c()
  boosted_error_matrix <- matrix(rep(0, 64), 8 , 8)
  actual_results <- pull(test_data[ , 1])
  
  # get the final_result from each column
  for( i in 1:ncol(results_matrix)) {
    final_result[i] <- results_matrix[ , i] %>% table() %>% which.max() %>% names() 
  }
  
  for (i in 1:nrow(test_data)) {
    boosted_error_matrix[actual_results[[i]], as.integer(final_result[i])] <- boosted_error_matrix[actual_results[[i]], as.integer(final_result[i])] + 1
  }
  
  # ########
  accuracy <- sum(diag(boosted_error_matrix)) / sum(boosted_error_matrix)
  
  return(list(accuracy, boosted_error_matrix))
}

boost_2 <- boosting_model(train_data = traindata, test_data = testdata, model_function = bioBayes, error_funciton = error_funciton, 2)
boost_2[1]
boost_2[2]

boost_10 <- boosting_model(train_data = traindata, test_data = testdata, model_function = bioBayes, error_funciton = error_funciton, 10)
boost_10[1]
boost_10[2]
