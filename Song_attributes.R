getwd()
setwd("D:\\Data analysis I with R\\R- file reader")
genre_data <- read.csv("genre_data.csv", header = TRUE)

# Question 1: 

ML_rule <- function(x,y){
  if(length(x) != length(y)){stop("2 vectors does not have the same length")}
  
  m_x <- mean(x[y == "rock"], na.rm = TRUE)
  m_y <- mean(x[y == "pop"], na.rm = TRUE)
  
  average_mean <- (m_x+m_y)/2
  
  # i) and ii) 
  # R1 = Range of rock song 
  # R2 = Range of pop song 
  if(m_x < m_y) {
    R1 <- c(- Inf, average_mean)
    R2 <- c(average_mean, Inf)
  } else { 
    R1 <- c(average_mean, Inf) 
    R2 <- c(- Inf, average_mean)
  }
  # iii) - confusion matrix
  Predicted_genre <- ifelse( x < average_mean, "rock", "pop")
  confusion_matrix <- table(Predicted_genre, y)
  
  n12 <- confusion_matrix[1,2]
  n21 <- confusion_matrix[2,1]
  APER <- (n12 + n21)/(length(x)+length(y))
  # iv)
  mis_prob <- c(confusion_matrix[1,2]/length(y),confusion_matrix[2,1]/length(x))
  
  # return the list 
  return(list("R1" = R1, "R2" = R2, "confusion_matrix"= confusion_matrix,"miss_probabilities" = mis_prob , "APER" = APER))
}



# Question 2
R_danceability <- genre_data$danceability[genre_data$genre == "rock"]
P_danceability <- genre_data$danceability[genre_data$genre == "pop"]

max(genre_data$danceability)
min(genre_data$danceability)

plot(density(R_danceability), main = "Distribution of danceability",xlim = c(11,97.5),ylim = c(0.0,0.05), type = "l")
points(density(P_danceability), type = "l", col = "blue")


# Question 3

ML_dance = ML_rule(genre_data$danceability, genre_data$genre)
R1dance = ML_dance$R1
R2dance = ML_dance$R2

# Question 4
dance.cm = ML_dance$confusion_matrix
dance.pm = ML_dance$miss_probabilities
dance.pm
dance.aper = ML_dance$APER


#Question 5  
55 > R2dance[1]
z_q5 = -(mean(P_danceability)-mean(R_danceability))/(2*sd(R_danceability))
pnorm(z_q5, mean = 0, sd =1)


# Question 6
ML_cheer = ML_rule(genre_data$valence, genre_data$genre)
ML_cheer$miss_probabilities


# Question 7
ML_energy = ML_rule(genre_data$energy , genre_data$genre)
ML_energy

R_energy <- genre_data$energy[genre_data$genre == "rock"]
P_energy <- genre_data$energy[genre_data$genre == "pop"]


plot(density(R_energy), main = "Distribution of danceability",xlim = c(11,97.5),ylim = c(0.0,0.05), type = "l")
points(density(P_energy), type = "l", col = "blue")









# Question 8

# Alternative function to get misclassification probabilities
calculate_missclassification <- function(data) {
  n <- length(data)
  
  if (n > 0) {
    # Assuming first half of data belongs to population 1 and second half to population 2
    mu1 <- mean(data[1:(n/2)])
    mu2 <- mean(data[(n/2 + 1):n])
    sigma <- sd(data)
    
    p1_2 <- pnorm((mu1 - mu2) / (2 * sigma))
    p2_1 <- p1_2
    
    return(c(pb1_2 = p1_2, pb2_1 = p2_1))
  } else {
    # Handle case when the data is empty
    return(c(pb1_2 = NA, pb2_1 = NA))
  }
}

# simulation study with 1000 random normal distribution
set.seed(123)  # Set seed for reproducibility
simulations <- 1000
pb1_2_values <- numeric(simulations)
pb2_1_values <- numeric(simulations)

for (i in 1:simulations) {
  # Generate data
  set.seed(1 + i)
  data1 <- rnorm(150, mean = 1, sd = sqrt(3))
  data2 <- rnorm(150, mean = 2, sd = sqrt(3))
  simulated_data <- c(data1, data2)
  
  # missclassification probabilities of the simulation
  missclass_probs <- calculate_missclassification(simulated_data)
  
  pb1_2_values[i] <- missclass_probs["pb1_2"]
  pb2_1_values[i] <- missclass_probs["pb2_1"]
}

# True missclassification probabilities
true_pb1_2 <- pnorm((1 - 2) / (2 * sqrt(3)))
true_pb2_1 <- true_pb1_2

# Density plots for comparison
plot_density <- function(values, true_value, title) {
  density_values <- density(values)
  plot(density_values, main = title, col = "lightblue", lwd = 2)
  abline(v = true_value, col = "red", lwd = 2)
  legend("topright", legend = c("Simulated", "True"), col = c("lightblue", "red"), lwd = 2)
}

# Compare the distribution of pb1|2 with the true probability p1|2
plot_density(pb1_2_values, true_pb1_2, "Density Plot of pb1|2")

# Repeat the same for pb2|1
plot_density(pb2_1_values, true_pb2_1, "Density Plot of pb2|1")

