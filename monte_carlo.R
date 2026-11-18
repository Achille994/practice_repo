#' monte carlo generator
#'
#' @description
#' Function will generate random x/y coordinates in a user defined bounds
#'
#' @param n int # of points to be generated
#' @param xmin minimum x value
#' @param ymin minimum y value
#' @param xmax maximum x value
#' @param ymax maximum y value
#' 
#' @returns  Data frame with all the x/y coordinates
#'
#' @details
#' Function generates n amount of coordinates randomly (x,y)
#' X coordinates must be within xmin and xmax and y coordinates must be
#' within ymin and ymax. Runif() generates the coordinates.
#'
# This function generates random (x,y) coordinates within the user defined bounds
# User can choose how many coordinates they want
library(patchwork)
library(tidyverse)
# This function generates random (x,y) coordinates within the bounds
#
Monte_carlo <- function(n,xmin,ymin,xmax,ymax){
  
  coordinates <- data.frame(
    x= runif(n, xmin, xmax),
    y= runif(n, ymin, ymax)
  )
  return(coordinates)
}

# Identify points below the normal curve ----
points10$Below <- points10$y <= dnorm(points10$x)
points100$Below <- points100$y <= dnorm(points100$x)
points1000$Below <- points1000$y <= dnorm(points1000$x)
points10000$Below <- points10000$y <= dnorm(points10000$x)

# Plot with 10 points ----
plot10 <- ggplot(points10, aes(x = x, y = y)) +
  geom_point(aes(color = Below), size = 2) +
  stat_function(fun = dnorm, color = "black") +
  scale_color_manual(values = c("TRUE" = "#0000FF",
                                "FALSE" = "#FF0000")) +
  labs(title = "n = 10", x = "X", y = "Y",
       color = "Below Curve") +
  theme_minimal()

# Plot with 100 points ----
plot100 <- ggplot(points100, aes(x = x, y = y)) +
  geom_point(aes(color = Below), size = 1.5, alpha = 0.7) +
  stat_function(fun = dnorm, color = "black") +
  scale_color_manual(values = c("TRUE" = "#0000FF",
                                "FALSE" = "#FF0000")) +
  labs(title = "n = 100", x = "X", y = "Y",
       color = "Below Curve") +
  theme_minimal()

# Plot with 1000 points ----
plot1000 <- ggplot(points1000, aes(x = x, y = y)) +
  geom_point(aes(color = Below), size = 0.7, alpha = 0.5) +
  stat_function(fun = dnorm, color = "black") +
  scale_color_manual(values = c("TRUE" = "#0000FF",
                                "FALSE" = "#FF0000")) +
  labs(title = "n = 1000", x = "X", y = "Y",
       color = "Below Curve") +
  theme_minimal()

# Plot with 10000 points ----
plot10000 <- ggplot(points10000, aes(x = x, y = y)) +
  geom_point(aes(color = Below), size = 0.3, alpha = 0.2) +
  stat_function(fun = dnorm, color = "black") +
  scale_color_manual(values = c("TRUE" = "#0000FF",
                                "FALSE" = "#FF0000")) +
  labs(title = "n = 10000", x = "X", y = "Y",
       color = "Below Curve") +
  theme_minimal()

# Combine all four plots ----
plot10 + plot100 + plot1000 + plot10000