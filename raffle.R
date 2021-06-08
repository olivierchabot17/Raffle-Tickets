# Set the working directory
directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(directory)

# Prizes value
v1 <- 1200
v2 <- 200
v3 <- 100

ticket_price <- 20

# Probabilities
probalities <- function(n = 1, N = 100){
  # Events 
  p_www <- (n/N) * ((n-1)/(N-1)) * ((n-2)/(N-2))
  p_wwl <- (n/N) * ((n-1)/(N-1)) * (((N-2)-(n-2))/(N-2))
  p_wlw <- (n/N) * (((N-1)-(n-1))/(N-1)) * ((n-1)/(N-2))
  p_wll <- (n/N) * (((N-1)-(n-1))/(N-1)) * (((N-2)-(n-1))/(N-2))
  p_lww <- ((N-n)/N) * ((n)/(N-1)) * ((n-1)/(N-2))
  p_lwl <- ((N-n)/N) * ((n)/(N-1)) * (((N-2)-(n-1))/(N-2))
  p_llw <- ((N-n)/N) * (((N-1)-n)/(N-1)) * (n/(N-2))
  p_lll <- ((N-n)/N) * (((N-1)-n)/(N-1)) * (((N-2)-n)/(N-2))
  
  # Probabilities
  p0 <- p_lll
  p1 <- p_llw
  p2 <- p_lwl
  p3 <- p_wll
  p12 <- p_lww
  p13 <- p_wlw
  p23 <- p_wwl
  p123 <- p_www
  p_total <- p_www + p_wwl + p_wlw + p_wll + p_lww + p_lwl + p_llw + p_lll
  
  # Expected Values
  e1 <- p1 * (v1 - (n*ticket_price))
  e2 <- p2 * (v2 - (n*ticket_price))
  e3 <- p3 * (v3 - (n*ticket_price))
  e0 <- p0 * (0 - (n*ticket_price))
  e12 <- p12 * (v1 + v2 - (n*ticket_price))
  e13 <- p13 * (v1 + v3 - (n*ticket_price))
  e23 <- p23 * (v2 + v3 - (n*ticket_price))
  e123 <- p123 * (v1 + v2 + v3 - (n*ticket_price))
  
  EV <- e1 + e2 + e3 + e0 + e12 + e13 + e23 + e123
  
  return(list(p1 = p1, p2 = p2, p3 = p3, p0 = p0, p_total = p_total,
              e1 = e1, e2 = e2, e3 = e3, e0 = e0, e12 = e12, e13 = e13, e23 = e23, e123 = e123, EV = EV))
}


# Simulation

# Number of replications
B <- 1000

# Empty vector of length B
winnings <- numeric(length = B)

# Set seed for replicability
set.seed(2021)

# Perform simulation
for(i in 1:B){
  winnings[i] <- sample(
    x = c(v1 - 1 * ticket_price, v2 - 1 * ticket_price, v3 - 1 * ticket_price, 0 - 1 * ticket_price),
    size = 1,
    prob = c(probalities()$p1, probalities()$p2, probalities()$p3, probalities()$p0),
    replace = FALSE
  )
}

# Create a datafram of the winnings
df <- data.frame(
  draw_number = 1:B,
  winnings = winnings
)

# Load libraries
library(tidyverse) # data wrangling (dplyr) & visualization (ggplot)
library(ggrepel) # offset labels in ggplot

# Calculate the cumulative statistics
df <- df %>%
  mutate(
    cumulative_sum = cumsum(winnings),
    cumulative_average = cumulative_sum / draw_number
  )

# Gety an overview of the results
summary(df)

# Define some labels

# Lowest point
min_cum_sum <- min(df$cumulative_sum)
x_min <- which(df$cumulative_sum == min_cum_sum)
y_min <- df$cumulative_average[x_min]

# Highest point
max_cum_sum <- max(df$cumulative_sum)
x_max <- which(df$cumulative_sum == max_cum_sum)
y_max <- df$cumulative_average[x_max]

# Last point
last_cum_sum <- df$cumulative_sum[B]
x_last <- B
y_last <- df$cumulative_average[B]

labels_df <- data.frame(
  x = c(x_min, x_max, x_last),
  y = c(y_min, y_max, y_last),
  cum_sum = c(min_cum_sum, max_cum_sum, last_cum_sum)
)

labels_df <- labels_df %>%
  mutate(label = paste(cum_sum, "$", sep = ""))

# Plot the average winnings per ticket over 1000 replications
ggplot(df, aes(x = draw_number, y = cumulative_average)) +
  geom_line() +
  geom_hline(yintercept = probalities()$EV, colour = "red", linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20, 50, by = 5)) +
  geom_point(data = labels_df, aes(x = x, y = y, col = label), show.legend = FALSE) +
  geom_label_repel(data = labels_df, aes(x = x, y = y, label = paste(label, "\n after", x), col = label), show.legend = FALSE) +
  scale_colour_manual(values = c("blue", "red", "green")) +
  theme_classic() +
  labs(
    title = "Average Dollars per Draw with 1 Ticket (100 Tickets Sold)",
    x = "Draw Number",
    y = "Average Winnings per ticket ($)"
  )


##### PART 2

# Expected value plot for 100 tickets sold
N = 100

EV_df <- data.frame(
  n = 0:N,
  N = rep(N, N+1)
) %>%
  mutate(
    EV = probalities(n, N)$EV
  )


ggplot(
  data = EV_df,
  aes(x = n, y = EV)
) +
  geom_line() +
  theme_bw() +
  labs(
    x = "Tickets Purchased (1 - 100)",
    y = "Expected Vlaue ($)",
    title = "Expected Value When 100 Tickets Are Sold",
    subtitle = "Buy More = Lose More"
  )

# Expected value plot for 100 tickets sold
N = 10

EV_df <- data.frame(
  n = 0:N,
  N = rep(N, N + 1)
) %>%
  mutate(
    EV = probalities(n, N)$EV
  )


ggplot(
  data = EV_df,
  aes(x = n, y = EV)
) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = 0:N, limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0,1300, 100), limits = c(0, 1300)) +
  labs(
    x = "Tickets Purchased (1 - 10)",
    y = "Expected Vlaue ($)",
    title = paste("Expected Value When", N, "Tickets Are Sold"),
    subtitle = "Buy More = Win More"
  )


