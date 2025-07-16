# Simulating the Monty Hall Problem in R

simulate_monty_hall <- function(nsim = 10000) {
  stay_results <- numeric(nsim)
  switch_results <- numeric(nsim)

  total_stay_wins <- 0
  total_switch_wins <- 0

  for (i in 1:nsim) {
    
    car_door <- sample(1:3, 1)
    player_choice <- sample(1:3, 1)

    # Host opens a door revealing a goat
    doors_available <- setdiff(1:3, c(player_choice, car_door))
    host_opens <- if (player_choice == car_door) sample(doors_available, 1) else doors_available

    # Switch door
    switch_choice <- setdiff(1:3, c(player_choice, host_opens))

    total_stay_wins <- total_stay_wins + (player_choice == car_door)
    total_switch_wins <- total_switch_wins + (switch_choice == car_door)

    stay_results[i] <- total_stay_wins / i
    switch_results[i] <- total_switch_wins / i
  }

  list(
    stay_win_rate = stay_results,
    switch_win_rate = switch_results
  )
}

for (seed in 1:10) {

    set.seed(seed)
    results <- simulate_monty_hall()

    plot(results$stay_win_rate, type = "l", col = "red", 
      ylab = "Win Rate", xlab = "Simulations", 
      main = "Monty Hall Simulation: Stay vs Switch",
      ylim = c(0, 1))
    lines(results$switch_win_rate, col = "green")
    legend("topright", legend = c("Stay", "Switch"), col = c("red", "green"), lty = 1)

    Sys.sleep(2)
}
