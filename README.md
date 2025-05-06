# Master

#Kode til Vågå

# ------------------------------------------------------
# TAMREINDRIFT I Vågå
# ------------------------------------------------------
Sys.setlocale("LC_ALL", "nb_NO.UTF-8")

params <- list(
  # Reproduksjon og overgangsparametere:
  birth_rate = 0.97,              # 97 % av simlene føder kalv
  p_calv_to_ungbukk = 0.50,       # Andel kalv som går over til ungbukk (Storbukk 3år)
  p_calv_to_simle = 0.50,         # Andel kalv som gårover til simlebestand
  p_ungbukk_storbukk = 0.33,      # Andel ungbukk som modnes til storbukk (>2 år)
  p_ungbukk_simle = 0.00,         # Andel ungbukk som går over til simle (satt til 0)
  
  # ??konomiske parametre (oppdatert ut fra regnskapsdata):
  price_kg = 165,                 # Kilopris (kr per kg)
  cost_kg = 25,                   # Variabel kostnad per kg
  
  # Yield gjennomsnittsvekt (kg) for slaktebar vekt (fra Tabell 11 for Vågå)
  yield = c(
    kalv = 27.4,
    ungbukk = 40.0,
    storbukk = 60.9,
    simle = 47.6
  ),
  
  # Faste kostnader (oppdatert ut fra regnskapsdata):
  fixed_costs = 5811235,          # Faste kostnader (kr)
  
  # Variable kostnader (lagerendringer og varekostnader):
  var_costs_range = list(
    inventory_change = c(-652500, 25099),  # Lagerendringer
    cost_of_goods_sold = c(800000, 1600000) # Varekostnader
  ),
  
  # Totalkostnader uten de variable kostnadene (for å beregne de faste kostnadene)
  total_costs = 6308903,           
  
  # Diskonteringsrate og simuleringslengde:
  delta = 0.1,
  T = 10,
  
  # Bestandsmål:
  lower_bound = 2200,
  upper_bound = 2300
)


# ------------------------------------------------------
# TOTAL 2308 dyr
# ------------------------------------------------------
N0 <- list(
  kalv     = 300,
  ungbukk  = 250,
  storbukk = 250,
  simle    = 1508
)

# ------------------------------------------------------
# 2) HJELPEFUNKSJON: DISKONTERINGSFAKTOR
# ------------------------------------------------------
discount <- function(r, t) {
  1 / ((1 + r)^(t - 1))
}

# ------------------------------------------------------
# 3) FUNKSJON FOR BESTANDSUTVIKLING MED STOKASTISKE DØDELIGHETSRATER
# ------------------------------------------------------
next_year_population <- function(kalv, ungbukk, storbukk, simle, harvest, params) {
  current_mu_kalv <- runif(1, min = 0.07, max = 0.13)
  current_mu_ungbukk <- runif(1, min = 0.08, max = 0.12)
  current_mu_storbukk <- runif(1, min = 0.06, max = 0.10)
  current_mu_simle <- runif(1, min = 0.08, max = 0.12)
  
  new_kalv <- params$birth_rate * simle
  
  next_kalv_raw     <- (kalv + new_kalv) * (1 - current_mu_kalv)
  next_ungbukk_raw  <- (ungbukk + params$p_calv_to_ungbukk * kalv) * (1 - current_mu_ungbukk)
  next_storbukk_raw <- (storbukk + params$p_ungbukk_storbukk * ungbukk) * (1 - current_mu_storbukk)
  next_simle_raw    <- (simle + params$p_calv_to_simle * kalv) * (1 - current_mu_simle)
  
  Hk <- min(harvest["kalv"], next_kalv_raw)
  Hu <- min(harvest["ungbukk"], next_ungbukk_raw)
  Hs <- min(harvest["storbukk"], next_storbukk_raw)
  Hm <- min(harvest["simle"], next_simle_raw)
  
  # Reduser bestandene med uttaket
  next_kalv     <- next_kalv_raw - Hk
  next_ungbukk  <- next_ungbukk_raw - Hu
  next_storbukk <- next_storbukk_raw - Hs
  next_simle    <- next_simle_raw - Hm
  
  total_after <- next_kalv + next_ungbukk + next_storbukk + next_simle
  overshoot   <- total_after - params$upper_bound
  if (overshoot > 0) {
    ratio_k <- next_kalv / total_after
    ratio_u <- next_ungbukk / total_after
    ratio_s <- next_storbukk / total_after
    ratio_m <- next_simle / total_after
    adjustment <- overshoot  
    
    next_kalv     <- next_kalv - adjustment * ratio_k
    next_ungbukk  <- next_ungbukk - adjustment * ratio_u
    next_storbukk <- next_storbukk - adjustment * ratio_s
    next_simle    <- next_simle - adjustment * ratio_m
    
    # (Valgfritt) Juster de faktiske uttakene tilsvarende:
    Hk <- Hk + adjustment * ratio_k
    Hu <- Hu + adjustment * ratio_u
    Hs <- Hs + adjustment * ratio_s
    Hm <- Hm + adjustment * ratio_m
  }
  
  return(list(
    kalv     = next_kalv,
    ungbukk  = next_ungbukk,
    storbukk = next_storbukk,
    simle    = next_simle,
    harvest_k = Hk,
    harvest_u = Hu,
    harvest_s = Hs,
    harvest_m = Hm
  ))
}

# ------------------------------------------------------
# 4) SIMULERING MED FASTE SLAKTEFRASJONER
# ------------------------------------------------------
simulate_with_fixed_fractions <- function(N0, params, frac_k, frac_u, frac_s, frac_m) {
  T <- params$T
  results <- data.frame(
    year = 0:T,
    kalv = numeric(T + 1),
    ungbukk = numeric(T + 1),
    storbukk = numeric(T + 1),
    simle = numeric(T + 1),
    harvest_k = numeric(T + 1),
    harvest_u = numeric(T + 1),
    harvest_s = numeric(T + 1),
    harvest_m = numeric(T + 1),
    harvest_weight = numeric(T + 1),  
    profit = numeric(T + 1)           
  )
  
  # Sett startbestand
  results$kalv[1]     <- N0$kalv
  results$ungbukk[1]  <- N0$ungbukk
  results$storbukk[1] <- N0$storbukk
  results$simle[1]    <- N0$simle
  
  for (t in 1:T) {
    K0 <- results$kalv[t]
    U0 <- results$ungbukk[t]
    S0 <- results$storbukk[t]
    M0 <- results$simle[t]
    
    # Planlagt høstning:
    planned_harvest <- c(
      kalv     = frac_k * K0,
      ungbukk  = frac_u * U0,
      storbukk = frac_s * S0,
      simle    = frac_m * M0
    )
    
    next_pop <- next_year_population(K0, U0, S0, M0, harvest = planned_harvest, params = params)
    
    results$kalv[t + 1]     <- next_pop$kalv
    results$ungbukk[t + 1]  <- next_pop$ungbukk
    results$storbukk[t + 1] <- next_pop$storbukk
    results$simle[t + 1]    <- next_pop$simle
    
    results$harvest_k[t + 1] <- next_pop$harvest_k
    results$harvest_u[t + 1] <- next_pop$harvest_u
    results$harvest_s[t + 1] <- next_pop$harvest_s
    results$harvest_m[t + 1] <- next_pop$harvest_m
    
    #: trekk en verdi mellom 25 og 28 kg
    current_yields <- params$yield
    current_yields["kalv"] <- runif(1, 25, 28)
    
    # Beregn total slaktevekt for året (kg)
    hw <- next_pop$harvest_k * current_yields["kalv"] +
      next_pop$harvest_u * current_yields["ungbukk"] +
      next_pop$harvest_s * current_yields["storbukk"] +
      next_pop$harvest_m * current_yields["simle"]
    results$harvest_weight[t + 1] <- hw
    
    # Økonomi: Beregn revenue og profit
    net_price <- params$price_kg - params$cost_kg  
    revenue <- hw * net_price
    
    # Faste kostnader 
    fixed_cost <- params$fixed_costs  
    results$profit[t + 1] <- discount(params$delta, t) * (revenue - fixed_cost)
  }
  
  results
}


# ------------------------------------------------------
# 5) FAST SLAKTEUTTAK 
# ------------------------------------------------------
#   Simle: ca. 70 %
slakte_fractions_vaga <- c(
  kalv     = 0.14,
  ungbukk  = 0.08,
  storbukk = 0.08,
  simle    = 0.70
)

# ------------------------------------------------------
# 6) MåLFUNKSJON FOR OPTIMERING (MAKSIMERER NPV)
# ------------------------------------------------------
objective_function <- function(x, N0, params) {
  # x = c(frac_k, frac_u, frac_s, frac_m)
  frac_k <- x[1]
  frac_u <- x[2]
  frac_s <- x[3]
  frac_m <- x[4]
  
  if (any(x < 0) || any(x > 1)) {
    return(1e12)
  }
  
  sim_data <- simulate_with_fixed_fractions(N0, params, frac_k, frac_u, frac_s, frac_m)
  total_NPV <- sum(sim_data$profit)

  return(-total_NPV)
}

# ------------------------------------------------------
# 7) OPTIMERING AV SLAKTEUTTAK (H??STFRASJONER)
# ------------------------------------------------------
start_values <- c(0.1, 0.05, 0.05, 0.1)  

opt_result <- optim(
  par = start_values,
  fn  = objective_function,
  N0  = N0,
  params = params,
  method = "L-BFGS-B",
  lower = c(0, 0, 0, 0),
  upper = c(1, 0.10, 0.10, 1)
)

print(opt_result)
best_fracs <- opt_result$par
cat("Optimale slaktefraksjoner:\n")
cat("  kalv =", round(best_fracs[1], 3), "\n")
cat("  ungbukk =", round(best_fracs[2], 3), "\n")
cat("  storbukk =", round(best_fracs[3], 3), "\n")
cat("  simle =", round(best_fracs[4], 3), "\n")

# ------------------------------------------------------
# 8) SIMULERING MED "OPTIMALE" FRASJONER
# ------------------------------------------------------
sim_opt <- simulate_with_fixed_fractions(N0, params,
                                         frac_k = best_fracs[1],
                                         frac_u = best_fracs[2],
                                         frac_s = best_fracs[3],
                                         frac_m = best_fracs[4])
total_NPV_opt <- sum(sim_opt$profit)
cat("Total diskontert lønnsomhet (NPV) med optimale fraksjoner:", round(total_NPV_opt, 2), "kr\n")

# Beregn totalbestand per år
sim_opt$Total <- sim_opt$kalv + sim_opt$ungbukk + sim_opt$storbukk + sim_opt$simle

# ------------------------------------------------------
# 9) SCENARIOANALYSE (variasjon i bestandsmål)
# ------------------------------------------------------
upper_bounds <- c(1500, 1700, 1800, 1900, 2100, 2300)
scenario_results <- list()

for (ub in upper_bounds) {
  params$upper_bound <- ub
  
  sim_data <- simulate_with_fixed_fractions(
    N0, params,
    frac_k = best_fracs[1],
    frac_u = best_fracs[2],
    frac_s = best_fracs[3],
    frac_m = best_fracs[4]
  )
  
  # Beregn totalbestand og kumulativ NPV
  sim_data$Total       <- sim_data$kalv + sim_data$ungbukk + sim_data$storbukk + sim_data$simle
  sim_data$cumNPV      <- cumsum(sim_data$profit)
  sim_data$Bestandsmal <- ub
  
  scenario_results[[as.character(ub)]] <- sim_data
}

# Kombiner alle scenarioer til ett datasett
combined_data <- do.call(rbind, scenario_results)


# ------------------------------------------------------
# 10) PLOTTING
# ------------------------------------------------------
library(ggplot2)
library(scales)
library(patchwork)

# Plot 1: Totalbestand over tid
p_total <- ggplot(sim_opt, aes(x = year, y = Total)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue") +
  geom_hline(yintercept = 2200, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 2300, color = "red", linetype = "dashed") +
  labs(
    title = "Simulert totalbestand i Vågå",
    subtitle = "Bestandsmål etter slakting: ca. 2300 dyr",
    x = "År",
    y = "Antall dyr"
  ) +
  theme_minimal()

# Plot 2: Kumulativ diskontert NPV
p_npv <- ggplot(sim_opt, aes(x = year, y = cumsum(profit))) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_point(color = "darkgreen") +
  scale_y_continuous(labels = label_number(big.mark = " ", suffix = " kr", accuracy = 1)) +
  labs(
    title = "Kumulativ diskontert lønnsomhet (NPV)",
    x = "År",
    y = "NPV (kr)"
  ) +
  theme_minimal()

# Sl?? sammen plott under hverandre
combined_plot <- p_total / p_npv

# Vis samlet figur
combined_plot

print(p_total)
print(p_npv)


library(ggplot2)
library(scales)
library(patchwork)

# Plot 1: Totalbestand under ulike bestandsmål
p_scenario <- ggplot(combined_data, aes(x = year, y = Total)) +
  geom_line(linewidth = 1.2, color = "blue") +
  facet_wrap(~ Bestandsmal, scales = "free_y", ncol = 2) +
  labs(
    title = "Totalbestand under ulike bestandsmål",
    x = "År",
    y = "Antall dyr"
  ) +
  theme_minimal()

# Plot 2: Kumulativ NPV under ulike bestandsmål
p_npv_scenario <- ggplot(combined_data, aes(x = year, y = cumNPV)) +
  geom_line(linewidth = 1.2, color = "darkgreen") +
  facet_wrap(~ Bestandsmal, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = label_number(big.mark = " ", suffix = " kr", accuracy = 1)) +
  labs(
    title = "Kumulativ NPV under ulike bestandsmål",
    x = "År",
    y = "Kumulativ NPV (kr)"
  ) +
  theme_minimal(base_size = 12)

combined_scenario_plot <- p_scenario / p_npv_scenario

combined_scenario_plot

# ------------------------------------------------------
# Sjokk 25% 
# ------------------------------------------------------

sim_shock <- simulate_with_fixed_fractions(N0, params, best_fracs[1], best_fracs[2], best_fracs[3], best_fracs[4])

shock_year <- 5
loss_fraction <- 0.25  

# Reduser bestandene
sim_shock$kalv[shock_year + 1]     <- sim_shock$kalv[shock_year + 1] * (1 - loss_fraction)
sim_shock$ungbukk[shock_year + 1]  <- sim_shock$ungbukk[shock_year + 1] * (1 - loss_fraction)
sim_shock$storbukk[shock_year + 1] <- sim_shock$storbukk[shock_year + 1] * (1 - loss_fraction)
sim_shock$simle[shock_year + 1]    <- sim_shock$simle[shock_year + 1] * (1 - loss_fraction)

# Reduser slakteuttak
sim_shock$harvest_k[shock_year + 1] <- sim_shock$harvest_k[shock_year + 1] * (1 - loss_fraction)
sim_shock$harvest_u[shock_year + 1] <- sim_shock$harvest_u[shock_year + 1] * (1 - loss_fraction)
sim_shock$harvest_s[shock_year + 1] <- sim_shock$harvest_s[shock_year + 1] * (1 - loss_fraction)
sim_shock$harvest_m[shock_year + 1] <- sim_shock$harvest_m[shock_year + 1] * (1 - loss_fraction)

# Oppdater slaktevekt
current_yields <- params$yield
current_yields["kalv"] <- runif(1, 25, 28)

sim_shock$harvest_weight[shock_year + 1] <- sim_shock$harvest_k[shock_year + 1] * current_yields["kalv"] +
  sim_shock$harvest_u[shock_year + 1] * current_yields["ungbukk"] +
  sim_shock$harvest_s[shock_year + 1] * current_yields["storbukk"] +
  sim_shock$harvest_m[shock_year + 1] * current_yields["simle"]

# Oppdater profit
net_price <- params$price_kg - params$cost_kg
revenue <- sim_shock$harvest_weight[shock_year + 1] * net_price
fixed_cost <- params$fixed_costs

sim_shock$profit[shock_year + 1] <- discount(params$delta, shock_year) * (revenue - fixed_cost)

# Beregn Totalbestand og Kumulativ NPV
sim_shock$Total <- sim_shock$kalv + sim_shock$ungbukk + sim_shock$storbukk + sim_shock$simle
sim_shock$cumNPV <- cumsum(sim_shock$profit)

# ------------------------------------------------------
# Plot kun sjokk-scenario
# ------------------------------------------------------

library(ggplot2)
library(scales)
library(patchwork)

# Bestand
p_total <- ggplot(sim_shock, aes(x = year, y = Total)) +
  geom_line(color = "firebrick", linewidth = 1.2) +
  geom_point(color = "firebrick") +
  geom_vline(xintercept = shock_year, linetype = "dashed", color = "black") +
  labs(
    title = "Totalbestand med sjokk i år 5 (25 % tap)",
    x = "År",
    y = "Antall dyr"
  ) +
  theme_minimal(base_size = 13)

# NPV
p_npv <- ggplot(sim_shock, aes(x = year, y = cumNPV)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_point(color = "darkgreen") +
  geom_vline(xintercept = shock_year, linetype = "dashed", color = "black") +
  scale_y_continuous(labels = label_number(suffix = " kr", big.mark = " ", accuracy = 1)) +  
  labs(
    title = "Kumulativ NPV med sjokk i år 5 (25 % tap)",
    x = "År",
    y = "Kumulativ NPV"
  ) +
  theme_minimal(base_size = 13)

# Kombiner
p_total / p_npv + plot_layout(guides = "collect")


# Skriv ut NPV etter 10 
final_npv <- tail(sim_shock$cumNPV, 1)
print(paste0("Kumulativ NPV etter 10 År med sjokk: ", format(round(final_npv, 0), big.mark = " "), " kr"))




#Kode til Rendalen

# Sett norsk tegnsett Rendalen
Sys.setlocale("LC_ALL", "nb_NO.UTF-8")

# ------------------------------------------------------
# 1) PARAMETERE OG STARTBESTAND
# ------------------------------------------------------
params <- list(
  # Overgangs- og dødelighetsparametere
  p_kalv_ungbukk     = 0.30,
  p_kalv_simle       = 0.50,
  p_ungbukk_storbukk = 0.50,
  p_ungbukk_simle    = 0.00,
  
  mu_kalv    = 0.04,
  mu_ungbukk = 0.04,
  mu_storbukk= 0.04,
  mu_simle   = 0.04,
  
  # Pris (p) og variable kostnader (c) per dyr
  p = c(kalv = 6000, ungbukk = 12000, storbukk = 20000, simle = 9000),
  c = c(kalv = 861,  ungbukk = 861,   storbukk = 861,   simle = 861),
  
  # Areal og arealleie (faste kostnader knyttet til areal)
  area = 2300000,          # 2,3 millioner dekar
  area_lease_rate = 0.7,   # kr per dekar

  other_fixed_costs = 603992,
  
  # Diskonteringsrate og simuleringslengde
  delta = 0.05,
  T     = 10,
  
  # Bestandsgrenser
  lower_bound = 1600,
  upper_bound = 2000
)

N0 <- list(
  kalv     = 295,
  ungbukk  = 221,
  storbukk = 221,
  simle    = 737
)

# ------------------------------------------------------
# 2) HJELPEFUNKSJONER: DISKONTERING OG BESTANDSUTVIKLING
# ------------------------------------------------------

discount <- function(r, t) {
  1 / ((1 + r) ^ (t - 1))
}


next_year_population <- function(kalv, ungbukk, storbukk, simle, harvest, params, kalv_ratio) {
  new_kalv <- kalv_ratio * simle
  
  next_kalv     <- kalv     + new_kalv                         - params$mu_kalv    * kalv
  next_ungbukk  <- ungbukk  + params$p_kalv_ungbukk * kalv     - params$mu_ungbukk * ungbukk
  next_storbukk <- storbukk + params$p_ungbukk_storbukk * ungbukk - params$mu_storbukk* storbukk
  next_simle    <- simle    + params$p_kalv_simle    * kalv     - params$mu_simle   * simle
  
  Hk <- min(harvest["kalv"],     next_kalv)
  Hu <- min(harvest["ungbukk"],  next_ungbukk)
  Hs <- min(harvest["storbukk"], next_storbukk)
  Hm <- min(harvest["simle"],    next_simle)
  
  next_kalv     <- next_kalv - Hk
  next_ungbukk  <- next_ungbukk - Hu
  next_storbukk <- next_storbukk - Hs
  next_simle    <- next_simle - Hm
  
  total_after <- next_kalv + next_ungbukk + next_storbukk + next_simle
  overshoot   <- total_after - params$upper_bound
  
  if (overshoot > 0) {
    adjustment <- 0.8 * overshoot
    ratios <- c(next_kalv, next_ungbukk, next_storbukk, next_simle) / total_after
    
    next_kalv     <- next_kalv     - adjustment * ratios[1]
    next_ungbukk  <- next_ungbukk  - adjustment * ratios[2]
    next_storbukk <- next_storbukk - adjustment * ratios[3]
    next_simle    <- next_simle    - adjustment * ratios[4]
    
    Hk <- Hk + adjustment * ratios[1]
    Hu <- Hu + adjustment * ratios[2]
    Hs <- Hs + adjustment * ratios[3]
    Hm <- Hm + adjustment * ratios[4]
  }
  
  list(
    kalv     = next_kalv,
    ungbukk  = next_ungbukk,
    storbukk = next_storbukk,
    simle    = next_simle,
    harvest_k = Hk,
    harvest_u = Hu,
    harvest_s = Hs,
    harvest_m = Hm
  )
}

calc_N_plus <- function(kalv, ungbukk, storbukk, simle, params, kalv_ratio) {
  new_kalv <- kalv_ratio * simle
  
  Np_k <- kalv     + new_kalv                         - params$mu_kalv    * kalv
  Np_u <- ungbukk  + params$p_kalv_ungbukk * kalv     - params$mu_ungbukk * ungbukk
  Np_s <- storbukk + params$p_ungbukk_storbukk * ungbukk - params$mu_storbukk* storbukk
  Np_m <- simle    + params$p_kalv_simle    * kalv     - params$mu_simle   * simle
  
  list(kalv= Np_k, ungbukk= Np_u, storbukk= Np_s, simle= Np_m)
}

apply_harvest_and_adjust <- function(Np, fracs, params) {
  planned <- c(
    kalv     = fracs$k * Np$kalv,
    ungbukk  = fracs$u * Np$ungbukk,
    storbukk = fracs$s * Np$storbukk,
    simle    = fracs$m * Np$simle
  )
  # Faktisk uttak
  avail <- c(kalv= Np$kalv, ungbukk= Np$ungbukk, storbukk= Np$storbukk, simle= Np$simle)
  H     <- pmin(planned, avail)
  
  # Oppdatert bestand etter uttak
  N1 <- avail - H
  
  # Overshoot-justering
  total_after <- sum(N1)
  overshoot   <- total_after - params$upper_bound
  if (overshoot > 0) {
    adj    <- 0.8 * overshoot
    ratios <- N1 / total_after
    N1     <- N1 - adj * ratios
    H      <- H  + adj * ratios
  }
  
  list(N1 = N1, H = H)
}

# ------------------------------------------------------
# 3) SIMULERING MED FASTE HøSTFRASJONER
# ------------------------------------------------------
simulate_with_fixed_fractions <- function(N0, params, frac_k, frac_u, frac_s, frac_m, random_kalv_ratio = FALSE) {
  T <- params$T
  area_cost <- params$area * params$area_lease_rate
  
  results <- data.frame(
    year      = 0:T,
    kalv      = numeric(T + 1),
    ungbukk   = numeric(T + 1),
    storbukk  = numeric(T + 1),
    simle     = numeric(T + 1),
    harvest_k = numeric(T + 1),
    harvest_u = numeric(T + 1),
    harvest_s = numeric(T + 1),
    harvest_m = numeric(T + 1),
    profit    = numeric(T + 1)
  )
  
  # Startbestand
  results$kalv[1]     <- N0$kalv
  results$ungbukk[1]  <- N0$ungbukk
  results$storbukk[1] <- N0$storbukk
  results$simle[1]    <- N0$simle
  
  for (t in 1:T) {
    K0 <- results$kalv[t]
    U0 <- results$ungbukk[t]
    S0 <- results$storbukk[t]
    M0 <- results$simle[t]
    
    kalv_ratio_t <- if (random_kalv_ratio) runif(1, 0.1, 0.5) else 0.3
    

    Np <- calc_N_plus(K0, U0, S0, M0, params, kalv_ratio_t)
    
    fracs <- list(k = frac_k, u = frac_u, s = frac_s, m = frac_m)
    out   <- apply_harvest_and_adjust(Np, fracs, params)
    

    results$harvest_k[t+1] <- out$H["kalv"]
    results$harvest_u[t+1] <- out$H["ungbukk"]
    results$harvest_s[t+1] <- out$H["storbukk"]
    results$harvest_m[t+1] <- out$H["simle"]
    
    results$kalv[t+1]     <- out$N1["kalv"]
    results$ungbukk[t+1]  <- out$N1["ungbukk"]
    results$storbukk[t+1] <- out$N1["storbukk"]
    results$simle[t+1]    <- out$N1["simle"]

    net_k <- params$p["kalv"]    - params$c["kalv"]
    net_u <- params$p["ungbukk"] - params$c["ungbukk"]
    net_s <- params$p["storbukk"]- params$c["storbukk"]
    net_m <- params$p["simle"]   - params$c["simle"]
    
    revenue <- out$H["kalv"]    * net_k +
      out$H["ungbukk"] * net_u +
      out$H["storbukk"]* net_s +
      out$H["simle"]   * net_m -
      area_cost -
      params$other_fixed_costs
    
    results$profit[t+1] <- discount(params$delta, t) * revenue
  }
  
  results
}

# ------------------------------------------------------
# 4) OPTIMERING
# ------------------------------------------------------
objective_function <- function(x, N0, params, random_kalv_ratio = FALSE) {
  frac_k <- x[1]; frac_u <- x[2]; frac_s <- x[3]; frac_m <- x[4]
  if (any(x < 0) || any(x > 1)) return(1e12)
  
  sim_data  <- simulate_with_fixed_fractions(N0, params, frac_k, frac_u, frac_s, frac_m, random_kalv_ratio)
  total_NPV <- sum(sim_data$profit)
  
  -total_NPV
}

# ------------------------------------------------------
# 5) OPTIMERING
# ------------------------------------------------------
start_values <- c(0.1, 0.1, 0.1, 0.1)

opt_result <- optim(
  par    = start_values,
  fn     = objective_function,
  N0     = N0,
  params = params,
  method = "L-BFGS-B",
  lower  = c(0, 0, 0, 0),
  upper  = c(1, 1, 1, 1)
)

print(opt_result)

# ------------------------------------------------------
# 6) SIMULER MED "OPTIMALE" FRAKSJONER OG PLOTT RESULTATENE
# ------------------------------------------------------
best_fracs <- opt_result$par
cat("Optimale h??stfraksjoner:\n",
    "kalv =", round(best_fracs[1], 3), "\n",
    "ungbukk =", round(best_fracs[2], 3), "\n",
    "storbukk =", round(best_fracs[3], 3), "\n",
    "simle =", round(best_fracs[4], 3), "\n")

sim_opt <- simulate_with_fixed_fractions(N0, params,
                                         frac_k             = best_fracs[1],
                                         frac_u             = best_fracs[2],
                                         frac_s             = best_fracs[3],
                                         frac_m             = best_fracs[4],
                                         random_kalv_ratio  = FALSE)

total_NPV_opt <- sum(sim_opt$profit)
cat("Total diskontert l??nnsomhet med optimale fraksjoner:", round(total_NPV_opt, 2), "kr\n")

library(ggplot2)
library(patchwork)

p1 <- ggplot(sim_opt, aes(x = year, y = cumsum(profit))) +
  geom_line(linewidth = 1.3, color = "darkgreen") +
  geom_point(size = 2, color = "darkgreen") +
  scale_y_continuous(labels = scales::label_number(big.mark = " ", suffix = " kr", accuracy = 1)) +
  labs(
    title = "Kumulativ n??verdi over 10 ??r",
    x = "??r",
    y = "Kumulativ NPV (kroner)"
  ) +
  theme_minimal()

p2 <- ggplot(sim_opt, aes(x = year, y = kalv + ungbukk + storbukk + simle)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = params$lower_bound, linetype = "dashed", color = "red") +
  geom_hline(yintercept = params$upper_bound, linetype = "dashed", color = "red") +
  labs(
    title = "Totalbestand over tid",
    x = "??r",
    y = "Antall dyr"
  ) +
  theme_minimal()

# Vis dem sammen
p1 / p2


# ------------------------------------------------------
# SCENARIOANALYSE 
# ------------------------------------------------------
library(tidyr); library(scales)

upper_bounds <- c(1000, 1200, 1400, 1600, 1800, 2000)
scenario_results <- list()

for (ub in upper_bounds) {
  lb <- 0.8 * ub
  params$upper_bound <- ub
  params$lower_bound <- lb
  
  sim_data <- simulate_with_fixed_fractions(
    N0                = N0,
    params            = params,
    frac_k            = best_fracs[1],
    frac_u            = best_fracs[2],
    frac_s            = best_fracs[3],
    frac_m            = best_fracs[4],
    random_kalv_ratio = TRUE
  )
  
  sim_data$Total  <- with(sim_data, kalv + ungbukk + storbukk + simle)
  sim_data$cumNPV <- cumsum(sim_data$profit)
  sim_data$Bestandsmal <- ub
  sim_data$LowerBound  <- lb
  sim_data$UpperBound  <- ub
  
  scenario_results[[as.character(ub)]] <- sim_data
}

combined_data <- do.call(rbind, scenario_results)

#
library(ggplot2)
library(scales)
library(patchwork)

# Plot 1: 
p_totalbestand <- ggplot(combined_data, aes(x = year, y = Total)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ Bestandsmal, scales = "free_y", ncol = 2) +
  geom_hline(aes(yintercept = LowerBound), linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = UpperBound), linetype = "dashed", color = "red") +
  labs(
    title = "Totalbestand for ulike bestandsm??l",
    x = "??r",
    y = "Antall dyr"
  ) +
  theme_minimal()

# Plot 2: 
p_npv <- ggplot(combined_data, aes(x = year, y = cumNPV)) +
  geom_line(color = "darkgreen") +
  facet_wrap(~ Bestandsmal, scales = "free_y", ncol = 2) +
  scale_y_continuous(
    labels = label_number(big.mark = " ", suffix = " kr", accuracy = 1)
  ) +
  labs(
    title = "Kumulativ NPV for ulike bestandsm??l",
    x = "??r",
    y = "Kumulativ NPV (kroner)"
  ) +
  theme_minimal(base_size = 12)

combined_plot <- p_totalbestand / p_npv

combined_plot


# Plot 1: 
ggplot(combined_data, aes(x = year, y = Total)) +
  geom_line() + facet_wrap(~ Bestandsmal, scales="free_y", ncol=2) +
  geom_hline(aes(yintercept=LowerBound), linetype="dashed") +
  geom_hline(aes(yintercept=UpperBound), linetype="dashed") +
  labs(title="Totalbestand for ulike bestandsmål", x="år", y="Antall dyr") +
  theme_minimal()


library(ggplot2)
library(scales)
# Plot 2: 
ggplot(combined_data, aes(x = year, y = cumNPV)) +
  geom_line() +
  facet_wrap(~ Bestandsmal, scales = "free_y", ncol = 2) +
  scale_y_continuous(
    labels = label_number(big.mark = " ", accuracy = 1)
  ) +
  labs(
    title = "Kumulativ NPV for ulike bestandsmål",
    x     = "år",
    y     = "NPV (NOK)"
  ) +
  theme_minimal(base_size = 12)


# ------------------------------------------------------
#Sjokk
# ------------------------------------------------------

sim_rendalen <- simulate_with_fixed_fractions(N0, params, best_fracs[1], best_fracs[2], best_fracs[3], best_fracs[4])

# Definer sjokk
shock_year <- 5
loss_fraction <- 0.25  # 25 % tap


sim_rendalen$kalv[shock_year + 1]     <- sim_rendalen$kalv[shock_year + 1] * (1 - loss_fraction)
sim_rendalen$ungbukk[shock_year + 1]  <- sim_rendalen$ungbukk[shock_year + 1] * (1 - loss_fraction)
sim_rendalen$storbukk[shock_year + 1] <- sim_rendalen$storbukk[shock_year + 1] * (1 - loss_fraction)
sim_rendalen$simle[shock_year + 1]    <- sim_rendalen$simle[shock_year + 1] * (1 - loss_fraction)


sim_rendalen$harvest_k[shock_year + 1] <- sim_rendalen$harvest_k[shock_year + 1] * (1 - loss_fraction)
sim_rendalen$harvest_u[shock_year + 1] <- sim_rendalen$harvest_u[shock_year + 1] * (1 - loss_fraction)
sim_rendalen$harvest_s[shock_year + 1] <- sim_rendalen$harvest_s[shock_year + 1] * (1 - loss_fraction)
sim_rendalen$harvest_m[shock_year + 1] <- sim_rendalen$harvest_m[shock_year + 1] * (1 - loss_fraction)

# Oppdater profit etter sjokk
net_k <- params$p["kalv"] - params$c["kalv"]
net_u <- params$p["ungbukk"] - params$c["ungbukk"]
net_s <- params$p["storbukk"] - params$c["storbukk"]
net_m <- params$p["simle"] - params$c["simle"]

area_cost <- params$area * params$area_lease_rate
fixed_costs <- params$other_fixed_costs

revenue <- sim_rendalen$harvest_k[shock_year + 1] * net_k +
  sim_rendalen$harvest_u[shock_year + 1] * net_u +
  sim_rendalen$harvest_s[shock_year + 1] * net_s +
  sim_rendalen$harvest_m[shock_year + 1] * net_m -
  area_cost -
  fixed_costs

sim_rendalen$profit[shock_year + 1] <- discount(params$delta, shock_year) * revenue

# Lag total bestand og kumulativ NPV
sim_rendalen$Total <- sim_rendalen$kalv + sim_rendalen$ungbukk + sim_rendalen$storbukk + sim_rendalen$simle
sim_rendalen$cumNPV <- cumsum(sim_rendalen$profit)


library(ggplot2)
library(scales)
library(patchwork)

# Totalbestand
p_total_rendalen <- ggplot(sim_rendalen, aes(x = year, y = Total)) +
  geom_line(color = "firebrick", linewidth = 1.2) +
  geom_point(color = "firebrick") +
  geom_vline(xintercept = shock_year, linetype = "dashed", color = "black") +
  labs(
    title = "Totalbestand med sjokk i år 5 (25 % tap)",
    x = "år",
    y = "Antall dyr"
  ) +
  theme_minimal(base_size = 13)

# Kumulativ NPV
p_npv_rendalen <- ggplot(sim_rendalen, aes(x = year, y = cumNPV)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_point(color = "darkgreen") +
  geom_vline(xintercept = shock_year, linetype = "dashed", color = "black") +
  scale_y_continuous(labels = label_number(suffix = " kr", big.mark = " ", accuracy = 1)) +
  labs(
    title = "Kumulativ NPV med sjokk i år 5 (25 % tap)",
    x = "år",
    y = "Kumulativ NPV"
  ) +
  theme_minimal(base_size = 13)

# Kombiner
p_total_rendalen / p_npv_rendalen + plot_layout(guides = "collect")


final_npv_rendalen <- tail(sim_rendalen$cumNPV, 1)
print(paste0("Kumulativ NPV etter 10 ??r: ", format(round(final_npv_rendalen, 0), big.mark = " "), " kr"))

