############################################################
# STRATIFIED DESIGN FOR POPULATION MEAN
# Sample size with subgroups, bias, cost, and time
# + plotting the design (allocation barplot)
############################################################

### 1. INPUT: subgroups, variability, cost, time, bias ####

# Number of subgroups (strata)
L <- 4

# Population sizes in each subgroup (example values)
Nh <- c(2000, 3000, 2500, 2500)       # N1..N4
N  <- sum(Nh)
Wh <- Nh / N                          # weights W_h

# Standard deviations in each subgroup (pilot estimates)
Sh <- c(8, 15, 20, 10)                # S1..S4

# Cost per unit in each subgroup
ch <- c(1, 2, 3, 4)                   # c1..c4

# Time per unit in each subgroup (e.g., minutes)
th <- c(5, 8, 10, 6)                  # t1..t4

# Design parameters
M0   <- 1.0       # target maximum MSE for mean (example)
bias <- 0.2       # anticipated absolute bias |b|
V0   <- M0 - bias^2   # allowed variance = MSE - bias^2

# Budget (total cost) and total time available
B <- 2000         # total cost budget (example)
T <- 12000        # total time budget (example, in minutes)

Nh; Sh; ch; th; N; Wh; V0; B; T

############################################################
# 2. OPTIMUM (COST) ALLOCATION WITH COST ONLY
#    nh ∝ N_h * S_h / sqrt(c_h)
############################################################

num_cost   <- Nh * Sh / sqrt(ch)

# First, consider a fixed total sample size n (we will adjust later)
n_trial <- 400
nh_opt_raw_cost <- n_trial * num_cost / sum(num_cost)
nh_opt_cost     <- round(nh_opt_raw_cost)

# Adjust rounding to make sum(nh) = n_trial
adj_cost <- n_trial - sum(nh_opt_cost)
if (adj_cost != 0) {
  idx <- which.max(nh_opt_raw_cost)
  nh_opt_cost[idx] <- nh_opt_cost[idx] + adj_cost
}

nh_opt_cost
sum(nh_opt_cost)

# Cost and time under this trial n
cost_trial <- sum(ch * nh_opt_cost)
time_trial <- sum(th * nh_opt_cost)

cost_trial
time_trial

############################################################
# 3. REQUIRED n UNDER OPTIMUM ALLOCATION GIVEN VARIANCE TARGET
# Formula (ignoring FPC):
# Var(ȳ_st) ≈ (1 / n) * [ (Σ N_h S_h / sqrt(c_h)) * (Σ N_h S_h sqrt(c_h)) / N^2 ]
############################################################

A <- sum(Nh * Sh / sqrt(ch))
B_star <- sum(Nh * Sh * sqrt(ch))

# Required total n for variance <= V0
n_required <- (A * B_star) / (N^2 * V0)

n_required

# Use ceiling to get integer n
n_req_int <- ceiling(n_required)
n_req_int

# Now recompute optimum allocation with this required n
nh_opt_raw_V <- n_req_int * num_cost / sum(num_cost)
nh_opt_V     <- round(nh_opt_raw_V)

adj_V <- n_req_int - sum(nh_opt_V)
if (adj_V != 0) {
  idx <- which.max(nh_opt_raw_V)
  nh_opt_V[idx] <- nh_opt_V[idx] + adj_V
}

nh_opt_V
sum(nh_opt_V)

# Check cost and time under this design
cost_V <- sum(ch * nh_opt_V)
time_V <- sum(th * nh_opt_V)

cost_V
time_V

############################################################
# 4. DEFINE A FUNCTION TO COMPUTE VARIANCE AND MSE
############################################################

var_strat <- function(nh_vec) {
  sum((Wh^2) * (Sh^2) / nh_vec)
}

var_V  <- var_strat(nh_opt_V)
MSE_V  <- var_V + bias^2

var_V
MSE_V

############################################################
# 5. PLOT THE DESIGN OF THE EXPERIMENT
#    Show nh by subgroup under the chosen optimum design nh_opt_V
############################################################

# Base R barplot
barplot(
  height = nh_opt_V,
  names.arg = paste0("Subgroup ", 1:L),
  col = "steelblue",
  xlab = "Subgroup",
  ylab = "Allocated sample size n_h",
  main = "Design of stratified sample (optimum allocation)"
)

# Optional: overlay cost or time as labels
text(
  x = seq_along(nh_opt_V),
  y = nh_opt_V,
  labels = paste0("c=", ch, ", t=", th),
  pos = 3, cex = 0.8, col = "darkred"
)


############################################################
# 6. SUMMARY TABLE FOR WRITE‑UP
############################################################

design_table <- data.frame(
  Subgroup = 1:L,
  Nh       = Nh,
  Wh       = round(Wh, 3),
  Sh       = Sh,
  cost_per_unit = ch,
  time_per_unit = th,
  nh_optimum    = nh_opt_V
)

design_table
cost_V
time_V
var_V
MSE_V
