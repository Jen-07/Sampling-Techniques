############################################################
# STRATIFIED SAMPLING: PROPORTIONAL, NEYMAN, OPTIMUM
# Example with 3 strata (replace numbers with your question)
############################################################

### 1. INPUT: population, variability, cost ################

# Population sizes in each stratum
Nh <- c(4000, 3000, 3000)       # N1, N2, N3
L  <- length(Nh)                # number of strata
N  <- sum(Nh)                   # total population size

# Standard deviations in each stratum (from pilot or past data)
Sh <- c(10, 20, 30)             # S1, S2, S3

# Cost per sampled unit in each stratum
ch <- c(1, 2, 3)                # c1, c2, c3

# Total desired sample size
n_total <- 400                  # choose as per question

# Stratum weights
Wh <- Nh / N                    # W_h = N_h / N

Nh; Sh; ch; N; Wh; n_total

############################################################
# 2. PROPORTIONAL ALLOCATION
#    nh ∝ N_h   i.e. nh = n * N_h / N
############################################################

nh_prop_raw <- n_total * Wh     # raw (may be non-integer)
nh_prop      <- round(nh_prop_raw)

# Adjust rounding so that sum(nh) = n_total
diff_prop <- n_total - sum(nh_prop)
if(diff_prop != 0){
  # simple adjustment: add/subtract the difference to the largest stratum
  idx <- which.max(nh_prop_raw)
  nh_prop[idx] <- nh_prop[idx] + diff_prop
}

nh_prop
sum(nh_prop)


############################################################
# 3. NEYMAN ALLOCATION
#    nh ∝ N_h * S_h
############################################################

num_neyman   <- Nh * Sh
nh_neyman_raw <- n_total * num_neyman / sum(num_neyman)
nh_neyman     <- round(nh_neyman_raw)

# Adjust to sum to n_total
diff_neyman <- n_total - sum(nh_neyman)
if(diff_neyman != 0){
  idx <- which.max(nh_neyman_raw)
  nh_neyman[idx] <- nh_neyman[idx] + diff_neyman
}

nh_neyman
sum(nh_neyman)


############################################################
# 4. OPTIMUM (COST) ALLOCATION
#    nh ∝ N_h * S_h / sqrt(c_h)
############################################################

num_opt       <- Nh * Sh / sqrt(ch)
nh_opt_raw    <- n_total * num_opt / sum(num_opt)
nh_opt        <- round(nh_opt_raw)

# Adjust to sum to n_total
diff_opt <- n_total - sum(nh_opt)
if(diff_opt != 0){
  idx <- which.max(nh_opt_raw)
  nh_opt[idx] <- nh_opt[idx] + diff_opt
}

nh_opt
sum(nh_opt)


############################################################
# 5. COMPARE ALLOCATIONS AND TOTAL COST
############################################################

# Put everything in one data.frame for your report
alloc_table <- data.frame(
  Stratum        = 1:L,
  Nh             = Nh,
  Sh             = Sh,
  ch             = ch,
  nh_Proportional = nh_prop,
  nh_Neyman       = nh_neyman,
  nh_Optimum      = nh_opt
)

alloc_table

# Total expected cost under each allocation
cost_prop   <- sum(ch * nh_prop)
cost_neyman <- sum(ch * nh_neyman)
cost_opt    <- sum(ch * nh_opt)

cost_prop
cost_neyman
cost_opt


############################################################
# 6. (OPTIONAL) ESTIMATE VARIANCE OF STRATIFIED MEAN
#    IGNORING FPC, Var(ȳ_st) ≈ Σ W_h^2 * S_h^2 / n_h
############################################################

var_strat <- function(nh_vec){
  sum((Wh^2) * (Sh^2) / nh_vec)
}

var_prop   <- var_strat(nh_prop)
var_neyman <- var_strat(nh_neyman)
var_opt    <- var_strat(nh_opt)

var_prop
var_neyman
var_opt
############################################################
# simple allocation plot
#
############################################################

# Convert to long format for plotting
alloc_long <- data.frame(
  Stratum = factor(rep(alloc_table$Stratum, 3)),
  Allocation = c(alloc_table$nh_Proportional,
                 alloc_table$nh_Neyman,
                 alloc_table$nh_Optimum),
  Method = factor(rep(c("Proportional", "Neyman", "Optimum"),
                      each = nrow(alloc_table)))
)

# Basic bar plot (base R)
barplot(
  height = t(matrix(alloc_long$Allocation,
                    nrow = 3, byrow = TRUE)),
  beside = TRUE,
  names.arg = alloc_table$Stratum,
  col = c("skyblue", "orange", "seagreen"),
  xlab = "Stratum",
  ylab = "Allocated sample size",
  main = "Sample allocation by method"
)
legend("topright",
       legend = c("Proportional", "Neyman", "Optimum"),
       fill   = c("skyblue", "orange", "seagreen"))
