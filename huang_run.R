## Set seed for simulation ##
set.seed(11032025)

## Baseline ##
baseline <- simulate_nav()

## Baseline + 1hr extra ##
baseline_hr <- simulate_nav(shifts = c(8,18))

## Baseline + 2hr extra ##
baseline_2hr <- simulate_nav(shifts = c(8,19))

## Baseline w/ double patients ##
baseline_double <- simulate_nav(ecr_pt = 58*2)

## Baseline w/ triple patients ##
baseline_triple <- simulate_nav(ecr_pt = 58*3)

## Two angio INRs ##
double <- simulate_nav(angio_inr = 2, angio_ir = 0)

## Two angio INRs + 1hr extra ##
double_hr <- simulate_nav(angio_inr = 2, angio_ir = 0, shifts = c(8,18))

## Two angio INRs + 2hr extra ##
double_2hr <- simulate_nav(angio_inr = 2, angio_ir = 0, shifts = c(8,19))

## Now comment and uncomment IR trajectory in simulate_nav ##

## Exclusive-use ##
exclusive <- simulate_nav()

## Exclusive-use + 1hr extra ##
exclusive_hr <- simulate_nav(shifts = c(8,18))

## Exclusive-use + 2hr extra ##
exclusive_2hr <- simulate_nav(shifts = c(8,19))
