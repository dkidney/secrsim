# secrsim

Simulations for SECR

## Setup

See `setup.R` for help on installing dependencies.

## Running simulations

To run a simulation use `run_simulation.R`.

Change the parameter settings as appropriate and source the script.

Start by using a relatively large mask spacing (e.g. 500) and a small number of sims. Using a larger mask spacing will reduce fitting time but may lead to estimation bias, particularly for the bearing error parameter - e.g. see the plot for kappa in `example_results.png` which shows results for a simulation which used 250m mask spacing.




