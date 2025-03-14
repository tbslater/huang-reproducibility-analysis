# Reproducibility analysis: Huang et al. (2019)

This repository documents a reproducibility analysis of Huang et al., 2019. The original paper used a discrete-event simulation model to investigate treatment allocation within stroke patients and is referenced below.

> Huang, Shiwei et al. “Optimizing Resources for Endovascular Clot Retrieval for Acute Ischemic Stroke, a Discrete Event Simulation.” Frontiers in neurology vol. 10 653. 27 Jun. 2019, <doi:10.3389/fneur.2019.00653>

Original source code can be found at <https://github.com/shiweih/desECR.git.>

## Blog Post

### [Check out the results of my analysis here!](https://tbslater.github.io/public-health-simulation/posts/reproducing-huang-et-al/huang-blog-post.html)

## Code

R code associated with the reproducibility analysis can be found in `/code` folder.

-   `huang_plot.R` contains all the necessary functions for producing the plots. 
-   `huang_run.R` calls the simulation function to run the various scenarios. 
-   `huang_sim.R` contains the simulation function adapted from the original source code. 

The R package `simmer` is used extensively and I recommend getting to grips with this before using the above code. 

## Citation
> Slater, T. (2025). tbslater/huang-reproducibility-analysis: Version 1.0.0 (v1.0.0). Zenodo. https://doi.org/10.5281/zenodo.15025508
