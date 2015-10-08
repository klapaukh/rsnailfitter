# rsnailfitter

This is an R library for modelling snails as described by Raup's 1966 paper. 

## Usage

For the purposes of simplicity, we are assuming that you are measuring the
centroid of the snail shell, rather than a point on the surface of the shell.
From the point of view of the formulation used by Raup that means that 
y<sub>0</sub> and r</sub>c</sub> are both zero. Note that by making this 
simplification we can ignore the appeture shape entirely.

## Installation

```r
library(devtools)

install_github("rsnailfitter",username="klapaukh")
```
