library(flowCore)
library(flowViz)
library(ggcyto)

# Load the data
dat <- flowCore::read.FCS("data/fcs/K220706002034_MDS Tube 1.fcs")

# Plot the data
flowViz::plot(dat)