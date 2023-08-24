library(flowCore)
library(flowWorkspace)
library(ggcyto)

data <- flowCore::read.FCS("data/fcs/A.fcs")
comp <- flowCore::keyword(data)[["SPILL"]]

data <- flowWorkspace::compensate(
    data,
    comp
)

gate.debris <- polygonGate(
    filterId = "leukocytes",
    "FSC-A" = c(20000, 50000, 40000, 252400, 262400),
    "SSC-A" = c(10000, 60000, 262400, 262400, 0)
)

gate.singlets <- polygonGate(
    filterId = "singlets",
    "FSC-A" = c(0, 0, 50000, 252400, 250400),
    "FSC-H" = c(0, 30000, 70000, 155000, 96000)
)

gs <- GatingSet(flowSet(data))
gs_pop_add(gs, gate.debris)
gs_pop_add(gs, gate.singlets, parent = "leukocytes")

recompute(gs)

autoplot(gs[[1]], bool = TRUE)
