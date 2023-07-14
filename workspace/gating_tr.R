library(flowCore)
library(flowWorkspace)
library(ggcyto)

# private function
label_at <- function(n) function(x) ifelse(x %% n == 0, x / 1000, "")

data <- flowCore::read.FCS("data/fcs/A.fcs")
comp <- flowCore::keyword(data)[["SPILL"]]

data <- flowWorkspace::compensate(
    data,
    comp
)

outlier.gate <- rectangleGate(
    filterId = "-outlier",
    "FSC-A" = c(0, 100000),
    "FSC-H" = c(0, 100000)
)

gs <- GatingSet(flowSet(data))
gs_pop_add(gs, outlier.gate, parent = "root")
g <- openCyto:::.singletGate(data, channels = c("FSC-A", "FSC-H"))

p1 <- ggcyto::ggcyto(data, aes(x = `FSC-A`, y = `FSC-H`)) +
    geom_hex(bins = 100) +
    geom_gate(g) +
    ggcyto_par_set(limits = list(x = c(10, 260000), y = c(0, 260000))) +
    scale_y_continuous(
        breaks = seq(0, 260000, 10000),
        labels = label_at(50000),
        expand = c(0, 0)
    ) +
    scale_x_continuous(
        breaks = seq(0, 260000, 10000),
        labels = label_at(50000),
        expand = c(0, 0)
    ) +
    theme_bw() +
    theme(aspect.ratio = 1) +
    facet_null()


p2 <- ggcyto::ggcyto(gs[[1]], aes(x = `FSC-A`, y = `SSC-A`),
    subset = "/-outlier"
) +
    geom_point() +
    ggcyto_par_set(limits = list(x = c(10, 260000), y = c(0, 260000))) +
    scale_y_continuous(
        breaks = seq(0, 260000, 10000),
        labels = label_at(50000),
        expand = c(0, 0)
    ) +
    scale_x_continuous(
        breaks = seq(0, 260000, 10000),
        labels = label_at(50000),
        expand = c(0, 0)
    ) +
    theme_bw() +
    theme(aspect.ratio = 1) +
    facet_null()


grid.arrange(as.ggplot(p1), as.ggplot(p2), ncol = 2)
