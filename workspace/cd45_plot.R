library(flowCore)
library(ggcyto)

# private function
label_at <- function(n) function(x) ifelse(x %% n == 0, x / 1000, "")


data <- flowCore::read.FCS("data/fcs/B.fcs")
comp <- flowCore::keyword(data)[["SPILL"]]

data2 <- flowWorkspace::compensate(
    data,
    comp
)

g <- as.ggplot(ggcyto::ggcyto(data2, aes(x = `V500-A`, y = `SSC-A`)) +
    geom_point() +
    ggcyto_par_set(limits = list(x = c(10, 100000), y = c(0, 260000))) +
    scale_y_continuous(
        breaks = seq(0, 260000, 10000),
        labels = label_at(50000),
        expand = c(0, 0)
    ) +
    scale_x_flowjo_fasinh() +
    theme_bw() +
    facet_null())

print(g)
