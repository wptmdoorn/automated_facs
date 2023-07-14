library(flowCore)
library(ggcyto)

# private function
label_at <- function(n) function(x) ifelse(x %% n == 0, x / 1000, "")


data <- flowCore::read.FCS("data/fcs/A.fcs")
comp <- flowCore::keyword(data)[["SPILL"]]

data2 <- flowWorkspace::compensate(
    data,
    comp
)

g <- ggcyto::ggcyto(data2, aes(x = `FITC-A`, y = `PE-A`)) +
    geom_point() +
    ggcyto_par_set(limits = list(x = c(10, 100000), y = c(10, 100000))) +
    scale_y_flowjo_fasinh() +
    scale_x_flowjo_fasinh() +
    theme_bw() +
    theme(aspect.ratio = 1) +
    facet_null()

print(g)
