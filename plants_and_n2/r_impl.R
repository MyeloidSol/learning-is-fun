library(ggplot2)
library(tidyverse)


# Test Data Generation ----
# OG N- = 14 strains isolated from hN-
# OG N+ = 14 strains isolated from hN+
# Mixed 1 = 7 LQ strains from hN- | 7 LQ strains from hN+
# Mixed 2 = 7 HQ strains from hN- | 7 HQ strains from hN+

# EE N- Plant+ = 14 strains from hN-P+
# EE N+ Plant+ = 14 strains from hN+P+
# EE N- Plant- = 14 strains from hN-P-
# EE N+ Plant- = 14 strains from hN+P-

# Plant growth measured: nodule #, shoot biomass, leaf count, shoot length, chlorophyll content
# Rhizobia occupancy: Nodule DNA extraction