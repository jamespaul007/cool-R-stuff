devtools::install_github("hilaryparker/explainr")
library(explainr)
ptest <- prop.test(x = 500, n = 1008)
ptest
explain(ptest)
