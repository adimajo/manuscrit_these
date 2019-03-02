

library(party)
library(tikzDevice)

data <- carData::TitanicSurvival

t <- ctree(
  survived ~ passengerClass + sex + age, 
  data,
  controls = ctree_control(
    teststat="quad",
    testtype="Univariate",
    mincriterion=.95,
    minsplit=10, 
    minbucket=5,
    maxdepth=0
  )
)

tikz("titanic_tree.tex", standAlone=FALSE, width = 12, height = 6, fg = "black", sanitize = FALSE)
plot(t, type = "simple")
dev.off()