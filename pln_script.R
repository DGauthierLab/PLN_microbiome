install.packages("PLNmodels")
library(PLNmodels)
library(ggplot2)

```{r trichoptera vignette}

data("trichoptera")

trichoptera <- prepare_data(trichoptera$Abundance, trichoptera$Covariate)

str(trichoptera)
View(trichoptera)

corrplot::corrplot(
  t(log(1 + trichoptera$Abundance)),
  is.corr = FALSE,
  addgrid.col = NA
)

network_models <- PLNnetwork(Abundance ~ 1 + offset(log(Offset)), data = trichoptera)

network_models

network_models$criteria %>% head() %>% knitr::kable()

network_models$convergence %>% head() %>% knitr::kable()

plot(network_models, "diagnostic")

plot(network_models)

coefficient_path(network_models, corr = TRUE) %>% 
  ggplot(aes(x = Penalty, y = Coeff, group = Edge, colour = Edge)) + 
  geom_line(show.legend = FALSE) +  coord_trans(x="log10") + theme_bw()

model_pen <- getModel(network_models, network_models$penalties[20]) # give some sparsity
model_BIC <- getBestModel(network_models, "BIC")   # if no criteria is specified, the best BIC is used

library(future)
plan(multisession, workers = 2)

n <- nrow(trichoptera)
subs <- replicate(10, sample.int(n, size = n/2), simplify = FALSE)
stability_selection(network_models, subsamples = subs)

model_StARS <- getBestModel(network_models, "StARS")

plot(network_models, "stability")

future::plan("sequential")

model_StARS

my_graph <- plot(model_StARS, plot = FALSE)
my_graph

plot(model_StARS)
```
