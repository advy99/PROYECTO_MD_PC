library(tidymodels)

step_weighted_dist <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  var_imp = NULL,
  target = NULL,
  skip =FALSE,
  id = rand_id("weighted_dist")
){
  terms <- ellipse_check(...)
  add_step(
    recipe,
    step_weighted_dists_new(
      terms = terms,
      trained = trained,
      role = role,
      var_imp = var_imp,
      target = target,
      skip = skip,
      id = id
    )
  )
}

step_weighted_dists_new <- function(terms, role, trained, var_imp, target, skip,id){
  step(
    subclass="weighted_dist",
    terms = terms,
    role = role,
    trained = trained,
    var_imp = var_imp,
    target = target,
    skip = skip,
    id = id
  )
}


prep.step_weighted_dist <- function(x, training, info = NULL, ...){
  target <- tidyselect::eval_select(x$terms[[1]],training)
  print("Preparando")
  formula <- as.formula(glue::glue("{names(target)} ~ ."))
  
  rf <- randomForest(formula, data = training, importance = TRUE,
                     na.action = na.omit)
  print("Preparado")
  var_imp <- as.data.frame(rf$importance) %>% rownames_to_column("variable") %>%
    select(variable, MeanDecreaseAccuracy)
  step_weighted_dists_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    target = target,
    var_imp = var_imp,
    skip = x$skip,
    id = x$id
  )
}

weight_distances <- function(colname,data, var_imp){
  dist <- var_imp %>% filter(variable == colname) %>% 
    select(MeanDecreaseAccuracy) %>% pluck(1)
  select(data, colname) * dist
}

bake.step_weighted_dist <- function(object, new_data, ...){
  print("A")
  new_data_prep <- select(new_data,-object$target)
  var_imp <- object$var_imp
  new_data_prep <- map_dfc(colnames(new_data_prep), weight_distances, new_data_prep, var_imp)
  target_data <- select(new_data, object$target)
  tibble_data <- new_data_prep %>% bind_cols(target_data)
  print("Bakeado")
  as_tibble(tibble_data)
}