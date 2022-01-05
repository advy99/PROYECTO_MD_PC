library(tidymodels)
step_target_encoding <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  target = NULL,
  encoder = NULL,
  skip =FALSE,
  id = rand_id("target_encoding")
){
  terms <- ellipse_check(...)
  add_step(
    recipe,
    step_target_encodings_new(
      terms = terms,
      trained = trained,
      role = role,
      target = target,
      encoder = encoder
      skip = skip,
      id = id
    )
  )
}

step_target_encodings_new <- function(terms, role, trained, encoder, target, skip,id){
  step(
    subclass="target_encoding",
    terms = terms,
    role = role,
    trained = trained,
    target = target,
    encoder = encoder,
    skip = skip,
    id = id
  )
}


prep.step_target_encoding <- function(x, training, info = NULL, ...){
  target <- tidyselect::eval_select(x$terms[[1]],training)
  factor_cols <- training %>% select_if(is.factor) %>% colnames()
  encoder <- target_encoder(training,y=target,cat_columns = factor_cols,objective="binary") %>%
  
  step_target_encodings_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    target = target,
    encoder = encoder,
    skip = x$skip,
    id = x$id
  )
}




bake.step_target_encoding <- function(object, new_data, ...){
  new_data_prep <- select(new_data,-object$target)
  
  print("Bakeado")
  as_tibble(tibble_data)
}