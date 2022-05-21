make_data <- function() {
  set.seed(123)
  
  slice_sample(ggplot2::mpg, n = 5000, replace = TRUE)
}

make_initial_splits <- function(.grouped_data) {
  
  set.seed(123)
  
  .group <- unique(.grouped_data$manufacturer)
  
  .grouped_data <- .grouped_data %>%
    dplyr::select(
      -manufacturer,
      -tar_group,
      -cty,
      -model) %>%
    dplyr::mutate(
      dplyr::across(c(year, cyl, trans, drv, fl, class), factor)
    )
  
  .inital_split <- .grouped_data %>%
    rsample::initial_split(strata = hwy)
  
  tibble::tibble(
    group = .group,
    initial_split = list(.inital_split)
  )
}

make_trainings <- function(.initial_splits) {
  
  .group <- .initial_splits$group
  
  .initial_split  <- .initial_splits$initial_split[[1]]
  
  .training <- rsample::training(.initial_split)
  
  tibble::tibble(
    group = .group,
    training = list(.training)
  )
  
}

make_bootstraps <- function(.trainings) {
  
  set.seed(123)
  
  .group <- .trainings$group
  
  .training  <- .trainings$training[[1]]
  
  .bootstraps <- rsample::bootstraps(.training, strata = hwy)
  
  tibble::tibble(
    group = .group,
    bootstraps = list(.bootstraps)
  )
  
}

make_testings<- function(.initial_splits) {
  
  .group <- .initial_splits$group
  
  .initial_split  <- .initial_splits$initial_split[[1]]
  
  .testing <- rsample::testing(.initial_split)
  
  tibble::tibble(
    group = .group,
    testing = list(.testing)
  )
  
}

make_ranger_specification <- function() {
  parsnip::rand_forest(
    mtry = hardhat::tune(),
    trees = 1000,
    min_n = hardhat::tune()
  ) %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("ranger")
}

make_recipes <- function(.trainings) {
  
  .group <- .trainings$group
  
  .training <- .trainings$training[[1]]
  
  .recipe <- recipes::recipe(
    hwy ~ ., data = .training
  )
  
  tibble::tibble(
    group = .group,
    recipe = list(.recipe)
  )
  
}

make_workflows <- function(.recipes, .model_specification) {
  
  .group <- .recipes$group
  
  .recipe <- .recipes$recipe[[1]]
  
  .workflow <- workflows::workflow() %>%
    workflows::add_recipe(.recipe) %>%
    workflows::add_model(.model_specification)
  
  tibble::tibble(
    group = .group,
    workflow = list(.workflow)
  )
  
}

make_last_fits <- function(.workflows, .tune_resamples, .initial_splits) {
  
  .group = .workflows$group
  
  .workflow <- .workflows$workflow[[1]]
  
  .tune_resample <- .tune_resamples$tune_resample[[1]]
  
  .initial_split <- .initial_splits$initial_split[[1]]
  
  .last_fit <- .workflow %>%
    tune::finalize_workflow(
      tune::select_best(
        .tune_resample,
        metric = "rmse")) %>%
    tune::last_fit(
      .initial_split
    )
  
  tibble::tibble(
    group = .group,
    last_fit = list(.last_fit)
  )
  
}

make_tune_resamples <- function(.bootstraps, .workflows, grid_size = 1) {
  
  set.seed(123)
  
  doParallel::registerDoParallel()
  
  .group <- .bootstraps$group
  
  .bootstrap <- .bootstraps$bootstraps[[1]]
  
  .workflow <- .workflows$workflow[[1]]
  
  .tune_resample <- tune::tune_grid(
    .workflow,
    resamples = .bootstrap ,
    grid = grid_size
  )
  
  tibble::tibble(
    group = .group,
    tune_resample = list(.tune_resample)
  )
  
}

make_iml_predictors <- function(.testings, .last_fits) {
  
  set.seed(123)
  
  .group <- .testings$group
  
  .testing <- .testings$testing[[1]]
  
  .last_fit <- .last_fits$last_fit[[1]]
  
  .model <- .last_fit %>%
    hardhat::extract_fit_parsnip()
  
  .x <- .testing %>%
    dplyr::select(-hwy)
  
  .y <- .testing$hwy
  
  .iml_predictor <- iml::Predictor$new(
    model = .model,
    data = .x,
    y = .y
  )
  
  tibble::tibble(
    group = .group,
    iml_predictor = list(.iml_predictor)
  )
}

make_feature_importances <- function(.iml_predictors) {
  
  set.seed(123)
  
  .group <- .iml_predictors$group
  
  .iml_predictor <- .iml_predictors$iml_predictor[[1]]
  
  .feature_importance <- iml::FeatureImp$new(
    .iml_predictor,
    loss = "rmse"
  )$results %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::mutate(feature = forcats::fct_reorder(feature, importance))
  
  tibble::tibble(
    group = .group,
    feature_importance = list(.feature_importance)
  )
}