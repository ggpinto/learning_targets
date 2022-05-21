# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("tidyverse", "tidymodels"),
  format = "qs"
)

purrr::walk(list.files("R", full.names = TRUE), source)

list(
  tar_group_by(
    data,
    make_data(),
    manufacturer),
  tar_target(
    grouped_data,
    data,
    map(data)
  ),
  tar_target(
    initial_splits,
    make_initial_splits(grouped_data),
    map(grouped_data)
  ),
  tar_target(
    trainings,
    make_trainings(initial_splits),
    map(initial_splits)
  ),
  tar_target(
    bootstraps,
    make_bootstraps(trainings),
    map(trainings)
  ),
  tar_target(
    testings,
    make_testings(initial_splits),
    map(initial_splits)
  ),
  tar_target(
    ranger_specification,
    make_ranger_specification()
  ),
  tar_target(
    recipes,
    make_recipes(trainings),
    map(trainings)
  ),
  tar_target(
    workflows,
    make_workflows(recipes, ranger_specification),
    map(recipes)
  ),
  tar_target(
    tune_resamples,
    make_tune_resamples(bootstraps, workflows, grid_size = 1),
    map(bootstraps, workflows)
  ),
  tar_target(
    last_fits,
    make_last_fits(workflows, tune_resamples, initial_splits),
    map(workflows, tune_resamples, initial_splits)
  ),
  tar_target(
    iml_predictors,
    make_iml_predictors(testings, last_fits),
    map(testings, last_fits)
  ),
  tar_target(
    feature_importances,
    make_feature_importances(iml_predictors),
    map(iml_predictors)
  )
)
