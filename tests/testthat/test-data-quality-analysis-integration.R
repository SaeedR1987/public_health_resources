# Integration test for Data Quality and Analysis workflow
# This test demonstrates the connection between Data, DataQuality, and Analysis objects

test_that("Data can generate DataQuality objects with proper fields", {
  # Create a simple mock Data object
  mock_data <- data.frame(
    id = 1:10,
    value = rnorm(10),
    category = sample(c("A", "B", "C"), 10, replace = TRUE)
  )
  
  data_obj <- Data$new(
    data = mock_data,
    dataset_name = "TestData"
  )
  
  # Set up some basic mappings
  data_obj$variable_map <- list(uuid = "id")
  data_obj$value_map <- list(category = list(A = "A", B = "B", C = "C"))
  
  # Standardize the data
  data_obj$standardize()
  
  # Test generating a general DataQuality object (base Data class)
  quality_obj <- data_obj$generate_data_quality(
    stage = "standardized"
  )
  
  # Verify the quality object has the right properties
  expect_s3_class(quality_obj, "DataQuality")
  expect_equal(quality_obj$dataset_name, "TestData_DataQuality")
  expect_equal(quality_obj$data_stage_name, "standardized")
  expect_false(is.null(quality_obj$data_hash))
  expect_equal(quality_obj$variable_map, data_obj$variable_map)
  expect_equal(quality_obj$value_map, data_obj$value_map)
  expect_identical(quality_obj$parent_data_object, data_obj)
})

test_that("HouseholdData can generate different types of DataQuality objects", {
  mock_data <- data.frame(
    hh_id = 1:10,
    value = rnorm(10),
    stringsAsFactors = FALSE
  )
  
  hh_data <- HouseholdData$new(
    data = mock_data,
    dataset_name = "TestHouseholdData"
  )
  hh_data$standardize()
  
  # Test FSL quality
  fsl_quality <- hh_data$generate_data_quality(
    stage = "standardized",
    type = "fsl"
  )
  expect_s3_class(fsl_quality, "FSLDataQuality")
  expect_s3_class(fsl_quality, "DataQuality")
  
  # Test WASH quality
  wash_quality <- hh_data$generate_data_quality(
    stage = "standardized",
    type = "wash"
  )
  expect_s3_class(wash_quality, "WASHDataQuality")
  expect_s3_class(wash_quality, "DataQuality")
  
  # Test Health quality
  health_quality <- hh_data$generate_data_quality(
    stage = "standardized",
    type = "health"
  )
  expect_s3_class(health_quality, "HealthDataQuality")
  expect_s3_class(health_quality, "DataQuality")
  
  # Test Mortality quality
  mortality_quality <- hh_data$generate_data_quality(
    stage = "standardized",
    type = "mortality"
  )
  expect_s3_class(mortality_quality, "MortalityDataQuality")
  expect_s3_class(mortality_quality, "DataQuality")
})

test_that("IndividualData can generate demographics DataQuality objects", {
  mock_data <- data.frame(
    person_id = 1:10,
    hh_uuid = paste0("HH", 1:10),
    age = sample(1:80, 10, replace = TRUE),
    sex = sample(c("M", "F"), 10, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  ind_data <- IndividualData$new(
    data = mock_data,
    dataset_name = "TestIndividualData"
  )
  ind_data$standardize()
  
  # Test Demographics quality
  demo_quality <- ind_data$generate_data_quality(
    stage = "standardized",
    type = "demographics"
  )
  expect_s3_class(demo_quality, "DemographicsDataQuality")
  expect_s3_class(demo_quality, "DataQuality")
})

test_that("NutritionIndividualData can generate nutrition-specific DataQuality objects", {
  skip_if_not_installed("readxl")
  
  mock_data <- data.frame(
    person_id = 1:10,
    hh_uuid = paste0("HH", 1:10),
    age_months = sample(6:59, 10, replace = TRUE),
    weight_kg = rnorm(10, 12, 2),
    height_cm = rnorm(10, 80, 10),
    stringsAsFactors = FALSE
  )
  
  nutr_data <- NutritionIndividualData$new(
    data = mock_data,
    dataset_name = "TestNutritionData"
  )
  nutr_data$standardize()
  
  # Test Anthropometric quality
  anthro_quality <- nutr_data$generate_data_quality(
    stage = "standardized",
    type = "anthropometric"
  )
  expect_s3_class(anthro_quality, "AnthropometricDataQuality")
  expect_s3_class(anthro_quality, "DataQuality")
  
  # Test IYCF quality
  iycf_quality <- nutr_data$generate_data_quality(
    stage = "standardized",
    type = "iycf"
  )
  expect_s3_class(iycf_quality, "IYCFDataQuality")
  expect_s3_class(iycf_quality, "DataQuality")
})

test_that("HouseholdData can generate Analysis objects with proper fields", {
  skip_if_not_installed("srvyr")
  
  # Create a more complex mock HouseholdData object
  mock_data <- data.frame(
    hh_id = 1:20,
    cluster_id = rep(1:4, each = 5),
    weight = runif(20, 0.5, 2),
    value = rnorm(20),
    stringsAsFactors = FALSE
  )
  
  # Use HouseholdData so we can get survey design
  hh_data <- HouseholdData$new(
    data = mock_data,
    dataset_name = "TestHouseholdData"
  )
  
  # Set up survey design mappings
  hh_data$variable_map <- list(
    uuid = "hh_id",
    cluster_id = "cluster_id",
    weight = "weight"
  )
  hh_data$value_map <- list()
  
  # Standardize
  hh_data$standardize()
  
  # Test generating FSL Analysis object
  analysis_obj <- hh_data$generate_data_analysis(
    stage = "standardized",
    type = "fsl"
  )
  
  # Verify the analysis object has the right properties
  expect_s3_class(analysis_obj, "QuantDataAnalysisFSL")
  expect_equal(analysis_obj$dataset_name, "TestHouseholdData_FSLAnalysis")
  expect_equal(analysis_obj$data_stage_name, "standardized")
  expect_false(is.null(analysis_obj$data_hash))
  expect_equal(analysis_obj$variable_map, hh_data$variable_map)
  expect_equal(analysis_obj$value_map, hh_data$value_map)
  expect_identical(analysis_obj$parent_data_object, hh_data)
  expect_false(is.null(analysis_obj$survey_design))
})

test_that("HouseholdData can generate different types of Analysis objects", {
  skip_if_not_installed("srvyr")
  
  mock_data <- data.frame(
    hh_id = 1:20,
    cluster_id = rep(1:4, each = 5),
    weight = runif(20, 0.5, 2),
    value = rnorm(20),
    stringsAsFactors = FALSE
  )
  
  hh_data <- HouseholdData$new(
    data = mock_data,
    dataset_name = "TestHouseholdData"
  )
  
  hh_data$variable_map <- list(
    uuid = "hh_id",
    cluster_id = "cluster_id",
    weight = "weight"
  )
  hh_data$standardize()
  
  # Test FSL analysis
  fsl_analysis <- hh_data$generate_data_analysis(
    stage = "standardized",
    type = "fsl"
  )
  expect_s3_class(fsl_analysis, "QuantDataAnalysisFSL")
  expect_s3_class(fsl_analysis, "QuantDataAnalysis")
  
  # Test WASH analysis
  wash_analysis <- hh_data$generate_data_analysis(
    stage = "standardized",
    type = "wash"
  )
  expect_s3_class(wash_analysis, "WASHAnalysis")
  expect_s3_class(wash_analysis, "QuantDataAnalysis")
  
  # Test Health analysis
  health_analysis <- hh_data$generate_data_analysis(
    stage = "standardized",
    type = "health"
  )
  expect_s3_class(health_analysis, "HealthAnalysis")
  expect_s3_class(health_analysis, "QuantDataAnalysis")
  
  # Test Mortality analysis
  mortality_analysis <- hh_data$generate_data_analysis(
    stage = "standardized",
    type = "mortality"
  )
  expect_s3_class(mortality_analysis, "MortalityAnalysis")
  expect_s3_class(mortality_analysis, "QuantDataAnalysis")
})

test_that("IndividualData can generate DemographicsAnalysis", {
  mock_data <- data.frame(
    person_id = 1:10,
    hh_uuid = paste0("HH", 1:10),
    age = sample(1:80, 10, replace = TRUE),
    sex = sample(c("M", "F"), 10, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  ind_data <- IndividualData$new(
    data = mock_data,
    dataset_name = "TestIndividualData"
  )
  ind_data$standardize()
  
  # Test Demographics analysis
  demo_analysis <- ind_data$generate_data_analysis(
    stage = "standardized",
    type = "demographics"
  )
  expect_s3_class(demo_analysis, "DemographicsAnalysis")
  expect_s3_class(demo_analysis, "QuantDataAnalysis")
})

test_that("NutritionIndividualData can generate NutritionAnalysis", {
  skip_if_not_installed("readxl")
  
  mock_data <- data.frame(
    person_id = 1:10,
    hh_uuid = paste0("HH", 1:10),
    age_months = sample(6:59, 10, replace = TRUE),
    weight_kg = rnorm(10, 12, 2),
    height_cm = rnorm(10, 80, 10),
    stringsAsFactors = FALSE
  )
  
  nutr_data <- NutritionIndividualData$new(
    data = mock_data,
    dataset_name = "TestNutritionData"
  )
  nutr_data$standardize()
  
  # Test Nutrition analysis
  nutr_analysis <- nutr_data$generate_data_analysis(
    stage = "standardized"
  )
  expect_s3_class(nutr_analysis, "NutritionAnalysis")
  expect_s3_class(nutr_analysis, "QuantDataAnalysis")
})

test_that("Analysis classes can initialize without linked data", {
  skip_if_not_installed("srvyr")
  
  mock_data <- data.frame(
    hh_id = 1:20,
    cluster_id = rep(1:4, each = 5),
    weight = runif(20, 0.5, 2),
    stringsAsFactors = FALSE
  )
  
  hh_data <- HouseholdData$new(
    data = mock_data,
    dataset_name = "TestHouseholdData"
  )
  hh_data$variable_map <- list(
    uuid = "hh_id",
    cluster_id = "cluster_id",
    weight = "weight"
  )
  hh_data$standardize()
  
  # Test that analysis works without linked data
  wash_analysis <- hh_data$generate_data_analysis(stage = "standardized", type = "wash")
  expect_null(wash_analysis$linked_containers_data)
  
  health_analysis <- hh_data$generate_data_analysis(stage = "standardized", type = "health")
  expect_null(health_analysis$linked_ind_health_data)
  
  mortality_analysis <- hh_data$generate_data_analysis(stage = "standardized", type = "mortality")
  expect_null(mortality_analysis$linked_ind_roster_data)
  expect_null(mortality_analysis$linked_ind_deaths_data)
})
