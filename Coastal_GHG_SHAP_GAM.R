
# 0. Install and load the R package
packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "mgcv",
  "patchwork",
  "xgboost",
  "tibble"
)

new_packages <- packages[
  !(packages %in% installed.packages()[, "Package"])
]

if(length(new_packages) > 0){
  install.packages(new_packages)
}

invisible(
  lapply(
    packages,
    library,
    character.only = TRUE
  )
)

# 1. Read data

data <- read_excel("Data.xlsx")

cat("\n==============================\n")
cat("Data dimensions:\n")
cat(nrow(data), "rows ×", ncol(data), "columns\n")
cat("==============================\n\n")

print(names(data))



# 2. Define all predictor variables for machine learning

predictors <- c(
  "Temp",
  "Salinity",
  "pH",
  "DO",
  "DIP",
  "DSi",
  "NH4",
  "NOx",
  "Grain_size",
  "SOC",
  "SON",
  "Latitude",
  "Longitude"
)

responses <- c(
  "N2O",
  "CH4",
  "CO2"
)


# 3. Check column names

required_columns <- unique(
  c(
    predictors,
    responses
  )
)

missing_columns <- setdiff(
  required_columns,
  names(data)
)

if(length(missing_columns) > 0){
  
  stop(
    paste(
      "Data.xlsx The following variables are missing:",
      paste(missing_columns, collapse = ", ")
    )
  )
  
} else {
  
  cat("All required variables exist. \n\n")
  
}


# 4. Convert related variables to numeric

data <- data %>%
  mutate(
    across(
      all_of(required_columns),
      as.numeric
    )
  )


# 5. Missing value statistics

missing_summary <- data.frame(
  Variable = required_columns,
  Missing_n = sapply(
    data[required_columns],
    function(x) sum(is.na(x))
  ),
  Missing_percent = round(
    sapply(
      data[required_columns],
      function(x) mean(is.na(x)) * 100
    ),
    2
  )
)

cat("Missing values: \n")
print(missing_summary)
cat("\n")


# 6. XGBoost + SHAP


get_shap_main_effect <- function(
    df,
    response,
    focal_var,
    predictors,
    nrounds = 500,
    eta = 0.03,
    max_depth = 3,
    min_child_weight = 5,
    subsample = 0.8,
    colsample_bytree = 0.8,
    seed = 123
){
  
  
  dat <- df %>%
    select(
      all_of(
        c(
          response,
          predictors
        )
      )
    ) %>%
    filter(
      !is.na(.data[[response]])
    )
  
  
  # predictor matrix
  
  X <- as.matrix(
    dat[, predictors]
  )
  
  y <- dat[[response]]
  
  
  # DMatrix
  
  dtrain <- xgb.DMatrix(
    data = X,
    label = y,
    missing = NA
  )
  
  
  # ------------------------------
  # parameters
  # ------------------------------
  
  params <- list(
    objective = "reg:squarederror",
    eta = eta,
    max_depth = max_depth,
    min_child_weight = min_child_weight,
    subsample = subsample,
    colsample_bytree = colsample_bytree
  )
  
  
  # train
  
  set.seed(seed)
  
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    verbose = 0
  )
  
  
  # SHAP interaction
  
  shap_inter <- predict(
    model,
    newdata = dtrain,
    predinteraction = TRUE
  )
  
  
  var_index <- match(
    focal_var,
    predictors
  )
  
  if(is.na(var_index)){
    
    stop(
      paste(
        "variable",
        focal_var,
        "No predictors"
      )
    )
    
  }
  
  
  # SHAP main effect
  
  shap_main <- shap_inter[
    ,
    var_index,
    var_index
  ]
  
  
  # Output data
  
  out <- tibble(
    x = dat[[focal_var]],
    shap_main = shap_main
  ) %>%
    filter(
      !is.na(x),
      !is.na(shap_main)
    )
  
  
  return(
    list(
      data = out,
      model = model
    )
  )
}



# 7. GAM threshold recognition function


find_threshold <- function(pred_df){
  
  pred_df <- pred_df %>%
    arrange(x)
  
  x <- pred_df$x
  y <- pred_df$fit

  
  d1 <- diff(y) / diff(x)
  
  x1 <- (
    x[-1] +
      x[-length(x)]
  ) / 2
  
  
  sign_change <- which(
    diff(sign(d1)) != 0
  )
  
  
  if(length(sign_change) > 0){
    
    
    candidate_indices <- sign_change + 1
    
    d2_temp <- abs(
      diff(d1)
    )
    
    valid_indices <- candidate_indices[
      candidate_indices <= length(d2_temp)
    ]
    
    if(length(valid_indices) > 0){
      
      best <- valid_indices[
        which.max(
          d2_temp[valid_indices]
        )
      ]
      
      threshold <- x1[best]
      
    } else {
      
      threshold <- x1[
        sign_change[1] + 1
      ]
      
    }
    
    threshold_type <- "turning point"
    
    
  } else {
    
    
    d2 <- diff(d1) / diff(x1)
    
    x2 <- (
      x1[-1] +
        x1[-length(x1)]
    ) / 2
    
    threshold <- x2[
      which.max(
        abs(d2)
      )
    ]
    
    threshold_type <- "knee point"
    
  }
  
  
  return(
    list(
      threshold = threshold,
      type = threshold_type
    )
  )
}


# 8. GAM + SHAP Main effect + threshold plot function

plot_shap_gam_threshold <- function(
    df,
    response,
    focal_var,
    predictors,
    panel_title,
    x_lab,
    k_value = 5,
    seed = 123
){
  
  # SHAP main effect
  
  shap_result <- get_shap_main_effect(
    df = df,
    response = response,
    focal_var = focal_var,
    predictors = predictors,
    seed = seed
  )
  
  plot_df <- shap_result$data
  
  
  # GAM
  
  gam_model <- gam(
    shap_main ~ s(
      x,
      k = k_value
    ),
    data = plot_df,
    method = "REML"
  )
  
  
  newdata <- data.frame(
    x = seq(
      min(
        plot_df$x,
        na.rm = TRUE
      ),
      max(
        plot_df$x,
        na.rm = TRUE
      ),
      length.out = 500
    )
  )
  
  
  gam_pred <- predict(
    gam_model,
    newdata = newdata,
    se.fit = TRUE
  )
  
  
  newdata$fit <- gam_pred$fit
  
  newdata$lwr <-
    gam_pred$fit -
    1.96 * gam_pred$se.fit
  
  newdata$upr <-
    gam_pred$fit +
    1.96 * gam_pred$se.fit
  
  
  # threshold
  
  threshold_result <- find_threshold(
    newdata
  )
  
  threshold_value <-
    threshold_result$threshold
  
  
  gam_summary <- summary(
    gam_model
  )
  
  edf_value <-
    gam_summary$s.table[1, "edf"]
  
  p_value <-
    gam_summary$s.table[1, "p-value"]
  
  deviance_explained <-
    gam_summary$dev.expl * 100
  
  adj_r2 <-
    gam_summary$r.sq
  
  
  fitted_value <- predict(
    gam_model,
    newdata = plot_df
  )
  
  
  # ------------------------------
  # R2
  # ------------------------------
  
  R2 <- cor(
    plot_df$shap_main,
    fitted_value,
    use = "complete.obs"
  )^2
  
  
  # RMSE
  
  RMSE <- sqrt(
    mean(
      (
        plot_df$shap_main -
          fitted_value
      )^2,
      na.rm = TRUE
    )
  )
  
  
  # p-value label
  
  if(p_value < 0.001){
    
    p_label <- "p < 0.001"
    
  } else {
    
    p_label <- paste0(
      "p = ",
      format(
        p_value,
        digits = 2,
        scientific = TRUE
      )
    )
    
  }
  
  
  # annotation placement
  
  x_min <- min(
    plot_df$x,
    na.rm = TRUE
  )
  
  x_max <- max(
    plot_df$x,
    na.rm = TRUE
  )
  
  y_min <- min(
    plot_df$shap_main,
    na.rm = TRUE
  )
  
  y_max <- max(
    plot_df$shap_main,
    na.rm = TRUE
  )
  
  x_range <- x_max - x_min
  y_range <- y_max - y_min
  
  
  # plot
  
  p <- ggplot(
    plot_df,
    aes(
      x = x,
      y = shap_main
    )
  ) +
    
    # scatter
    geom_point(
      color = "#5995F5",
      alpha = 0.62,
      size = 1.8
    ) +
    
    # 95% CI
    geom_ribbon(
      data = newdata,
      aes(
        x = x,
        ymin = lwr,
        ymax = upr
      ),
      inherit.aes = FALSE,
      fill = "#F26B5B",
      alpha = 0.18
    ) +
    
    # GAM curve
    geom_line(
      data = newdata,
      aes(
        x = x,
        y = fit
      ),
      inherit.aes = FALSE,
      color = "#F26B5B",
      linewidth = 1.25
    ) +
    
    # threshold line
    geom_vline(
      xintercept = threshold_value,
      color = "#FF6666",
      linewidth = 0.85,
      linetype = "dashed"
    ) +
    
    # threshold number
    annotate(
      "text",
      x = threshold_value,
      y = y_min + 0.04 * y_range,
      label = round(
        threshold_value,
        2
      ),
      color = "#FF5555",
      size = 4.1,
      vjust = 1
    ) +
    
    # statistics
    annotate(
      "text",
      x = x_max - 0.03 * x_range,
      y = y_min + 0.10 * y_range,
      label = paste0(
        "GAM fitting\n",
        "R² = ",
        round(R2, 2),
        "\n",
        "Deviance = ",
        round(
          deviance_explained,
          1
        ),
        "%\n",
        p_label
      ),
      hjust = 1,
      vjust = 0,
      size = 4
    ) +
    
    labs(
      title = panel_title,
      x = x_lab,
      y = "SHAP main effect value"
    ) +
    
    theme_classic(
      base_size = 14
    ) +
    
    theme_classic(
      base_size = 14
    ) +
    
    theme(
      plot.title = element_text(
        size = 14,
        face = "bold",
        hjust = 0
      ),
      axis.title = element_text(
        size = 13
      ),
      axis.text = element_text(
        size = 11,
        color = "black"
      ),
      axis.line = element_line(
        linewidth = 0.7
      ),
      plot.margin = ggplot2::margin(
        8,
        8,
        8,
        8,
        unit = "pt"
      )
    )
  
  
  # Output the statistical results
  
  result_table <- data.frame(
    Gas = response,
    Variable = focal_var,
    n = nrow(plot_df),
    EDF = edf_value,
    P_value = p_value,
    Adjusted_R2 = adj_r2,
    Deviance_explained_percent =
      deviance_explained,
    Plot_R2 = R2,
    RMSE = RMSE,
    Threshold = threshold_value,
    Threshold_type =
      threshold_result$type
  )
  
  
  return(
    list(
      plot = p,
      gam_model = gam_model,
      xgb_model = shap_result$model,
      shap_data = plot_df,
      prediction_data = newdata,
      results = result_table
    )
  )
}


# 9. Plot N2O ~ NOx

p1 <- plot_shap_gam_threshold(
  df = data,
  response = "N2O",
  focal_var = "NOx",
  predictors = predictors,
  panel_title = "(a) N2O and NOx",
  x_lab = expression(
    NO[x]^"-"
  )
)


# 10. Plot N2O ~ NH4

p2 <- plot_shap_gam_threshold(
  df = data,
  response = "N2O",
  focal_var = "NH4",
  predictors = predictors,
  panel_title = "(b) N2O and NH4+",
  x_lab = expression(
    NH[4]^"+"
  )
)

# 11. Plot CH4 ~ DO


p3 <- plot_shap_gam_threshold(
  df = data,
  response = "CH4",
  focal_var = "DO",
  predictors = predictors,
  panel_title = "(c) CH4 and DO",
  x_lab = "DO"
)


# 12. Plot CH4 ~ NOx


p4 <- plot_shap_gam_threshold(
  df = data,
  response = "CH4",
  focal_var = "NOx",
  predictors = predictors,
  panel_title = "(d) CH4 and NOx",
  x_lab = expression(
    NO[x]^"-"
  )
)


# 13. Plot CO2 ~ Temperature


p5 <- plot_shap_gam_threshold(
  df = data,
  response = "CO2",
  focal_var = "Temp",
  predictors = predictors,
  panel_title = "(e) CO2 and temperature",
  x_lab = expression(
    "Temperature (" *
      degree *
      "C)"
  )
)


# 14. Plot CO2 ~ DO


p6 <- plot_shap_gam_threshold(
  df = data,
  response = "CO2",
  focal_var = "DO",
  predictors = predictors,
  panel_title = "(f) CO2 and DO",
  x_lab = "DO"
)



final_plot <- (
  p1$plot |
    p2$plot |
    p3$plot
) /
  (
    p4$plot |
      p5$plot |
      p6$plot
  )

print(
  final_plot
)

# 16. Summary of statistical results

result_table <- bind_rows(
  p1$results,
  p2$results,
  p3$results,
  p4$results,
  p5$results,
  p6$results
)


cat("\n========================================\n")
cat("SHAP + GAM + threshold results\n")
cat("========================================\n\n")

print(
  result_table
)


# 17. Export result table


write.csv(
  result_table,
  "GHG_SHAP_GAM_threshold_results.csv",
  row.names = FALSE
)


# 19. Save vector PDF


ggsave(
  filename = "GHG_SHAP_GAM_threshold.pdf",
  plot = final_plot,
  device = grDevices::pdf,
  width = 10,
  height = 6,
  units = "in",
  useDingbats = FALSE
)
