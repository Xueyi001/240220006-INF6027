>library(dplyr)
>library(readr)
>library(stringr)
> input_file <- "/Users/chenxueyi/Desktop/title_basics.tsv"
> output_dir <- "/Users/chenxueyi/Desktop/output"
> data_raw <- read_tsv(input_file, na = c("\\N", "NA", ""))
> data_cleaned <- data_raw %>% select(tconst, titleType, primaryTitle, startYear, runtimeMinutes, genres) %>% mutate( startYear = as.integer(startYear), runtimeMinutes = as.numeric(runtimeMinutes), genres = str_replace_all(genres, ",", "|") ) %>% na.omit()
> set.seed(42)
> data_sampled_100000 <- data_cleaned %>% sample_n(100000)
> write.csv(data_sampled_100000,
            +           file = file.path(output_dir, "data_cleaned_sampled_100000.csv"),
            +           row.names = FALSE)
> cat("finish! sample size：", nrow(data_cleaned), "；10,0000 samples have been saved to the output folder.\n")
> library(ggplot2)
> library(cluster)
> library(factoextra)
> library(caret)
> library(ranger)
> library(glmnet)
> library(reshape2)
> cluster_data <- data_sampled_100000 %>% select(runtimeMinutes, startYear) %>% scale()
> pca_result <- prcomp(cluster_data)
> pca_df <- as.data.frame(pca_result$x[, 1:2])
> set.seed(123)
> kmeans_model <- kmeans(cluster_data, centers = 4)
> pca_df$cluster <- as.factor(kmeans_model$cluster)
> data_sampled_100000$cluster <- kmeans_model$cluster
> write.csv(data_sampled_100000, file = file.path(output_dir, "data_with_clusters.csv"), row.names = FALSE)
> ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) + geom_point(alpha = 0.6) + labs(title = "K-Means Clustering (PCA Projection)") + theme_minimal()
> ggsave(file.path(output_dir, "kmeans_pca_plot.png"))
> model_data <- data_sampled_100000 %>% select(runtimeMinutes, startYear, cluster) %>% mutate(cluster = as.factor(cluster))
> set.seed(123)
> train_idx <- createDataPartition(model_data$cluster, p = 0.8, list = FALSE)
> train_data <- model_data[train_idx, ]
> test_data  <- model_data[-train_idx, ]
> rf_model <- ranger(cluster ~ ., data = train_data, probability = TRUE)
> rf_pred <- predict(rf_model, data = test_data)$predictions
> rf_pred_label <- colnames(rf_pred)[apply(rf_pred, 1, which.max)]
> conf_rf <- confusionMatrix(as.factor(rf_pred_label), test_data$cluster)
> print(conf_rf)
> cm_table <- as.data.frame(conf_rf$table)
> ggplot(cm_table, aes(Prediction, Reference, fill = Freq)) + geom_tile(color = "white") + geom_text(aes(label = Freq), size = 4) + scale_fill_gradient(low = "white", high = "steelblue") + labs(title = "Confusion Matrix", x = "Predicted", y = "Actual") + theme_minimal()
> ggsave(file.path(output_dir, "confusion_matrix_rf.png"))

> rf_model <- ranger( cluster ~ ., data = train_data, probability = TRUE, importance = "impurity")
> imp_vec <- rf_model$variable.importance
> importance_df <- data.frame(
  +     Feature = names(imp_vec),
  +     Importance = as.numeric(imp_vec),
  +     row.names = NULL
  + )
> print(importance_df)
> ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  +     geom_col() +
  +     coord_flip() +
  +     labs(title = "Random Forest Feature Importance", x = "Feature", y = "Importance") +
  +     theme_minimal()
> ggsave(file.path(output_dir, "rf_feature_importance.png"))
> x <- model.matrix(cluster ~ ., data = model_data)[, -1]
> y <- model_data$cluster
> set.seed(123)
> cv_glm <- cv.glmnet(x, y, alpha = 0, family = "multinomial")
> plot(cv_glm)
> saveRDS(cv_glm, file.path(output_dir, "ridge_model.rds"))

> library(zoo)
> library(forecast)
> ts_year_count <- data_sampled_100000 %>%
  +     group_by(startYear = as.integer(.data[["startYear"]])) %>%
  +     summarise(Count = n()) %>%
  +     arrange(startYear)
> ggplot(ts_year_count, aes(x = startYear, y = Count)) +
  +     geom_line(color = "steelblue", size = 1.2) +
  +     geom_smooth(method = "loess", se = FALSE, color = "red") +
  +     labs(title = "Number of Titles per Year", x = "Year", y = "Count") +
  +     theme_minimal()
> ggsave(file.path(output_dir, "titles_per_year.png"))
> data_sampled_100000 <- data_sampled_10000 %>%
  +     mutate(
    +         startYear = as.integer(startYear),
    +         runtimeMinutes = as.numeric(runtimeMinutes)
    +     )
> ts_runtime <- data_sampled_100000 %>%
  +     group_by(startYear) %>%
  +     summarise(avg_runtime = mean(runtimeMinutes)) %>%
  +     arrange(startYear)
> ggplot(ts_runtime, aes(x = startYear, y = avg_runtime)) +
  +     geom_line(color = "darkgreen", size = 1.2) +
  +     geom_smooth(method = "loess", se = FALSE, color = "orange") +
  +     labs(title = "Average Runtime per Year", x = "Year", y = "Runtime (min)") +
  +     theme_minimal()
> ggsave(file.path(output_dir, "avg_runtime_per_year.png"))
> runtime_ts <- ts(ts_runtime$avg_runtime, start = min(ts_runtime$startYear))
> fit <- auto.arima(runtime_ts)
> future <- forecast(fit, h = 10)
> autoplot(future) +
  +     labs(title = "Forecast: Average Runtime", x = "Year", y = "Runtime (min)") +
  +     theme_minimal()
> ggsave(file.path(output_dir, "runtime_forecast.png"))
> ggplot(ts_year_count, aes(x = startYear, y = Count)) +
  +     geom_line(color = "steelblue", size = 1) +
  +     geom_smooth(method = "loess", se = FALSE, color = "red") +
  +     labs(title = "Number of Titles per Year", x = "Year", y = "Count") +
  +     theme_minimal()
> ggsave(file.path(output_dir, "Number of Titles per Year.png"))

> library(tidyr)
> colnames(data_sampled_100000) <- tolower(colnames(data_sampled_100000))
> genres_long <- data_sampled_100000 %>%
  +     separate_rows(genres, sep = "\\|") %>%
  +     count(genres, sort = TRUE) %>%
  +     top_n(10)
> ggplot(genres_long, aes(x = "", y = n, fill = genres)) +
  +     geom_bar(stat = "identity", width = 1) +
  +     coord_polar(theta = "y") +
  +     labs(title = "Top 10 Genres Distribution") +
  +     theme_void()
> ggsave(file.path(output_dir, "genre_pie_chart.png"))
> ggplot(data_sampled_100000, aes(x = runtimeminutes, fill = titletype)) +
  +     geom_density(alpha = 0.4) +
  +     labs(title = "Runtime Distribution by Title Type", x = "Runtime (min)", y = "Density") +
  +     theme_minimal()
> ggsave(file.path(output_dir, "runtime_by_titleType.png"))
> ggplot(data_sampled_100000, aes(x = startyear)) +
  +     geom_histogram(binwidth = 1, fill = "skyblue", color = "white", alpha = 0.8) +
  +     geom_density(aes(y = ..count..), color = "red", size = 1, adjust = 2) +
  +     labs(title = "Distribution of Start Year", x = "Start Year", y = "Count") +
  +     theme_minimal()
> ggsave(file.path(output_dir, "distribution_startyear.png"))
> ggplot(data_sampled_100000, aes(x = runtimeminutes)) +
  +     geom_histogram(binwidth = 10, fill = "lightgreen", color = "black", alpha = 0.7) +
  +     geom_density(aes(y = ..count..), color = "darkgreen", size = 1) +
  +     labs(title = "Distribution of Runtime Minutes", x = "Runtime (min)", y = "Count") +
  +     theme_minimal()
> ggsave(file.path(output_dir, "distribution_runtimeMinutes.png"))
> genres_top10 <- data_sampled_100000 %>%
  +     separate_rows(genres, sep = "\\|") %>%
  +     count(genres, sort = TRUE) %>%
  +     top_n(10)
> ggplot(genres_top10, aes(x = reorder(genres, n), y = n)) +
  +     geom_col(fill = "coral") +
  +     coord_flip() +
  +     labs(title = "Top 10 Movie Genres", x = "Genre", y = "Count") +
  +     theme_minimal()
> ggsave(file.path(output_dir, "distribution_genres_top10.png"))