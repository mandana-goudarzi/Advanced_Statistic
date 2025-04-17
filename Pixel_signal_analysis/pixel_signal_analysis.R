library(tidyverse)
library(patchwork)
df <- read.csv("l2tree_maxedeptrack_str_20241119_072440_BTff3_ThetaMax60_TrgFlgAll_codver3_onlyMTX.csv")

upstream <- df[, 1:64]
downstream <- df[, 65:128]

# Checking missing values
print(sum(is.na(df)))

#  maximum and second maximum pixel values and indexes
results <- apply(upstream, 1, function(row) {
  max_val <- sort(row, decreasing=TRUE)[1]
  second_max <- sort(row, decreasing=TRUE)[2]
  max_idx <- which(row == max_val)[1]
  second_max_idx <- which(row== second_max)[1]
  max_idx <- max_idx - 1
  second_max_idx <- second_max_idx - 1
  c(max_val = max_val,
    second_max = second_max,
    max_idx = max_idx,
    second_max_idx = second_max_idx)
})
results_df <- as_tibble(t(results))
colnames(results_df) <- c("maximum value", "second maximum value", "maximum indices", "second maximum indices")

# Plot of distributions of the indices corresponding to the maximum and second maximum pixel values
p1 <- ggplot(results_df, aes(x = `maximum indices`)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of Maximum Pixel Index",
       x = "Pixel Index", y = "Count") +
  theme_minimal()
p2 <- ggplot(results_df , aes(x = `second maximum indices`)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of Second Maximum Pixel Index",
       x = "Pixel Index", y = "Count") +
  theme_minimal()
p1 + p2

# plot of excluding events where the maximum signal is less than 10
filtered_df <- results_df %>% filter(`maximum value` >= 10)
p1_filtered <- ggplot(filtered_df, aes(x = `maximum indices`)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Max Index (Max Value ≥ 10)",
       x = "Pixel Index", y = "Count") +
  theme_minimal()
p2_filtered <- ggplot(filtered_df , aes(x = `second maximum indices`)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Second Max Index (Max Value ≥ 10)",
       x = "Pixel Index", y = "Count") +
  theme_minimal()
p1_filtered + p2_filtered

# ratio between the second and first maximum values
results_df <- results_df %>% mutate(ratio= `second maximum value`/ `maximum value`)

#different signal ranges
filtered_range <- results_df %>% filter(`maximum value` > 0, `second maximum value` > 0)
filtered_range <- filtered_range %>% mutate(ratio = `second maximum value` / `maximum value`,
                                            signal_range = case_when(
                                              `maximum value` >= 10   & `maximum value` < 300    ~ "10–300",
                                              `maximum value` >= 300  & `maximum value` < 1200   ~ "300–1200",
                                              `maximum value` >= 1200 & `maximum value` < 30000  ~ "1200–30000",
                                              `maximum value` >= 30000                           ~ "≥30000"
                                            )
)
ggplot(filtered_range, aes(x=ratio, fill=signal_range)) + geom_histogram(binwidth=0.05,  position = "identity") +
  labs(title = "Distribution of Ratio (Second Max / Max) by Signal Range",
       x = "Ratio", y = "Count") +
  theme_minimal() 

# heatmaps illustrating the spatial distribution
spatial_dist <- results_df %>%
  mutate(
    max_row = `maximum indices` %% 8,
    max_col = `maximum indices` %/% 8,
    second_row = `second maximum indices` %% 8,
    second_col = `second maximum indices` %/% 8
  )
max_counts <- spatial_dist %>% count(max_row, max_col)
second_max_counts <- spatial_dist %>% count(second_row, second_col)

ggplot(max_counts, aes(x = max_col, y = max_row, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  labs(title = "Heatmap of Maximum Pixel Indices",
       x = "Column", y = "Row", fill = "Count") 

ggplot(second_max_counts, aes(x = second_col, y = second_row, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  labs(title = "Heatmap of Second Maximum Pixel Indices",
       x = "Column", y = "Row", fill = "Count") 

# maximum signal in the upstream matrix matches that of the downstream matrix
downstream_results <- apply(downstream, 1, function(row) {
  max_val <- max(row)
  max_idx <- which(row == max_val)[1] - 1  # zero-based index
  return(c(max_val = max_val, max_idx = max_idx))
})
  
downstream_df <- as.tibble(t(downstream_results))
colnames(downstream_df) <- c("downstream max value", "downstream max index")

combined_df <- bind_cols(results_df, downstream_df)

filtered_combined <- combined_df %>% filter(`maximum value` > 10)
match_sum <- filtered_combined %>% mutate(match = `maximum indices` == `downstream max index`) %>% count(match)
print(match_sum)

ggplot(match_sum, aes(x = match, y = n, fill = match)) +
  geom_col(width = 0.5) +
  labs(title = "Matching Max Pixel Indices (Upstream vs Downstream, Max > 10)",
       x = "Do They Match?", y = "Count")

# four most frequently occurring pairs of maximum pixel indice
top_paires <- combined_df %>% count(`maximum indices`, `downstream max index`, sort = TRUE)
top_4_paires <- top_paires %>% slice_max(order_by = n, n = 4)
print(top_4_paires)

