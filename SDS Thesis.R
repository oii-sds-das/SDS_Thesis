

#### COMBINED PLOTS ####


# Define a colour palette
colour_palette <- c("1" = "#A11360", "2" = "#FFD700", "3" = "#13627C")
# Define a colour palette
colour_palette <- c("1" = "#ff483a", "2" = "#ffb55f", "3" = "#15607a")
# Define a colour palette
colour_palette <- c("1" = "#df2644", "2" = "#ffd631", "3" = "#1898c2")
# Function to set the labels for the 'Rural' factor
set_rural_labels <- function() {
  scale_fill_manual(values = colour_palette, labels = c("Urban", "Intermediate", "Rural"))
}





#------------------.
# VIOLIN PLOTS
#------------------.

## Prepare Data
### 1. Select necessary columns and process data
violin_subset <- df_orig %>% 
  dplyr::select(NUTS_3, Projects, Rural, Population, `Int_Access`, Unemp, Education, GDP) %>% 
  dplyr::group_by(NUTS_3) %>%
  dplyr::summarise(
    Population = median(Population, na.rm = TRUE),
    Projects = sum(Projects, na.rm = TRUE),
    `Int_Access` = median(`Int_Access`, na.rm = TRUE),
    Unemp = median(Unemp, na.rm = TRUE),
    Education = median(Education, na.rm = TRUE),
    GDP = median(GDP, na.rm = TRUE),
    Rural = first(Rural) # Take the first Rural value for each group
  ) %>%
  dplyr::mutate(projects_per_10000 = Projects / Population * 10000) # Create a variable for number of projects per 10,000 people

### 2. Handle factors and missing values
violin_subset$Rural <- factor(violin_subset$Rural, levels = c(1, 2, 3), labels = c('Urban', 'Intermediate', 'Rural'))
violin_subset <- na.omit(violin_subset) # Remove rows with NA values

### 3. Convert data to long format
violin_long <- violin_subset %>% gather(key = "Variable", value = "Value", -NUTS_3, -Rural, -Population, -Projects)

### 4. Standardize data
violin_long <- violin_long %>% group_by(Variable) %>% mutate(ValueStd = scale(Value))

## Plotting
violin_plot <- ggplot(violin_long, aes(x = Rural, y = Value, fill = Rural)) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.4, method = "histodot", colour = "white") +
  geom_boxplot(width = 0.5, alpha = 0, outlier.shape = NA, lwd = 0.6) +
  scale_y_log10() +
  labs(x = "Type of Region", y = "Metrics Value (Log Scale)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none") +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) # Create separate panels for each variable
print(violin_plot)
## Save plot to PNG
#ggsave(filename = "violin_plot.png", plot = violin_plot, width = 10, height = 6, dpi = 300, bg = "white")


#------------------.
# TIME SERIES PLOTS
#------------------.

## Prepare Data
### 1. Aggregate and process data for projects
data_agg_pr <- aggregate(cbind(Projects, Population) ~ year + Rural,  
                         data = df_orig,
                         FUN = sum, 
                         na.rm = TRUE)
names(data_agg_pr)[3] <- "sum_projects" 
names(data_agg_pr)[4] <- "sum_population"
data_agg_pr <- data_agg_pr %>%
  mutate(projects_per_10000 = (sum_projects / sum_population) * 10000)
totals <- data_agg_pr %>%
  group_by(year) %>%
  summarise(total_projects = sum(projects_per_10000, na.rm = TRUE))
data_agg_pr <- left_join(data_agg_pr, totals, by = "year") %>%
  mutate(prop_projects = projects_per_10000 / total_projects)


### 2. Set titles for the plot
titles <- c(
  "Unemp" = "Unemployment Rate",
  "Int_Access" = "Internet Access Rate",
  "Education" = "Education Level",
  "Population" = "Population Count in thousands",
  "GDP" = "GDP per capita",
  "prop_projects" = "Proportion of Projects per 10k people"
)

### 3. Aggregate other variables
data_agg_temp <- list(
  Unemp = df_orig %>% aggregate(Unemp ~ year + Rural, ., median),
  `Int_Access` = df_orig %>% aggregate(`Int_Access` ~ year + Rural, ., median),
  Education = df_orig %>% aggregate(Education ~ year + Rural, ., median),
  Population = df_orig %>% aggregate(Population ~ year + Rural, ., median),
  GDP = df_orig %>% aggregate(GDP ~ year + Rural, ., median)
) %>%
  Reduce(function(x, y) merge(x, y, by=c("year", "Rural")), .)

data_agg_temp$Population <- data_agg_temp$Population / 1e3 # Adjust the population to 10k units

data_agg <- data_agg_temp %>%
  left_join(data_agg_pr, by = c("year", "Rural")) %>%
  gather(key = "Variable", value = "Value", -year, -Rural) %>%
  filter(Variable %in% names(titles))

## Plotting
trends_plot <- ggplot(data_agg, aes(x = year, y = Value, color = factor(Rural))) +
  geom_line(size = 0.7, alpha = 0.7) +
  scale_color_manual(values = colour_palette, labels = c("Urban", "Intermediate", "Rural")) +  # Updated this line to use your custom colour palette
  labs(
    title = "Timeline: Various Metrics",
    x = "Year", 
    y = "Median Value",
    color = "Region Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0),
    panel.grid.major = element_line(color = "grey92"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  scale_x_continuous(breaks = unique(data_agg$year)) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 3, labeller = as_labeller(titles))

print(trends_plot)
## Save plot to PNG
# ggsave(filename = "trends_plot.png", plot = trends_plot, width = 10, height = 6, dpi = 300, bg = "white")


# Export the plot to a PDF file
ggsave("trends_plot.pdf", plot = trends_plot, width = 12, height = 6)




# Export the plot to a PDF file in A4 format
ggsave("trends_plot_a4.pdf", plot = trends_plot, device = "pdf", width = 8.3, height = 11.7)




#--------------------------------------------------.
# PROJECTS CIRCLES BY REGION TYPE AND COUNTRY 2020
#--------------------------------------------------.


# Filter out NaN in the Rural column
dataset <- subset(df_orig, !is.na(Rural))

# Filter the data for the year 2020
dataset_2020 <- subset(dataset, year == 2020)

# Aggregate the data by 'COUNTRY' and 'Rural', summing 'Projects' and 'Population'
grouped_data <- dataset_2020 %>% 
  group_by(COUNTRY, Rural) %>%
  summarise(Projects = sum(Projects), Population = sum(Population)) %>%
  mutate(Projects_per_million = (Projects / Population) * 1e6)

# Calculate the median project count per 1 million population for each country
median_project_count <- grouped_data %>% 
  group_by(COUNTRY) %>%
  summarise(Median_Projects = median(Projects_per_million, na.rm = TRUE)) %>%
  arrange(Median_Projects)

# Merge the median project count back into the original grouped data
grouped_data <- merge(grouped_data, median_project_count, by = "COUNTRY")

# Create the bubble plot with adjustments
final_plot <- ggplot(grouped_data, aes(x = reorder(COUNTRY, Median_Projects), y = Projects_per_million, fill = factor(Rural), size = Projects)) +
  geom_jitter(alpha = 0.6, width = 0.3, shape = 21, color = "black", stroke = 1) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides = "l") +
  scale_size_continuous(breaks = c(1, 10, 100, 250, 500, 1000), range = c(1, 10)) +
  set_rural_labels() +
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  labs(title = "Projects Count Per 1 Million Population by Country and Area Type (2020)",
       x = "Country",
       y = "Projects Count Per 1 Million Population",
       fill = "Region Type",
       size = "Total Projects") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "right",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())  # Remove dotted horizontal lines

# Show the plot
print(final_plot)

ggsave(filename = "project_circles.png", plot = final_plot, width = 10, height = 4, dpi = 300, bg = "white")









#------------- Imbalance between Urban/Rural Regions in OLM -------------------.

# Read the full dataset
all_data <- df_orig

# Aggregate the data by 'COUNTRY' and 'Rural', summing 'Projects' and 'Population'
grouped_all_data <- all_data %>%
  group_by(COUNTRY, Rural) %>%
  summarise(Projects = sum(Projects), Population = sum(Population)) %>%
  mutate(Projects_per_million = (Projects / Population) * 1e6)

# Count the total number of projects for each country
total_projects <- all_data %>%
  group_by(COUNTRY) %>%
  summarise(Total_Projects = sum(Projects))

# Pivot the data to wide format
wide_all_data <- grouped_all_data %>%
  pivot_wider(id_cols = COUNTRY, names_from = Rural, values_from = Projects_per_million, names_prefix = "Area_")

# Calculate the imbalance ratio excluding intermediate areas
wide_all_data <- wide_all_data %>%
  mutate(Imbalance_Ratio = Area_1 / Area_3) %>%
  arrange(-Imbalance_Ratio)

# Merge the total number of projects back to the wide_all_data
wide_all_data <- merge(wide_all_data, total_projects, by = "COUNTRY")

# Create the bar plot with gradient colour
imbalance_plot <- ggplot(wide_all_data, aes(x = reorder(COUNTRY, -Imbalance_Ratio), y = Imbalance_Ratio)) +
  geom_bar(stat = "identity", aes(fill = Imbalance_Ratio)) +
  scale_fill_gradient(low = "#E68A9B", high = "#E0435F") +
  geom_text(aes(label = formatC(Total_Projects, big.mark = ",", format = "d")), 
            angle = 90, y=0.4, hjust=0.2, vjust=0.3, color = "#611D2A") +
  labs(title = "Online Labour Markets Imbalance Between Urban and Rural Areas in Online Labour Markets (2013-2020)",
       x = "Country",
       y = "Imbalance Ratio",
       fill = "Imbalance Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none")

# Show the plot
print(imbalance_plot)

ggsave(filename = "imbalance_plot.png", plot = imbalance_plot, width = 10, height = 4, dpi = 300, bg = "white")

wide_all_data$COUNTRY





#----------- Table: MAE	MSE	RMSE Correlation AIC	BIC -------------------.
# Prepare a list of models and their names
models <- list(m_ols_log1p, m_ols_log1p_fe, m_ols_log1p_re, m_ols_log, m_nbin, m_mult_mix_log1p, m_nb_fe, m_gam)
model_names <- c("OLS\n(log1p)", "OLS FE\n(log1p)", "OLS RE\n(log1p)",
                 "OLS NAs\n(LOG)", "NB\n(No Transf)", "Mixed FE\n(log1p)",
                 "NB Mixed FE\n(No Transf)", "GAM Model\n(LOG1P)")

# Initialize an empty data frame to store the results
results <- data.frame(Model = character(), MAE = numeric(), MSE = numeric(), RMSE = numeric(), Correlation = numeric(), AIC = numeric(), BIC = numeric())

# Loop over the models
for (i in 1:length(models)) {
  # Get the predicted values
  preds <- predict(models[[i]], newdata = df_no_missing %>% filter(all_vars == 1))
  
  # Calculate the metrics
  actuals <- df_no_missing$Projects.log1p
  mae <- mean(abs(preds - actuals))
  mse <- mean((preds - actuals)^2)
  rmse <- sqrt(mse)
  cor <- cor(preds, actuals)
  aic <- AIC(models[[i]])
  bic <- BIC(models[[i]])
  
  # Add the results to the data frame
  results <- rbind(results, data.frame(Model = model_names[i], MAE = mae, MSE = mse, RMSE = rmse, Correlation = cor, AIC = aic, BIC = bic))
}

# Normalize the measures so they're all on the same scale
results$normalized_MAE <- scale(results$MAE)
results$normalized_MSE <- scale(results$MSE)
results$normalized_RMSE <- scale(results$RMSE)
results$normalized_Corr <- scale(results$Correlation)
results$normalized_AIC <- scale(results$AIC)
results$normalized_BIC <- scale(results$BIC)

# Create a score that is the average of the normalized measures
# For MAE, MSE, RMSE, AIC, and BIC, lower is better so we multiply by -1
# For Correlation, higher is better so we don't multiply by -1
results$Score <- rowMeans(data.frame(
  -results$normalized_MAE,
  -results$normalized_MSE,
  -results$normalized_RMSE,
  results$normalized_Corr,
  -results$normalized_AIC,
  -results$normalized_BIC
))

# Now we can sort by the Score
results <- results[order(results$Score, decreasing = TRUE),]

# Drop the normalized and score columns
results <- subset(results, select = -c(normalized_MAE, normalized_MSE, normalized_RMSE, 
                                       normalized_Corr, normalized_AIC, normalized_BIC, Score))


# Define color vectors based on relative performance
pastel_green <- "#66C776"
pastel_red <- "#E8646F"
pastel_yellow <- "#FFFFFF"

color_RMSE <- color_Corr <- c()

for(i in 1:nrow(results)){
  if(i == 1){
    color_RMSE <- c(color_RMSE, pastel_green) 
    color_Corr <- c(color_Corr, pastel_red) 
  } else if(i == nrow(results)){
    color_RMSE <- c(color_RMSE, pastel_red)
    color_Corr <- c(color_Corr, pastel_green)
  } else {
    color_RMSE <- c(color_RMSE, pastel_yellow)
    color_Corr <- c(color_Corr, pastel_yellow)
  }
}

gt(results) %>%
  fmt_markdown(columns = c("Model")) %>%
  tab_header(title = "Model Performance") %>%
  tab_style(style = cell_fill(color = "white"), 
            locations = cells_body(columns = "Model")) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = "Model") %>%
  tab_style(style = cell_borders(sides = "all", color = "gray", weight = px(1)), 
            locations = cells_body(columns = everything())) %>%
  data_color(columns = c("MAE", "MSE", "RMSE"), colors = color_RMSE) %>%
  data_color(columns = c("Correlation"), colors = color_Corr) %>%
  data_color(columns = c("AIC", "BIC"), colors = color_RMSE)





#--------------- Cross-validated MAE / Correlation ------------------- 10-fold CV loops

library(tidyverse)
library(rsample)
library(ggplot2)
library(mgcv)

#------------------- 1. Setup and Data Filtering -------------------#
df <- df_orig %>% filter(all_vars == 1)

threshold <- 50 
rare_countries <- df %>%
  group_by(COUNTRY) %>%
  tally() %>%
  filter(n < threshold) %>%
  pull(COUNTRY)

df <- df %>%
  filter(!(COUNTRY %in% rare_countries))

#------------------- 2. Model Definition -------------------#

# List of model formulas
model_formulas <- list(
  ols_ihs = list(
    formula = ihs(Projects) ~ log(Population) + factor(Rural) + Int_Access + log(GDP) + Education + log(Unemp),
    func = lm,
    response_transformation = "ihs"
  ),
  ols_log1p = list(
    formula = log1p(Projects) ~ log(Population) + factor(Rural) + Int_Access + log(GDP) + Education + log(Unemp),
    func = lm,
    response_transformation = "log1p" 
  ),
  m_ols_log1p_fe = list(
    formula = log1p(Projects) ~ log(Population) + factor(Rural) + Int_Access + log(GDP) + Education + log(Unemp),
    func = plm,
    index = c("COUNTRY", "year"),
    model = "within",
    effect = "twoways",
    effect.id = TRUE,
    response_transformation = "log1p" 
  ),
  m_nb_fe = list(
    formula = Projects ~ log(Population) + factor(Rural) + Int_Access + log(GDP) + Education + log(Unemp) + (1 | COUNTRY) + (1 | COUNTRY:NUTS_3) + (1 | year),
    func = glmmTMB,
    family = nbinom2,
    response_transformation = "raw"
  ),
  gam_log1p = list(
    formula = log1p(Projects) ~ s(log(Population)) + factor(Rural) + s(Int_Access) + s(log(GDP)) + s(Education) + s(log(Unemp)) + factor(COUNTRY) + factor(year),
    func = gam,
    response_transformation = "log1p" 
  ),
  m_mult_mix_log1p = list(
    formula = log1p(Projects) ~ log(Population) + factor(Rural) + Int_Access + log(GDP) + Education + log(Unemp) + (1 | COUNTRY) + (1 | COUNTRY:NUTS_3) + (1 | year),
    func = lmer,
    response_transformation = "log1p"
  )
)

#------------------- 3. Cross-Validation Loop -------------------#
set.seed(17)
folds <- vfold_cv(df, v = 10)

results <- folds$splits %>%
  map_dfr(function(split) {
    train_data <- training(split)
    test_data <- testing(split)
    test_data <- pdata.frame(test_data, index = c("COUNTRY", "year"))
    
    imap_dfr(model_formulas, function(model_details, model_name) {
      args_list <- list(formula = model_details$formula, data = train_data)
      if ("index" %in% names(model_details)) {
        args_list$index <- model_details$index
      }
      model <- do.call(model_details$func, args_list)
      preds <- predict(model, newdata = test_data)
      
      if (model_details$response_transformation == "log") {
        preds <- exp(preds)
        true_values <- test_data$Projects.NA
      } else if (model_details$response_transformation == "log1p") {
        preds <- expm1(preds)
        true_values <- test_data$Projects
      } else if (model_details$response_transformation == "ihs") {
        preds <- sinh(preds)
        true_values <- test_data$Projects
      } else if (model_details$response_transformation == "raw") {
        true_values <- test_data$Projects
      }
      
      tibble(
        model = model_name,
        mae = mean(abs(true_values - preds), na.rm = TRUE),
        rho = cor(true_values, preds, use = "complete.obs")
      )
    })
  })

# make sure the models are in the correct order
desired_order <- names(model_formulas)
results$model <- factor(results$model, levels = desired_order)

#------------------- 4. Performance Evaluation and Plotting -------------------#
testPerformance <- results %>%
  pivot_longer(cols = c(mae, rho), names_to = "measure", values_to = "value")

cv.plot <- ggplot(testPerformance, aes(x = model, y = value, col = model, fill = model)) + 
  geom_point(position = position_jitter(0.1), shape = 21, col = "black", size = 3, stroke = 0.2) + 
  stat_summary(fun.y = mean, geom = "point", size = 2) + 
  stat_summary(fun.y = mean, geom = "point", size = 1, col = "white") + 
  facet_wrap(~ measure, scales = "free") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", fun.args = list(mult = 2), width = 0.2, lwd = 1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", fun.args = list(mult = 2), width = 0.18, lwd = 0.1, col = "white") +
  scale_x_discrete(labels = c("OLS\n(ihs)", "OLS\n(log1p)", "FE\n(log1p)", "NB\nMixed", "GAM\n(log1p)", "Mult-L\nMixed log1p")) +
  labs(x = "Model", y = "Cross-validated MAE / Correlation") + 
  theme_bw() + theme(panel.grid = element_blank(), legend.position = "none", text = element_text(size = 14))

print(cv.plot)
