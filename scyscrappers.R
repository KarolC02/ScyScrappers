library(tidyverse)
library(here)
library(viridis)
library(ggpmisc)

#######  Section 1 - By country #######
tallest_usa_2025 <- read_csv(here("data", "by_country", "100_tallest_constructed_USA.csv"))
tallest_china_2025 <- read_csv(here("data", "by_country", "100_tallest_constructed_China.csv"))
tallest_north_korea_2025 <- read_csv(here("data", "by_country", "100_tallest_constructed_North_Korea.csv"))
tallest_uae_2025 <- read_csv(here("data", "by_country", "100_tallest_constructed_UAE.csv"))
tallest_world_2025 <- read_csv(here("data", "by_decade", "100_tallest_constructed_world_2025_with_countries.csv"))
#######  Section 1 - By country #######

#######  Section 2 - Load Tallest building in each decade #######
decades <- seq(1890, 2020, 10)
decades <- union(decades, 2025)
decades
tallest_world_list <- lapply(decades, function(year) {
  path <- here("data", "by_decade", paste0("100_tallest_constructed_world_", year, "_with_countries.csv"))
  df <- read_csv(path)
  assign(paste0("tallest_world_", year), df, envir = .GlobalEnv)
  return(df)
})
names(tallest_world_list) <- paste0("tallest_world_", decades)
#######  Section 2 - Load Tallest building in each decade #######

###### Helper Function for filling missing heights #######
fill_missing_heights <- function(df) {
  heights <- df$`Height (m)`
  
  for (i in which(is.na(heights))) {
    filled_val <- NA
    
    if (i == 1) {
      next_val <- heights[which(!is.na(heights))[which(!is.na(heights)) > 1][1]]
      filled_val <- next_val
    } else if (i == length(heights)) {
      prev_val <- rev(heights[1:(i - 1)])[which(!is.na(rev(heights[1:(i - 1)])))[1]]
      filled_val <- prev_val
    } else {
      above <- rev(heights[1:(i - 1)])
      below <- heights[(i + 1):length(heights)]
      
      above_val <- above[which(!is.na(above))[1]]
      below_val <- below[which(!is.na(below))[1]]
      
      if (!is.na(above_val) && !is.na(below_val)) {
        filled_val <- mean(c(above_val, below_val))
      } else if (!is.na(above_val)) {
        filled_val <- above_val
      } else if (!is.na(below_val)) {
        filled_val <- below_val
      }
    }
    
    heights[i] <- filled_val
    message("Filled the height of '", df$Name[i], "' to be ", round(filled_val, 2))
  }
  
  df$`Height (m)` <- heights
  
  if (!all(df$`Height (m)` == sort(df$`Height (m)`, decreasing = TRUE))) {
    stop("Post-fill check failed: Heights are no longer in decreasing order.")
  }
  
  return(df)
}

###### Helper Function for filling missing heights #######


## Any missing countries?
for (year in decades) {
  df_name <- paste0("tallest_world_", year)
  df <- get(df_name)
  
  if (!"Country" %in% names(df)) {
    message(df_name, ": ❌ no 'Country' column found.")
    next
  }
  
  n_missing <- sum(is.na(df$Country))
  
  if (n_missing > 0) {
    message(df_name, ": ⚠️ ", n_missing, " missing value(s) in 'Country'")
  } else {
    message(df_name, ": ✅ no missing 'Country' values")
  }
}





tallest_north_korea_2025 <- fill_missing_heights(tallest_north_korea_2025)

######## Buffor for checking for any missing values ########
colSums(is.na(tallest_world_2025))

plot_top5_by_country <- function(df, country_name) {
  if (!dir.exists(here("graphs"))) {
    dir.create(here("graphs"))
  }
  
  top5 <- df %>%
    arrange(desc(`Height (m)`)) %>%
    slice_head(n = 5)
  
  p <- ggplot(top5, aes(x = reorder(Name, `Height (m)`), y = `Height (m)`)) +
    geom_col(fill = "#0072BD") +
    geom_text(aes(label = `Height (m)`), hjust = -0.1, size = 4) +
    coord_flip() +
    theme_minimal() +
    labs(
      title = paste("Top 5 Tallest Constructed Buildings in", country_name, "(April 2025)"),
      x = "Building",
      y = "Height (m)"
    )
  
  print(p)
  
  ggsave(
    filename = here("graphs", paste0("top5_", tolower(country_name), ".png")),
    plot = p,
    width = 8,
    height = 6
  )
}
######## Buffor for checking for any missing values ########


######## Fill missing heights for each decade #########
for (year in decades) {
  df_name <- paste0("tallest_world_", year)
  df <- get(df_name)
  
  if (any(is.na(df$`Height (m)`))) {
    message("📐 Filling heights in ", df_name)
    df <- fill_missing_heights(df)
    assign(df_name, df, envir = .GlobalEnv)
  } else {
    message("✅ No missing heights in ", df_name)
  }
}

######## Fill missing heights for each decade #########


######### 5 Tallest by Country##########
plot_top5_by_country(tallest_usa_2025, "USA")
plot_top5_by_country(tallest_china_2025, "China")
plot_top5_by_country(tallest_north_korea_2025, "North Korea")
plot_top5_by_country(tallest_uae_2025, "UAE")
plot_top5_by_country(tallest_world_2025, "World")
######### 5 Tallest by Country##########


######### 5 Tallest by Country Comparison ##########
build_comparison_df <- function(df, country) {
  df %>%
    arrange(desc(`Height (m)`)) %>%
    slice_head(n = 5) %>%
    mutate(Rank = 1:5, Country = country) %>%
    select(Country, Rank, Name, `Height (m)`)
}

comparison_df <- bind_rows(
  build_comparison_df(tallest_usa_2025, "USA"),
  build_comparison_df(tallest_china_2025, "China"),
  build_comparison_df(tallest_north_korea_2025, "North Korea"),
  build_comparison_df(tallest_uae_2025, "UAE")
)


p <- ggplot(comparison_df, aes(x = factor(Rank), y = `Height (m)`, fill = Country)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_viridis_d(option = "C", end = 0.85) +
  theme_minimal() +
  labs(
    title = "Top 5 Tallest Constructed Buildings in 2025: Country Comparison",
    x = "Rank (1st to 5th tallest)",
    y = "Height (m)"
  )

print(p)

ggsave(
  filename = here("graphs", "top5_country_comparison_2025.png"),
  plot = p,
  width = 10,
  height = 6
)
######### 5 Tallest by Country Comparison ##########


####### Helper for Horizontal Bar Chart by decade #######
plot_country_distribution_bar <- function(df, year) {
  if (!dir.exists(here("graphs"))) {
    dir.create(here("graphs"))
  }
  
  if ("Country" %in% names(df)) {
    countries <- df$Country
  } else if ("City" %in% names(df)) {
    countries <- df$City
  } else {
    stop("No 'Country' or 'City' column found.")
  }
  
  counts <- as_tibble(table(countries)) %>%
    rename(Country = countries, Count = n) %>%
    arrange(desc(Count)) %>%
    slice_head(n = 10)
  
  if (nrow(counts) < 10) {
    blanks <- tibble(
      Country = paste0(" ", seq_len(10 - nrow(counts))),
      Count = rep(0, 10 - nrow(counts))
    )
    counts <- bind_rows(counts, blanks)
  }
  
  counts <- counts %>%
    mutate(Country = factor(Country, levels = rev(Country)))
  
  p <- ggplot(counts, aes(x = Country, y = Count)) +
    geom_col(fill = "#0072BD", width = 0.7) +
    geom_text(aes(label = Count), hjust = -0.2, size = 3.5) +
    coord_flip() +
    theme_minimal() +
    labs(
      title = paste("Top 10 Countries by Number of Buildings in the 100 Tallest –", year),
      x = NULL,
      y = "Number of Buildings"
    ) +
    theme(
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold")
    )
  
  print(p)
  
  ggsave(
    filename = here("graphs", paste0("country_distribution_", year, ".png")),
    plot = p,
    width = 8,
    height = 6
  )
}
####### Helper for Horizontal Bar Chart by decade #######

###### Top 100 tallest building by country for eache decade #####
for (year in decades) {
  df_name <- paste0("tallest_world_", year)
  
  if (exists(df_name)) {
    message("📊 Generating plot for ", df_name)
    df <- get(df_name)
    plot_country_distribution_bar(df,year)
  } else {
    message("⚠️ Skipping missing: ", df_name)
  }
}
###### Top 100 tallest building by country for eache decade #####






















###### Top 100 tallest buildings by function for each decade ######
# Define function standardizer
standardize_function <- function(func_col) {
  func_col <- as.character(func_col)
  func_col[is.na(func_col) | trimws(func_col) %in% c("", "NA", "Unknown")] <- "Unknown"
  ifelse(grepl("/", func_col), "Mixed", func_col)
}

# Apply standardization to all decades
for (year in decades) {
  df_name <- paste0("tallest_world_", year)
  if (exists(df_name)) {
    df <- get(df_name)
    if ("Function" %in% names(df)) {
      df$Function <- standardize_function(df$Function)
      assign(df_name, df, envir = .GlobalEnv)
    }
  }
}

# Collect all unique function labels
all_functions <- character(0)
for (year in decades) {
  df_name <- paste0("tallest_world_", year)
  if (exists(df_name)) {
    df <- get(df_name)
    if ("Function" %in% names(df)) {
      funcs <- unique(df$Function)
      all_functions <- union(all_functions, funcs[!is.na(funcs)])
    }
  }
}
all_functions <- sort(unique(all_functions))

# Define known standard levels (from prior analysis)
known_levels <- c("Mixed", "Hotel", "Residential", "Office", "Religious", 
                  "Education", "Retail", "Government", "Industrial", 
                  "Observation",  "Unknown", "Belltower")

# Generate function level order based on 2025 data
function_order_2025 <- standardize_function(tallest_world_2025$Function)
function_order_2025 <- as.data.frame(table(function_order_2025)) %>%
  rename(Function = function_order_2025, Count = Freq) %>%
  complete(Function = known_levels, fill = list(Count = 0)) %>%
  mutate(Function = factor(Function, levels = known_levels)) %>%
  arrange(desc(Count)) %>%
  pull(Function)

# Warn about any missing Function values across all decades
for (year in decades) {
  df_name <- paste0("tallest_world_", year)
  if (exists(df_name)) {
    df <- get(df_name)
    if ("Function" %in% names(df)) {
      n_missing <- sum(is.na(df$Function))
      if (n_missing > 0) {
        message("⚠️  ", year, ": ", n_missing, " missing Function value(s)")
        cat("  → Rows with missing Function:\n")
        print(df[is.na(df$Function), c("Name", "City", "Height (m)")])
      }
    }
  }
}



plot_function_distribution_bar <- function(df, year) {
  if (!dir.exists(here("graphs"))) {
    dir.create(here("graphs"))
  }
  
  if (!"Function" %in% names(df)) {
    stop("No 'Function' column found.")
  }
  
  counts <- as.data.frame(table(df$Function, useNA = "no")) %>%
    rename(Function = Var1, Count = Freq) %>%
    complete(Function = function_order_2025, fill = list(Count = 0)) %>%
    mutate(Function = factor(Function, levels = rev(function_order_2025)))
  
  p <- ggplot(counts, aes(x = Function, y = Count)) +
    geom_col(fill = "#0072BD", width = 0.7) +
    geom_text(aes(label = ifelse(Count == 0, "-", Count)), hjust = -0.2, size = 3.5) +
    coord_flip() +
    theme_minimal() +
    labs(
      title = paste("Top 100 Tallest Buildings by Function –", year),
      x = NULL,
      y = "Number of Buildings"
    ) +
    theme(
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold")
    )
  
  print(p)
  
  ggsave(
    filename = here("graphs", paste0("function_distribution_", year, ".png")),
    plot = p,
    width = 8,
    height = 6
  )
}

for (year in decades) {
  df_name <- paste0("tallest_world_", year)
  if (exists(df_name)) {
    df <- get(df_name)
    plot_function_distribution_bar(df, year)
  }
}
###### Top 100 tallest building by function for eache decade #####






plot_function_distribution_bar(tallest_world_1890, 1890)





###### Top 100 tallest building by material for eache decade #####

all_materials <- character(0)

standardize_material <- function(x) {
  ifelse(is.na(x) | trimws(x) == "", "Unknown", x)
}

for (year in decades) {
  df_name <- paste0("tallest_world_", year)
  
  if (exists(df_name)) {
    df <- get(df_name)
    
    if ("Material" %in% names(df)) {
      materials <- unique(df$Material)
      materials <- materials[!is.na(materials)]
      new_mats <- materials[!materials %in% all_materials]
      
      if (length(new_mats) > 0) {
        message("➕ New values from ", df_name, ": ", paste(new_mats, collapse = ", "))
        all_materials <- c(all_materials, new_mats)
      }
    } else {
      message("⚠️ No 'Material' column in ", df_name)
    }
  }
}

# Also check 2025 explicitly
if (exists("tallest_world_2025")) {
  df <- tallest_world_2025
  
  if ("Material" %in% names(df)) {
    materials <- unique(df$Material)
    materials <- materials[!is.na(materials)]
    new_mats <- materials[!materials %in% all_materials]
    
    if (length(new_mats) > 0) {
      message("➕ New values from tallest_world_2025: ", paste(new_mats, collapse = ", "))
      all_materials <- c(all_materials, new_mats)
    }
  }
}

all_materials <- sort(all_materials)
all_materials

material_order_2025 <- tallest_world_2025$Material
material_order_2025 <- ifelse(is.na(material_order_2025) | trimws(material_order_2025) == "", "Unknown", material_order_2025)
material_order_2025 <- standardize_material(material_order_2025)

material_order_2025 <- as.data.frame(table(material_order_2025)) %>%
  rename(Material = material_order_2025, Count = Freq) %>%
  complete(Material = c(
    "All-Concrete", "All-Masonry", "All-Steel",
    "Concrete Over Steel", "Concrete-Steel Composite",
    "Steel Over Concrete", "Composite", "Unknown"
  ), fill = list(Count = 0)) %>%
  arrange(desc(Count)) %>%
  pull(Material)


plot_material_distribution_bar <- function(df, year) {
  if (!dir.exists(here("graphs"))) {
    dir.create(here("graphs"))
  }
  
  if (!"Material" %in% names(df)) {
    stop("No 'Material' column found.")
  }
  
  df$Material <- standardize_material(df$Material)
  
  counts <- as.data.frame(table(df$Material)) %>%
    rename(Material = Var1, Count = Freq) %>%
    arrange(desc(Count))
  
  target_materials <- c(
    "All-Concrete", "All-Masonry", "All-Steel",
    "Concrete Over Steel", "Concrete-Steel Composite",
    "Steel Over Concrete", "Composite", "Unknown"
  )
  
  counts <- counts %>%
    complete(Material = target_materials, fill = list(Count = 0)) %>%
    mutate(Material = factor(Material, levels = rev(material_order_2025)))
  
  p <- ggplot(counts, aes(x = Material, y = Count)) +
    geom_col(fill = "#0072BD", width = 0.7) +
    geom_text(aes(label = ifelse(Count == 0, "-", Count)), hjust = -0.2, size = 3.5) +
    coord_flip() +
    theme_minimal() +
    labs(
      title = paste("Top 100 Tallest Buildings by Material –", year),
      x = NULL,
      y = "Number of Buildings"
    ) +
    theme(
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold")
    )
  
  print(p)
  
  ggsave(
    filename = here("graphs", paste0("material_distribution_", year, ".png")),
    plot = p,
    width = 8,
    height = 6
  )
}

for (year in decades) {
  df_name <- paste0("tallest_world_", year)
  if (exists(df_name)) {
    df <- get(df_name)
    plot_material_distribution_bar(df, year)
  }
}


###### Top 100 tallest building by material for eache decade #####













###### Average Height of the 100 Tallest Buildings by Decade (1890–2025) ######

# Ensure no missing heights and compute averages
average_heights <- numeric(length(decades))

for (i in seq_along(decades)) {
  year <- decades[i]
  df_name <- paste0("tallest_world_", year)
  
  if (!exists(df_name)) {
    message("⚠️ Missing dataset: ", df_name)
    next
  }
  
  df <- get(df_name)
  
  if (any(is.na(df$`Height (m)`))) {
    message("📐 Filling missing heights in ", df_name)
    df <- fill_missing_heights(df)
    assign(df_name, df, envir = .GlobalEnv)
  }
  
  if (any(is.na(df$`Height (m)`))) {
    stop("❌ Still missing heights in ", df_name)
  }
  
  average_heights[i] <- mean(df$`Height (m)`)
}

# Prepare data frame
avg_height_df <- tibble(
  Decade = decades,
  `Average Height (m)` = round(average_heights, 2)
)

p <- ggplot(avg_height_df, aes(x = Decade, y = `Average Height (m)`)) +
  geom_line(color = "#0072BD", size = 1.2) +
  geom_point(size = 2) +
  geom_text(aes(label = `Average Height (m)`), vjust = -1.2, size = 3.5) +
  scale_x_continuous(
    breaks = avg_height_df$Decade,
    labels = avg_height_df$Decade,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  theme_minimal() +
  labs(
    title = "Average Height of the 100 Tallest Buildings by Decade",
    x = "Decade",
    y = "Average Height (m)"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

print(p)

ggsave(
  filename = here("graphs", "average_height_by_decade.png"),
  plot = p,
  width = 10,
  height = 6
)


###### Average Height of the 100 Tallest Buildings by Decade (1890–2025) ######





# Combine all buildings across decades
all_buildings_df <- map_df(decades, function(year) {
  df_name <- paste0("tallest_world_", year)
  if (exists(df_name)) {
    df <- get(df_name)
    df <- df %>%
      mutate(Decade = year) %>%
      select(Name, `Completion Year`, Decade)
    return(df)
  }
  return(NULL)
})

# Remove duplicates
unique_buildings <- all_buildings_df %>%
  distinct(Name, .keep_all = TRUE)

# Filter those completed before 1800
pre_1800_buildings <- unique_buildings %>%
  filter(!is.na(`Completion Year`), `Completion Year` < 1800)

# Extract names
pre_1800_names <- pre_1800_buildings$Name

# Save to txt
writeLines(pre_1800_names, here("graphs", "pre_1800_tallest_buildings.txt"))

cat("✅ Saved", length(pre_1800_names), "building names to pre_1800_tallest_buildings.txt\n")















###### Material vs. Height Distribution Across All Decades ######

###### Material vs. Height Distribution Across All Decades – Ordered by Median ######

# Helper to standardize material
standardize_material <- function(x) {
  x <- as.character(x)
  x[is.na(x) | trimws(x) == ""] <- "Unknown"
  x
}

# Define target material categories
target_materials <- c(
  "All-Concrete", "All-Masonry", "All-Steel",
  "Concrete Over Steel", "Concrete-Steel Composite",
  "Steel Over Concrete", "Composite", "Unknown"
)

# Combine data across decades
all_material_data <- map_df(decades, function(year) {
  df_name <- paste0("tallest_world_", year)
  if (exists(df_name)) {
    df <- get(df_name)
    if (all(c("Material", "Height (m)") %in% names(df))) {
      df <- df %>%
        mutate(
          Decade = year,
          Material = standardize_material(Material)
        ) %>%
        filter(Material %in% target_materials) %>%
        select(Name, `Height (m)`, Material, Decade)
      return(df)
    }
  }
  return(NULL)
})

# Reorder materials by median height
material_order <- all_material_data %>%
  group_by(Material) %>%
  summarise(median_height = median(`Height (m)`)) %>%
  arrange(desc(median_height)) %>%
  pull(Material)

all_material_data <- all_material_data %>%
  mutate(Material = factor(Material, levels = material_order))

# Plot
p <- ggplot(all_material_data, aes(x = Material, y = `Height (m)`)) +
  geom_violin(fill = "#0072BD", color = "black", alpha = 0.8) +
  stat_summary(fun = "median", geom = "point", color = "white", size = 2) +
  theme_minimal() +
  coord_flip() +
  labs(
    title = "Height Distribution by Material (All Decades)",
    subtitle = "Ordered by median height of buildings using each material",
    x = "Material",
    y = "Height (m)"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

print(p)

ggsave(
  filename = here("graphs", "material_height_distribution_violin.png"),
  plot = p,
  width = 10,
  height = 6
)

###### Material vs. Height Distribution Across All Decades ######


















###### Floors vs. Height – Equal Axis Scaling + Regression Equation ######




###### Floors vs. Height – Unique Buildings from 1890–2025 with Equation ######

# Prepare unique buildings data
all_buildings_df <- map_df(decades, function(year) {
  df_name <- paste0("tallest_world_", year)
  if (exists(df_name)) {
    df <- get(df_name)
    df <- df %>%
      mutate(Decade = year) %>%
      select(Name, `Height (m)`, Floors, Decade)
    return(df)
  }
  return(NULL)
})

unique_buildings <- all_buildings_df %>%
  distinct(Name, .keep_all = TRUE) %>%
  filter(!is.na(`Height (m)`), !is.na(Floors))

# Plot
p <- ggplot(unique_buildings, aes(x = Floors, y = `Height (m)`)) +
  geom_point(alpha = 0.6, color = "#0072BD", size = 2) +
  geom_smooth(method = "lm", color = "darkred", se = FALSE, linetype = "dashed") +
  ggpmisc::stat_poly_eq(
    formula = y ~ x,
    aes(label = paste("y =", ..eq.label..)),
    parse = TRUE,
    label.x = "right",
    label.y = "top",
    size = 4,
    color = "black"
  ) +
  theme_minimal() +
  labs(
    title = "Floors vs. Height (Unique Buildings, 1890–2025)",
    subtitle = "Each dot is a unique building; equation shows linear fit across all decades",
    x = "Number of Floors",
    y = "Height (m)"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

print(p)

ggsave(
  filename = here("graphs", "floors_vs_height_with_equation.png"),
  plot = p,
  width = 10,
  height = 6
)






###### COMPLETION YEAR #########
###### Completion Year Distribution (All Unique Tallest Buildings) ######

# Combine data across all decades
all_buildings_df <- map_df(decades, function(year) {
  df_name <- paste0("tallest_world_", year)
  if (exists(df_name)) {
    df <- get(df_name)
    df <- df %>%
      mutate(Decade = year) %>%
      select(Name, `Completion Year`, `Height (m)`, Decade)
    return(df)
  }
  return(NULL)
})

# Remove duplicate buildings by name
unique_buildings <- all_buildings_df %>%
  distinct(Name, .keep_all = TRUE) %>%
  filter(!is.na(`Completion Year`), is.numeric(`Completion Year`))

# Plot histogram
p <- ggplot(unique_buildings, aes(x = `Completion Year`)) +
  geom_histogram(binwidth = 5, fill = "#0072BD", color = "white", boundary = 0) +
  theme_minimal() +
  labs(
    title = "Completion Year Distribution of the Tallest Buildings (1890–2025)",
    subtitle = "Each bar counts unique buildings among the 100 tallest in any decade",
    x = "Completion Year",
    y = "Number of Unique Buildings"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

print(p)

ggsave(
  filename = here("graphs", "completion_year_distribution.png"),
  plot = p,
  width = 10,
  height = 6
)

###### COMPLETION YEAR #########





