setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library('tidyverse')

files <- list.files(path = 'raw_data', pattern = '*.csv', full.names = TRUE)

data <- list()

region_name <- c('Alto Paraguay', 'Alto Paraná', 'Amambay', 'Asunción (Distrito Capital)', 'Boquerón', 'Caagrazú', 'Caazapá', 'Canindeyú', 'Central', 'Concepción', 'Cordillera', 'Guairá', 'Itapúa', 'Misiones', 'Ñeembucú', 'Paraguarí', 'Presidente Hayes', 'San Pedro')
abbrev_name <- sprintf(fmt = 'R%02d', 1:length(region_name))

week_name <- function (X) {
  sapply(X = X, FUN = function (x) {
      if (nchar(x) == 2) as.numeric(substr(x, 2, 2)) else as.numeric(substr(x, 2, 3))
    })
}

for (i in 1:length(files)) {

  temp_data <- read_csv(file = files[i])
  
  temp_data <- temp_data %>% 
                 arrange(admin1) %>% 
                 rename(Region = admin1, Year = year) %>% rename_with(.fn = week_name, .cols = starts_with(match = 'w')) %>% 
                 mutate(Region = region_name) %>% 
                 add_column(Abbrev_name = abbrev_name) %>%
                 select(Region, Abbrev_name, Year, num_range(prefix = '', range = 1:52)) %>% 
                 pivot_longer(cols = 4:55, names_to = 'Week', values_to = 'Count') %>% 
                 mutate(Count = replace_na(Count, 0)) %>% # It only occurs at the last week of 2012
                 mutate(Week = as.numeric(Week))
                 
  data[[i]] <- temp_data
  
}

week_seq <- function (X, t, r) {
  t <- (1:t) - 1
  X <- X + (rep(x = t, each = (r * 52)) * 52)
  X
}

data <- bind_rows(data) %>% 
          add_column(Index = 1:(52 * length(files) * length(region_name)), .before = 1) %>% 
          mutate(Week_seq = week_seq(X = Week, t = length(files), r = length(region_name)), .after = Week)

write_csv(x = data, file = 'paraguay.csv')
