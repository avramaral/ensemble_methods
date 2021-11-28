setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library('geobr')
library('tidyverse')

brazil <- read_country(year = 2020, simplified = TRUE)

pop <- read_csv('brazil_pop.csv')
pop$pop_2014_frac <- pop$pop_2014 / sum(pop$pop_2014)
pop$pop_2015_frac <- pop$pop_2015 / sum(pop$pop_2015)
pop$pop_2016_frac <- pop$pop_2016 / sum(pop$pop_2016)
pop$pop_2017_frac <- pop$pop_2017 / sum(pop$pop_2017)
pop$pop_2018_frac <- pop$pop_2018 / sum(pop$pop_2018)
pop$pop_2019_frac <- pop$pop_2019 / sum(pop$pop_2019)
pop$pop_2020_frac <- pop$pop_2020 / sum(pop$pop_2020)

brazil <- left_join(x = brazil, y = pop, by = c('abbrev_state' = 'region'))

brazil <- brazil %>% select(abbrev_state, 
                            pop_2014,
                            pop_2015,
                            pop_2016,
                            pop_2017,
                            pop_2018,
                            pop_2019,
                            pop_2020,
                            pop_2014,
                            pop_2014_frac,
                            pop_2015_frac,
                            pop_2016_frac,
                            pop_2017_frac,
                            pop_2018_frac,
                            pop_2019_frac,
                            pop_2020_frac,
                            geom) %>% rename(region = abbrev_state)

saveRDS(object = brazil, file = 'brazil.rds')
