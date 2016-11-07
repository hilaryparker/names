
```{r}
library(dplyr)
library(ggplot2)
library(readr)

babynames <- read_csv('https://cdn.rawgit.com/wharton-data-analytics/babynames/3192856738dee4a91bfc7d320355daa5ae428c17/data-raw/csv/babynames.csv')
babynames %>% summarize(max(year))
```


```{r}
poisoned_names <- babynames %>% 
  filter(sex == "F") %>% 
  filter(n >= 115) %>% 
  arrange(name, year) %>% 
  group_by(name) %>% 
  mutate(years = n()) %>% 
  mutate(yeardiff = c(NA, diff(year)),
         YoY_increase = 100* prop / lag(prop, 1),
         year_before = lag(year, 1), prop_before = lag(prop, 1)) %>% 
  ungroup() %>% 
  mutate(YoY_decrease = 100-YoY_increase) %>% 
  filter(!is.na(YoY_increase), yeardiff == 1) %>% 
  arrange(YoY_increase) %>% 
  top_n(30) %>% 
  select(name, year, prop, YoY_decrease, year_before, prop_before)
```

```{r}
babynames %>% 
  filter(sex == "F" & year > 1950) %>% 
  inner_join(poisoned_names %>% select(name), by = "name") %>% 
  ggplot(aes(x = year, y = prop, color = name)) +
  geom_line() +
  geom_text_repel(aes(x = year_before, y = prop_before, label = name), data = poisoned_names) +
  theme_bw() 
```

```{r}
babynames %>% 
  filter(sex == "F" & year > 1950 & n >= 200 & !(name %in% c("Marian", "Ashanti", "Christin", "Litzy"))) %>% 
  inner_join(poisoned_names %>% select(name), by = "name") %>% 
  group_by(name) %>% 
  mutate(years = n()) %>% 
  filter(years > 18) %>% 
  ggplot(aes(x = year, y = prop, color = name)) +
  geom_line() +
  geom_text_repel(aes(x = year_before, y = prop_before, label = paste0(name, ", ", round(YoY_decrease), "%")), data = poisoned_names %>% filter(name %in% c("Hilary", "Hillary", "Isis"))) +
  theme_bw() 
```


