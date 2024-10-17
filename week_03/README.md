library(ggplot2)
library(dplyr)
library(gapminder)

gapminder
str(gapminder)

NoK_gapminder <- gapminder %>% 
  filter(country != "Kuwait")

#first plot
ggplot(NoK_gapminder, aes(x=lifeExp, y=gdpPercap, color = continent, size = pop/100000)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~year, nrow = 1) +
  scale_y_continuous(trans = "sqrt") +
  labs(title = "Relationship")

#prepare data for second plot
gapminder_continent <- NoK_gapminder %>% 
  group_by(continent, year) 

gapminder_weighted <- gapminder_continent %>%  
  summarize(gdpPercapweighted = weighted.mean(x = gdpPercap, w = pop),
  pop = sum(as.numeric(pop)))

#second plot

ggplot() +
  geom_point(data = gapminder_continent, aes(x=year, y=gdpPercap, color = continent, size = pop/100000, group = country)) +
  geom_line(data = gapminder_continent, aes(x=year, y=gdpPercap, color = continent, group = country)) +
  geom_line(data = gapminder_weighted, aes(x=year, y=gdpPercapweighted), color = "black") +
  geom_point(data = gapminder_weighted, aes(x=year, y=gdpPercapweighted, size = pop/100000), color = "black") +
  facet_wrap(~continent, nrow = 1) +
  theme_bw() +
  labs(title = "Wealth and Life Expectancy Through Time", x = "Year", y = "GDP Per Capita")

