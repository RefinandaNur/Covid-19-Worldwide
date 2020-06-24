# As this an ongoing situation, frequent changes in the data format may occur, please visit the package news to get updates about those changes
install.packages("coronavirus")

# library(digest)
# Install the Github version (refreshed on a daily bases):
# install.packages("devtools")
devtools::install_github("RamiKrispin/coronavirus")

library(coronavirus)
update_dataset()
data("coronavirus")

# Summary of the total confrimed cases by country (top 20):
library(dplyr)

glimpse(coronavirus)
summary(coronavirus)
tail(coronavirus)

# case in world wide
library(tidyr)
cases_worldwide <- coronavirus  %>%
  group_by ( date, type) %>% 
  summarise (cases = sum(cases)) 

# pivot summary case world wide
summaryall <- cases_worldwide %>% filter(date >= '2020-01-22') %>%
              pivot_wider (names_from = type, values_from = cases)%>%
              ungroup() %>%
              mutate(active = confirmed - death,
                  cum_active = cumsum(active),
                  cum_confirm = cumsum(confirmed),
                  cum_death = cumsum(death),
                  cum_recovered = cumsum(recovered))
dfw <- as.data.frame(summaryall)

# Persentase ratio dari death and recovery
ratio <- dfw %>%
  group_by(date) %>%
  summarise(death = sum(cum_death), confirmed = sum(cum_confirm),recovered = sum(cum_recovered)) %>%
  mutate(recov_rate = 100*(recovered/confirmed))%>%
  mutate(death_rate = 100*(death/confirmed))

summaryratio <- summary(ratio) 

ggplot(ratio) +
  geom_line(aes(x=date, y= death_rate, color = 'Death Ratio')) +
  geom_line(aes(x=date, y= recov_rate, color = 'Recovery Ratio')) +
  labs(x = "", y = 'Rate', title = 'Ratio of Death and Recovered',
       subtitle = 'Worldwide') 

# plot kumulatif case world wide
library(ggplot2)
ggplot(summaryall) + 
  geom_line(aes(x=date, y=cum_confirm, color = cum_confirm)) + 
  ylab("Confirmed cases")+
  xlab("Date")

# line from type covid 19
library(plotly)
dfw %>%
  plot_ly( x = ~date, y= ~cum_confirm,
           name = 'Confirm', type= 'scatter',
           mode = 'lines+markers') %>%
  add_trace(y = ~cum_recovered, name = 'Recovered',fillcolor = '#E41317') %>%
  add_trace(y = ~cum_death, name = 'Death',fillcolor = 'forestgreen')%>%
  add_trace(y = ~cum_active, name = 'Active cases',fillcolor = ' ')%>%
  layout ( title = "Total Cumulatif Cases Covid 19 World Wide",
           legend = list (x = 0.1, y = 0.9),
           yaxis = list(title = "Total"),
           xaxis = list(title = "Source: Johns Hopkins University Center for Systems Science and Engineering"))%>%
  add_annotations(x = ('2020-06-05'), y = '6734088',
                text = paste('6.734.088 confirm cases'), 
                xref = 'x',
                yref = 'y',
                arrowhead = 1,
                arrowhead = 1,
                arrowsize = 1,
                showarrow = TRUE,
                ax = -0.5,
                ay =-20) %>%
  add_annotations(x = ('2020-06-05'), y = '6339213',
                  text = paste('6.339.213 active cases'), 
                  xref = 'x',
                  yref = 'y',
                  arrowhead = 1,
                  arrowhead = 1,
                  arrowsize = 1,
                  showarrow = TRUE,
                  ax = -0.5,
                  ay = 70) %>%
  add_annotations(x = ('2020-06-05'), y = '394875',
                  text = paste('394.875 death cases'), 
                  xref = 'x',
                  yref = 'y',
                  arrowhead = 1,
                  arrowhead = 1,
                  arrowsize = 1,
                  showarrow = TRUE,
                  ax = -0.5,
                  ay =-20) %>%
  add_annotations(x = ('2020-06-05'), y = '2846192',
                  text = paste('2.746.192 recovery cases'), 
                  xref = 'x',
                  yref = 'y',
                  arrowhead = 1,
                  arrowhead = 1,
                  arrowsize = 1,
                  showarrow = TRUE,
                  ax = -0.5,
                  ay =-20)

# Treemaps
library(ggplot2)
library(plotly)
confirmed_cases_worldwide <- coronavirus %>% filter ( type == 'confirmed') %>%
                group_by ( country) %>% 
                summarise (cum_cases = sum(cases)) %>%
                arrange (-cum_cases) %>%
                mutate(parents = "Confirmed") %>%
                ungroup() %>%
                plot_ly(type= "treemap",
                 values = ~cum_cases,
                 labels= ~ country,
                 parents=  ~parents,
                 domain = list(column=0),
                 name = "Country Confirmed Covid 19",
                 textinfo="label+value+percent parent")
      

recovered_cases_worldwide <- coronavirus %>% filter ( type == 'recovered') %>%
  group_by ( country) %>% 
  summarise (cum_cases = sum(cases)) %>%
  arrange (-cum_cases) %>%
  mutate(parents = "Recovered") %>%
  ungroup() %>%
  plot_ly(type= "treemap",
          values = ~cum_cases,
          labels= ~ country,
          parents=  ~parents,
          domain = list(column=0),
          name = "Country Recovered Covid 19",
          textinfo="label+value+percent parent")

# Case anoter country
library(tidyr)
pivot_cases_by_country <- coronavirus %>%
                          filter(date >= 2020-04-01, type== 'confirmed') %>%
                          group_by (country, date) %>%
                          summarise (total = sum(cases)) %>%
                          pivot_wider (names_from = country,
                                       values_from = total)

pivot_cases_by_country %>%
  plot_ly( x = ~date, y= ~Indonesia,
           name = 'Indonesia', type= 'scatter',
           mode = 'lines+markers') %>%
  add_trace(y= ~Singapore,
            name = 'Singapore', type= 'scatter',
            mode = 'lines+markers') %>%
  add_trace(y= ~Vietnam,
            name = 'Vietnam', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~Thailand,
            name = 'Thailand', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~Malaysia,
            name = 'Malaysia', type= 'scatter',
            mode = 'lines+markers')%>%
  layout ( title = "Kasus Terkonfirmasi",
           legend = list (x = 0.1, y = 1),
           yaxis = list(title = "Kasus positif baru"),
           xaxis = list(title = "Tanggal"))
