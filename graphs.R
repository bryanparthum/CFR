library(purrr)
library(ggplot2)
library(ggthemes)
library(reshape2)

years <- as.character(seq(1998, 2018))

read_one_year_processed_xml <- function(year) {
  
  filename <- file.path(".", "data", "processed_xml_bottom_up", paste0("processed_xml_", year, ".Rda"))
  
  # I have a note to rename the files as RDs so I can import them in w/ diff. names
  # all xml files should load as a df named year_xml
  load(filename) 
  
  return(year_xml)
  
}

# Start the clock!
ptm <- proc.time()

all_sections_processed <- map_dfr(years, read_one_year_processed_xml)

# Stop the clock
proc.time() - ptm

all_sections_incorporation <- all_sections_processed %>%
  mutate(incorporation = str_detect(section_subject, "Incorporation")*1,
         CFR_cite = str_count(paragraph_text, "CFR")*1) %>%
  group_by(CFR_year) %>%
  summarize(all_rows = n()/100 , 
            all_incorporations = sum(incorporation, na.rm = TRUE),
            all_CFRs = sum(CFR_cite, na.rm = TRUE)) %>%
  ungroup() 


all_sections_graph <- melt(all_sections_incorporation, id.vars = "CFR_year") %>%
  mutate(CFR_year = as.integer(CFR_year)) %>%
  filter(CFR_year > 2001)


ggplot(data = subset(all_sections_graph, variable == "all_incorporations"),
                     aes(x = CFR_year, y = value)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2002, 2018, by =2)) +
  labs( y = "Count", x = "CFR year", title = "Growth in Incorporations by Reference, EPA CFR") +
  theme_minimal()

ggsave("incorp by reference.png")

ggplot(data = subset(all_sections_graph, variable == "all_CFRs"),
       aes(x = CFR_year, y = value)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2002, 2018, by =2)) +
  labs( y = "Count", x = "CFR year", title = "Growth in CFR References, EPA CFR") +
  theme_minimal()

ggsave("use of CFR.png")
