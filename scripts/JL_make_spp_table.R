# Script to find species that are shared between datasets
# create one master list of all species with where they are found



library(tidyverse)
library(here)


# Step 1. Upload Data - INT first
#--------------------------------------------------------------
int_counts <- read.csv(here("SEED_data","intertidal_data","counts_data.csv")) %>% janitor::clean_names()
int_cover <- read.csv(here("SEED_data","intertidal_data","percent_cover_data.csv")) %>% janitor::clean_names()
int_sizes <- read.csv(here("SEED_data","intertidal_data","sizes_data.csv")) %>% janitor::clean_names()
int_spp <- read.csv(here("SEED_data","intertidal_data","species_list.csv")) %>% janitor::clean_names()

# Get Int Counts Species --------------------------------------------------
int_spp %>% glimpse()

# start with full data
int_counts_spp <- int_counts %>% 
  
  # rename column to name for consistency
  rename(name = organism) %>%
  
  # remove parenthetical outliers (e.g. canopy, cover, egg case)
  separate(col = name, into =c("name","type"), sep=" \\(") %>% 
  mutate(part = gsub(")","",type)) %>%
  # remove parenthetical specificity for now
  select(-type) %>% 
  
  # add presence / absence column binary
  mutate(present = case_when(count == "p" | count > 0  ~ 1,
                             TRUE ~ 0)) %>%
  
  # now pull only spp that were present at some time
  filter(present ==1) %>%
  
  # pull names column 
  distinct(name) %>%
  arrange(name) %>%
  mutate(dataset = "int_counts")

  



# Get Int Cover Species ---------------------------------------------------
int_cover_spp <- int_cover %>% 
  
  # rename column to name for consistency
  rename(name = organism) %>%
  
  # remove parenthetical outliers (e.g. canopy, cover, egg case)
  separate(col = name, into =c("name","type"), sep=" \\(") %>% 
  mutate(type = gsub(")","",type)) %>%
  # remove parenthetical specificity for now
  select(-type) %>% 
  
  # many percent_cover values have notes in them. 
  # need to separate those from the numbers so like "75 phymato" becomes 75
  drop_na(percent_cover) %>%
  mutate(percent_cover_numeric = as.numeric(str_extract(percent_cover, "\\-*\\d+\\.*\\d*"))) %>%


  # add presence / absence column binary - when cover is > 0 or marked as "present". 
  mutate(present = case_when(percent_cover_numeric > 0  | percent_cover == "p" ~ 1,
                             TRUE ~ 0))  %>%
  
  # now pull only spp that were present at some time
  filter(present ==1) %>%
  
  # pull names column 
  distinct(name) %>%
  arrange(name) %>%
  mutate(dataset = "int_cover")

int_cover_spp




# Get Int Sizes Species ---------------------------------------------------

int_sizes_spp <- int_sizes %>% 
  
  # rename column to name for consistency
  rename(name = organism) %>%
  
  # filter spp with no data
  drop_na(count) %>%
  
  # get spp
  distinct(name) %>%
  mutate(dataset = "int_sizes")





#----


# Step 2. Upload Data - KEEN first
#--------------------------------------------------------------
keen_cover <- read.csv(here("tidy_data","keen_cover.csv")) %>% janitor::clean_names()
keen_quads <- read.csv(here("tidy_data","keen_quads.csv")) %>% janitor::clean_names()
keen_swath <- read.csv(here("tidy_data","keen_swath.csv")) %>% janitor::clean_names()
keen_kelp  <- read.csv(here("tidy_data","keen_kelp.csv")) %>% janitor::clean_names()
keen_fish  <- read.csv(here("tidy_data","keen_fish.csv")) %>% janitor::clean_names()




# Get keen Cover Species --------------------------------------------------
keen_cover %>% glimpse()

# start with full data
keen_cover_spp <- keen_cover %>% 
  
  # rename column to name for consistency
  rename(name = species) %>%
  
  # fix one specific spp
  mutate(name = recode(name,
                       "Halichondria (Halichondria) panicea" = "Halichondria panicea")) %>%
  
  # pick those where % cover > 0
  mutate(percent_cover = as.numeric(percent_cover)) %>%
  filter(percent_cover > 0) %>%
  
  # pull names column 
  distinct(name) %>%
  arrange(name) %>%
  filter(!is.na(name)) %>%
  mutate(dataset = "keen_cover")





# Get Keen Quads Species ---------------------------------------------------
keen_quads_spp <- keen_quads %>% 
  
  # rename column to name for consistency
  rename(name = species) %>%
  
  # find where count > 0 
  mutate(count = as.numeric(count)) %>%

  # add presence / absence column binary - when count is > 0 or marked as "present". 
  mutate(present = case_when(count > 0   ~ 1,
                             TRUE ~ 0))  %>%
  
  # now pull only spp that were present at some time
  filter(present ==1) %>%
  
  # pull names column 
  distinct(name) %>%
  arrange(name) %>%
  drop_na(name)%>%
  mutate(dataset = "keen_quads")





# Get Keen Swath  Species ---------------------------------------------------

keen_swath_spp <- keen_swath %>% 
  
  # rename column to name for consistency
  rename(name = species) %>%
  drop_na(name) %>% 

  # filter spp with no data
  drop_na(count) %>%
  
  # find where count > 0 
  mutate(count = as.numeric(count)) %>%
  
  # add presence / absence column binary - when count is > 0 or marked as "present". 
  mutate(present = case_when(count > 0   ~ 1,
                             TRUE ~ 0))  %>%
  
  # now pull only spp that were present at some time
  filter(present ==1) %>%
  
  # pull names column 
  distinct(name) %>%
  arrange(name) %>%
  mutate(dataset = "keen_swath")





# Kelp kelp spp --------------------------------------------------
keen_kelp_spp <- keen_kelp %>% 
  
  # rename column to name for consistency
  rename(name = species) %>%
  drop_na(name) %>% 
  
  # filter spp with no data
  drop_na(blade_length_cm)  %>%
  
  # pull names column 
  distinct(name) %>%
  arrange(name) %>%
  mutate(dataset = "keen_kelp")


# Keen fish spp ----------------------------------------------------------
keen_fish_spp <- keen_fish %>%
  
  # rename column to name for consistency
  rename(name = species) %>%
  drop_na(name) %>% 
  
  # filter spp with no data
  drop_na(count) %>%
  
  # add presence / absence column binary - when count is > 0 or marked as "present". 
  mutate(present = case_when(count > 0   ~ 1,
                             TRUE ~ 0))  %>%
  
  # filter to only present spp
  filter(present ==1) %>%
  
  # pull names column 
  distinct(name) %>%
  arrange(name) %>%
  mutate(dataset = "keen_fish")
  
  



# merge species lists from all datasets -----------------------------------
full_spplist <- 
  rbind(int_counts_spp,
        int_cover_spp,
        int_sizes_spp,
        keen_cover_spp,
        keen_quads_spp,
        keen_swath_spp,
        keen_kelp_spp,
        keen_fish_spp)


full_spplist %>% 
  mutate(present = 1) %>%
  pivot_wider(names_from = dataset,
              values_from = present,
              values_fill = 0 ) %>%
  mutate(sum = rowSums(across(where(is.numeric)))) %>%
  arrange(desc(sum)) %>% view()


# make plot ============================================================
p <- full_spplist %>% 
  mutate(present = 1) %>%
  pivot_wider(names_from = dataset,
              values_from = present,
              values_fill = 0 ) %>%
  mutate(sum = rowSums(across(where(is.numeric))),
         sum_int = rowSums(across(contains("int"))),
         sum_keen = rowSums(across(contains("keen")))) %>%
  pivot_longer(cols = c(2:9),names_to = "dataset",values_to = "present") %>%
  group_by(name) %>%
  arrange(desc(sum)) %>%
  filter(sum_int > 0 & sum_keen > 0) %>%
  mutate(label = case_when(dataset == "int_counts" ~ "Intertidal\nCounts",
                           dataset == 'int_cover' ~ "Intertidal\n% Cover",
                           dataset == "int_sizes" ~ "Intertidal\nSizes",
                           dataset == "keen_cover" ~ "KEEN\n% Cover",
                           dataset == "keen_quads" ~ "KEEN\nQuads",
                           dataset == "keen_swath" ~ "KEEN\nSwath",
                           dataset == "keen_kelp" ~ "KEEN\nKelp",
                           dataset == "keen_fish" ~ "KEEN\nFish")) %>%
  # plot it as a table
  ggplot(aes(x=label,y=reorder(name,sum),fill=as.character(present))) +
  geom_tile(show.legend = F,color="grey20",size=.25) +
  scale_fill_manual(values = c("transparent","lightblue"))+
  labs(x=NULL,y=NULL,
       title = "Species present in both intertidal and subtidal datasets") + 
  ggthemes::theme_few() +
  geom_vline(xintercept = 3.5,color="grey20",size=1)+
  scale_x_discrete(position = "top")+
  coord_cartesian(xlim=c(.5,8.5),expand = F,clip = "off") +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(family="Open Sans"),
        axis.text.x=element_text(family="Open Sans Semibold"),
        plot.title = element_text(family="Open Sans",hjust=0,margin=margin(b=5), size=18),
        plot.title.position = "plot"
        )+
  annotate(geom="segment",
           x=9.1,xend=9.1,
           y=2,yend=33,
           arrow = arrow(length = unit(2, "mm"),
                         ends = "first")
           )+
  annotate(geom="text",
           x=9.1,
           y=1.75,
           label = "Few\nDatasets",
           lineheight=.75,
           hjust=0.5,
           vjust=1,
           family="Open Sans"
  )+
  annotate(geom="text",
           x=9.1,
           y=33.25,
           label = "Many\nDatasets",
           lineheight=.75,
           hjust=0.5,
           vjust=0,
           family = "Open Sans"
  )+
  theme(plot.margin = margin(t=5,r=24,b=5,l=5,unit = "mm")) 

p
ggsave(filename = here("outputs","species_in_surveys.png"),
       height = 10,
       width = 9,
       dpi=200)


#===========================================================================
#===========================================================================
# Now repeat with thresholds - only species present in >25% of years
#===========================================================================
#===========================================================================


# Get Int Counts Species with Threshold --------------------------------------------------
int_spp %>% glimpse()
range(int_counts$year)

# ok so this dataset goes from 1982-2017. 36 years in total. 
# so first I will filter for species that show up in >=9 years, 
# or 25% of the possible years.

int_counts_spp_threshold <- 
  # start with full data
  int_counts %>% 
  
  # rename column to name for consistency
  rename(name = organism) %>%
  
  # remove parenthetical outliers (e.g. canopy, cover, egg case)
  separate(col = name, into =c("name","type"), sep=" \\(") %>% 
  mutate(part = gsub(")","",type)) %>%
  # remove parenthetical specificity for now
  select(-type) %>% 
  
  # add presence / absence column binary
  mutate(present = case_when(count == "p" | count > 0  ~ 1,
                             TRUE ~ 0)) %>%
  
  # now pull only spp that were present at some time
  filter(present ==1) %>%
  
  # collapse into species x year
  nest_by(name,year)  %>%
  
  # count number of years that spp is present
  group_by(name) %>%
  add_count(name = "num_years") %>%
  
  # filter spp that are present in 9+ years
  filter(num_years >= 9) %>%
  
  # unnest
  unnest(data) %>%

  # pull names column 
  distinct(name,num_years) %>%
  arrange(desc(num_years)) %>%
  mutate(dataset = "int_counts")
# ok, so this massively decreased number of species present, from 74 to 31. 





# Get Int Cover Species With Threshold---------------------------------------------------
range(int_cover$year)
# ok so this dataset goes from 1982-2017. 36 years in total. 
# so first I will filter for species that show up in >=9 years, 
# or 25% of the possible years.

int_cover_spp_threshold <-
int_cover %>% 
  
  # rename column to name for consistency
  rename(name = organism) %>%
  
  # remove parenthetical outliers (e.g. canopy, cover, egg case)
  separate(col = name, into =c("name","type"), sep=" \\(") %>% 
  mutate(type = gsub(")","",type)) %>%
  # remove parenthetical specificity for now
  select(-type) %>% 
  
  # many percent_cover values have notes in them. 
  # need to separate those from the numbers so like "75 phymato" becomes 75
  drop_na(percent_cover) %>%
  mutate(percent_cover_numeric = as.numeric(str_extract(percent_cover, "\\-*\\d+\\.*\\d*"))) %>%
  
  
  # add presence / absence column binary - when cover is > 0 or marked as "present". 
  mutate(present = case_when(percent_cover_numeric > 0  | percent_cover == "p" ~ 1,
                             TRUE ~ 0))  %>%
  
  # now pull only spp that were present at some time
  filter(present ==1) %>%
  
  # collapse into species x year
  nest_by(name,year)  %>%
  
  # count number of years that spp is present
  group_by(name) %>%
  add_count(name = "num_years") %>%
  
  # filter spp that are present in 9+ years
  filter(num_years >= 9) %>%
  
  # unnest
  unnest(data) %>%
  
  # pull names column 
  distinct(name,num_years) %>%
  arrange(desc(num_years)) %>%
  mutate(dataset = "int_cover")

nrow(int_cover_spp)
nrow(int_cover_spp_threshold)
# ok, here we decreased from 92 to 42




# Get Int Sizes Species with Threshold---------------------------------------------------
int_sizes_spp_threshold <-
  int_sizes%>% 
  
  # rename column to name for consistency
  rename(name = organism) %>%
  
  # filter spp with no data
  drop_na(count) %>%
  
  # collapse into species x year
  nest_by(name,year)  %>%
  
  # count number of years that spp is present
  group_by(name) %>%
  add_count(name = "num_years") %>%

  # filter spp that are present in 9+ years
  filter(num_years >= 9) %>%
  
  # unnest
  unnest(data) %>%
  
  # pull names column 
  distinct(name,num_years) %>%
  arrange(desc(num_years)) %>%
  mutate(dataset = "int_sizes")

nrow(int_sizes_spp_threshold)
nrow(int_sizes_spp)
# ok, this one didnt change (both have 4 spp)





#----

# Make Intertidal common species table
int_spp_thresholds_total <-
  rbind(int_counts_spp_threshold,
        int_cover_spp_threshold,
        int_sizes_spp_threshold)


pal <- wesanderson::wes_palette("Zissou1")

p2 <- 
int_spp_thresholds_total %>% 
  pivot_wider(names_from = dataset,
              values_from = num_years,
              values_fill = NA ) %>%
  mutate(sum = rowSums(across(where(is.numeric)),na.rm = T)) %>%
  pivot_longer(cols = c(2:4),names_to = "dataset",values_to = "num_years") %>%
  group_by(name) %>%
  arrange(desc(sum)) %>%
  mutate(label = case_when(dataset == "int_counts" ~ "Intertidal\nCounts",
                           dataset == 'int_cover' ~ "Intertidal\n% Cover",
                           dataset == "int_sizes" ~ "Intertidal\nSizes")) %>%
  # plot it as a table
  ggplot(aes(x=label,y=reorder(name,sum),fill=num_years)) +
  geom_tile(color="grey20",size=.25) +
  scale_fill_gradientn(colours=pal,na.value = "transparent")+
  labs(x=NULL,y=NULL,
       title = "Intertidal species in 25% or more of years sampled",
       fill = "Years Present\nin Dataset") + 
  ggthemes::theme_few() +
  scale_x_discrete(position = "top")+
  coord_cartesian(xlim=c(.5,3.5),expand = F,clip = "off") +
  theme(legend.title = element_text(family = "Open Sans Semibold",
                                    size=10,
                                    margin = margin(b=5)),
        legend.margin = margin(l=40),
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(family="Open Sans"),
        axis.text.x=element_text(family="Open Sans Semibold"),
        plot.title = element_text(family="Open Sans",hjust=0.5,margin=margin(b=5), size=18),
        plot.title.position = "plot"
  ) +
  guides(fill=guide_colorbar(barwidth = unit(.2,"in"),
                              barheight = unit(7.5,"in"),
                              ticks.colour = "black",
                              frame.colour = "black",
                              title.position = "top"))+
  annotate(geom="segment",
           x=3.9,xend=3.9,
           y=3,yend=nrow(int_spp_thresholds_total %>% distinct(name))-1.5,
           arrow = arrow(length = unit(2, "mm"),
                         ends = "first")
  )+
  annotate(geom="text",
           x=3.9,
           y=2.75,
           label = "Present in\nfewer years",
           lineheight=.75,
           hjust=0.5,
           vjust=1,
           family="Open Sans",
           size=3.25
  )+
  annotate(geom="text",
           x=3.9,
           y=nrow(int_spp_thresholds_total %>% distinct(name))-1,
           label = "Present in\nmany years",
           lineheight=.75,
           hjust=0.5,
           vjust=0,
           family = "Open Sans",
           size=3.25
  )+
  theme(plot.margin = margin(t=5,r=5,b=5,l=5,unit = "mm")) 

ggsave(p2,
       filename = here("outputs","species_in_intertidal.png"),
       height = 10,
       width = 7.5,
       dpi=200)



int_counts %>% distinct(transect) %>% arrange(transect)


int_spp_thresholds_total %>% 
  pivot_wider(names_from = dataset,
              values_from = num_years,
              values_fill = NA ) %>%
  mutate(sum = rowSums(across(where(is.numeric)),na.rm = T)) %>%
  arrange(desc(sum)) %>%
  select(-sum) %>%
  write.csv(file=here("outputs","top_intertidal_spp.csv"),
            row.names = F)
?write.csv
