library(rio) 
library(tidyr)
library(dplyr)
library(nlme)
library(MASS)
library(ggplot2)
library(scales)
library(car)
library(AER)
library(mice)
library(naniar)
library(flextable)
library(officer)
getwd()

# Import the aggregated dataset.
# Dataset includes item level RAMP data
# merged with citation info from Crossref
# and OA availability data from Unpaywall.
dat <- import("../data/ramp_crossref_unpaywall_merged.csv")

# Drop rows where ir_is_oa_loc == TRUE but ct_ir_oa_copies == 0 (102 rows).
# Note the filter drops rows where count_error is NA or FALSE,
# so more than just the 102 rows are dropped.
dat$count_error <- (dat$ir_is_oa_loc == TRUE & dat$ct_ir_oa_copies == 0)
dat <- dat %>% filter(count_error != TRUE)

# Drop columns related to which IR hosts the item,
# and also the method used to extract the DOI from 
# item level metadata.
dat_adj <- dplyr::select(dat,
                         ir_is_oa_loc,
                         ir_pub_year,
                         cref_created_year,
                         doi,
                         ct_oa_copies,
                         ct_ir_oa_copies,
                         ct_ir_oa_copies,
                         ct_dr_oa_copies,
                         ct_pub_oa_copies,
                         ct_other_oa_copies,
                         item_uri_sum_clicks,
                         ct_citations)


# Drop rows where IR publication year
# is before 2017. This filters out items
# that had been available from an IR
# for less than 2 years before RAMP data were collected.
dat_adj <- dat_adj%>%
  filter(ir_pub_year < 2017)

# Limit items to those for which the IR year of
# publication is not more than 1 year from when the Crossref
# DOI was created.
dat_adj$ir_pub_year <- as.numeric(dat_adj$ir_pub_year)
dat_adj$cref_created_year <- as.numeric(dat_adj$cref_created_year)

dat_adj$pub_yr_diff <- dat_adj$ir_pub_year - dat_adj$cref_created_year
dat_adj_pub_yr <- dat_adj %>% filter(pub_yr_diff == 0 | pub_yr_diff == 1)


# Check for incomplete observations.
md.pattern(dat_adj_pub_yr, rotate.names = TRUE)


# Adjust citations by year.
# Average the Crossref DOI year of creation and IR year of upload/publication
# to create a single column to refer to for calculating years of availability.
dat_adj_pub_yr$avg_pub_year <- (dat_adj_pub_yr$cref_created_year + 
                                  dat_adj_pub_yr$ir_pub_year)/2

# Create a new column for the number of years an item has been available.
dat_adj_pub_yr$year <- ifelse(dat_adj_pub_yr$avg_pub_year < 2004, 16,
                              ifelse(dat_adj_pub_yr$avg_pub_year >= 2004 & dat_adj_pub_yr$avg_pub_year < 2005, 15,
                                     ifelse(dat_adj_pub_yr$avg_pub_year >= 2005 & dat_adj_pub_yr$avg_pub_year < 2006, 14,
                                            ifelse(dat_adj_pub_yr$avg_pub_year >= 2006 & dat_adj_pub_yr$avg_pub_year < 2007, 13,
                                                   ifelse(dat_adj_pub_yr$avg_pub_year >= 2007 & dat_adj_pub_yr$avg_pub_year < 2008, 12,
                                                          ifelse(dat_adj_pub_yr$avg_pub_year >= 2008 & dat_adj_pub_yr$avg_pub_year < 2009, 11,
                                                                 ifelse(dat_adj_pub_yr$avg_pub_year >= 2009 & dat_adj_pub_yr$avg_pub_year < 2010, 10,
                                                                        ifelse(dat_adj_pub_yr$avg_pub_year >= 2010 & dat_adj_pub_yr$avg_pub_year < 2011, 9,
                                                                               ifelse(dat_adj_pub_yr$avg_pub_year >= 2011 & dat_adj_pub_yr$avg_pub_year < 2012, 8,
                                                                                      ifelse(dat_adj_pub_yr$avg_pub_year >= 2012 & dat_adj_pub_yr$avg_pub_year < 2013, 7,
                                                                                             ifelse(dat_adj_pub_yr$avg_pub_year >= 2013 & dat_adj_pub_yr$avg_pub_year < 2014, 6,
                                                                                                    ifelse(dat_adj_pub_yr$avg_pub_year >= 2014 & dat_adj_pub_yr$avg_pub_year < 2015, 5,
                                                                                                           4))))))))))))

# Create a column for the adjusted number 
# of citations per year.
dat_adj_pub_yr$ct_citations_adj <- dat_adj_pub_yr$ct_citations/dat_adj_pub_yr$year

# Every item in the dataset has at least one OA copy hosted by an IR.
# Not every IR in the study was harvested by Unpwayall at time of data collection,
# so make an adjustment to add 1 to count of IR hosted OA copies
# and also add 1 to count of total OA copies for any row where the RAMP IR
# that hosts an item was not listed as an OA host by Unpaywall.
adj_dat <- dat_adj_pub_yr %>%
  mutate(ct_ir_oa_copies_adj = case_when(ir_is_oa_loc == FALSE ~ ct_ir_oa_copies + 1L,
                                         ir_is_oa_loc == TRUE ~ ct_ir_oa_copies + 0L),
         ct_oa_copies_adj = case_when(ir_is_oa_loc == FALSE ~ ct_oa_copies + 1L,
                                      ir_is_oa_loc == TRUE ~ ct_oa_copies + 0L))

#--Combine DOIs
# There are five DOIs with 2 IR hosts occurring in the remaining data.
# These are not true duplicates, as they are two distinct copies of an 
# item hosted by different IR. Their search engine performance data will
# be combined into a single observation for each DOI.
# View the DOIs with 2 hosts:
adj_dat %>%
  group_by(doi) %>% 
  summarise(count_dois = sum(!is.na(doi))) %>% 
  filter(count_dois > 1)


# Combine: get sum of all clicks from SERP,
# average other stats to avoid double counting citations, etc.
adj_dat_n <- adj_dat%>%
  group_by(doi)%>%
  summarize(click = sum(item_uri_sum_clicks, na.rm = TRUE),
            ir_c = mean(ct_ir_oa_copies, na.rm = TRUE),
            ir_c_adj = mean(ct_ir_oa_copies_adj, na.rm = TRUE),
            citation_c = mean(ct_citations, na.rm = TRUE),
            citation_c_adj = mean(ct_citations_adj, na.rm = TRUE),
            oa_c = mean(ct_oa_copies, na.rm = TRUE),
            oa_c_adj = mean(ct_oa_copies_adj, na.rm = TRUE),
            dr_c = mean(ct_dr_oa_copies, na.rm = TRUE),
            other_c = mean(ct_other_oa_copies, na.rm = TRUE),
            pub_c = mean(ct_pub_oa_copies, na.rm = TRUE))

#---Transform data for the ANCOVA analysis.
# Change click counts to categorical data
adj_dat_n$click_b <-ifelse(adj_dat_n$click<=3, "Median and below or 1-3 clicks",
                           "Above median")
adj_dat_n$click_b <- factor(adj_dat_n$click_b, levels = c("Median and below or 1-3 clicks",  "Above median"))
prop.table(table(adj_dat_n$click_b))

# Create categorical variable using adjusted count of total OA copies.
adj_dat_n$oa_c_adj_n <- ifelse(adj_dat_n$oa_c_adj>2, "Above median or 3 or more copies", "Median and below or 1-2 copies")
adj_dat_n$oa_c_adj_n <- factor(adj_dat_n$oa_c_adj_n, levels = c("Median and below or 1-2 copies", "Above median or 3 or more copies"))
prop.table(table(adj_dat_n$oa_c_adj_n))

# Create categorical variable using adjusted count of IR hosted copies.
adj_dat_n$ir_c_adj_c <- ifelse(adj_dat_n$ir_c_adj==1, "Median and below or 1 copy",
                               "Above median or more than 1 copy")
adj_dat_n$ir_c_adj_c <- factor(adj_dat_n$ir_c_adj_c, levels = c("Median and below or 1 copy",
                                                                "Above median or more than 1 copy"))
prop.table(table(adj_dat_n$ir_c_adj_c))

# Create binary variables based on availability of
# OA copies from disciplinary repositories.
adj_dat_n$dr_c_b <- ifelse(adj_dat_n$dr_c>0, "1", "0")
prop.table(table(adj_dat_n$dr_c_b))

# Create binary variables based on availability of
# OA copies from "other" OA host types.
adj_dat_n$other_c_b <- ifelse(adj_dat_n$other_c>0, "1", "0")
prop.table(table(adj_dat_n$other_c_b))


# Create binary variables based on availability of
# OA copies from publisher-provided OA.
adj_dat_n$pub_c_b <- ifelse(adj_dat_n$pub_c>0, "1", "0")
prop.table(table(adj_dat_n$pub_c_b))


#-----Use descriptive statistics to explore whether clicks 
# and number of types of OA copies are related to 
# citation rates. 

# Citation rate mean differences across click groups.
# Data are reported in Table 2 of the manuscript
adj_dat_n %>%
  dplyr::select(citation_c_adj, click_b) %>% 
  group_by(click_b) %>% 
  summarise(n = n(), 
            mean = mean(citation_c_adj), 
            sd = sd(citation_c_adj),
            median = median(citation_c_adj),
            min = min(citation_c_adj), 
            max = max(citation_c_adj))

# Citation rate mean differences by OA host type.
# Data for all host types are reported in Table 3 of the manuscript.
# Total OA availability
adj_dat_n %>%
  dplyr::select(citation_c_adj, oa_c_adj_n) %>% 
  group_by(oa_c_adj_n) %>% 
  summarise(n = n(), 
            mean = mean(citation_c_adj), 
            sd = sd(citation_c_adj),
            median = median(citation_c_adj),
            min = min(citation_c_adj), 
            max = max(citation_c_adj))

# IR
adj_dat_n %>%
  dplyr::select(citation_c_adj, ir_c_adj_c) %>% 
  group_by(ir_c_adj_c) %>% 
  summarise(n = n(), 
            mean = mean(citation_c_adj), 
            sd = sd(citation_c_adj),
            median = median(citation_c_adj),
            min = min(citation_c_adj), 
            max = max(citation_c_adj))

# DR (binary)
adj_dat_n %>%
  dplyr::select(citation_c_adj, dr_c_b) %>% 
  group_by(dr_c_b) %>% 
  summarise(n = n(), 
            mean = mean(citation_c_adj), 
            sd = sd(citation_c_adj),
            median = median(citation_c_adj),
            min = min(citation_c_adj), 
            max = max(citation_c_adj))


# Pub (binary)
adj_dat_n %>%
  dplyr::select(citation_c_adj, pub_c_b) %>% 
  group_by(pub_c_b) %>% 
  summarise(n = n(), 
            mean = mean(citation_c_adj), 
            sd = sd(citation_c_adj),
            median = median(citation_c_adj),
            min = min(citation_c_adj), 
            max = max(citation_c_adj))

# Other (binary)
adj_dat_n %>%
  dplyr::select(citation_c_adj, other_c_b) %>% 
  group_by(other_c_b) %>% 
  summarise(n = n(), 
            mean = mean(citation_c_adj), 
            sd = sd(citation_c_adj),
            median = median(citation_c_adj),
            min = min(citation_c_adj), 
            max = max(citation_c_adj))

###########---------------------multiple linear models as ANCOVA
# Test for correlations between clicks received from search engine 
# results pages, citations, and availability from different types of
# OA hosts.
library(lmtest)
library(sandwich)
library(car) 
library(broom)

adj_dat_n <- cbind(index = 1:nrow(adj_dat_n), adj_dat_n)

#------- Citation effects based on total OA availability
m1 <- lm (citation_c_adj ~ click_b + oa_c_adj_n, data = adj_dat_n)
summary(m1)
anova(m1)

#---- Assumptions
#-Normality assumptions
res <- m1$residuals
hist(res)
# We can't assume normality of residuals

#-Deal with outliers
m1_1 <- augment(m1) %>%
  mutate(index = 1:n())
m1_1 %>% top_n(3, .cooksd)
list_1 <- m1_1 %>%
  filter(abs(.std.resid) > 3)
index <- list_1$index
list_1_n <- data.frame(index)
adj_dat_n_1 <- bind_rows(adj_dat_n, list_1_n)

# Extract the rows which appear only once to remove influential values
adj_dat_n_1  <- adj_dat_n_1 [!(duplicated(adj_dat_n_1$index ) | duplicated(adj_dat_n_1$index , fromLast = TRUE)), ]


#-Run the model again without outliers.
# Results are presented in Table 5 of the manuscript.
m1_2 <- lm (citation_c_adj ~ click_b + oa_c_adj_n, data = adj_dat_n_1)
summary(m1_2)
anova(m1_2)


#------- Citation effects based on availability from IR
m2 <- lm (citation_c_adj ~ click_b + ir_c_adj_c, data = adj_dat_n)
summary(m2)

#---- Assumptions
#-Normality assumptions
res <- m2$residuals
hist(res)
# We can't assume normality of residuals

#-Deal with outliers
m2_1 <- augment(m2) %>%
  mutate(index = 1:n())
m2_1 %>% top_n(3, .cooksd)
list_2 <- m2_1 %>%
  filter(abs(.std.resid) > 3)
index <- list_2$index
list_2_n <- data.frame(index)
adj_dat_n_2 <- bind_rows(adj_dat_n, list_2_n)

# Extract the rows which appear only once to remove influential values.
adj_dat_n_2  <- adj_dat_n_2 [!(duplicated(adj_dat_n_2$index ) | duplicated(adj_dat_n_2$index , fromLast = TRUE)), ]
str(adj_dat_n_2)

#-Run the model again with outliers removed.
# Results are reported in Table 5 of the manuscript.
m2_2 <- lm (citation_c_adj ~ click_b + ir_c_adj_c, data = adj_dat_n_2)
summary(m2_2)
anova(m2_2)

#------- Citation effects based on availability from disciplinary repositories
m3 <- lm (citation_c_adj ~ click_b + dr_c_b, data = adj_dat_n)
summary(m3)

#---- Assumptions
#-Normality assumptions
res <- m3$residuals
hist(res)
# We can't assume normality of residuals

#-Deal with outliers.
m3_1 <- augment(m3) %>%
  mutate(index = 1:n())
m3_1 %>% top_n(3, .cooksd)
list_3 <- m3_1 %>%
  filter(abs(.std.resid) > 3)
index <- list_3$index
list_3_n <- data.frame(index)
adj_dat_n_3 <- bind_rows(adj_dat_n, list_3_n)

# Extract the rows which appear only once to remove influential values.
adj_dat_n_3  <- adj_dat_n_3 [!(duplicated(adj_dat_n_3$index ) | duplicated(adj_dat_n_3$index , fromLast = TRUE)), ]
str(adj_dat_n_3)

#-Run the model again with outliers removed.
# Results are reported in Table 5 of the manuscript.
m3_2 <- lm (citation_c_adj ~ click_b + dr_c_b, data = adj_dat_n_3)
summary(m3_2)
anova(m3_2)


#------- Citation effects based on availability of publisher-provided OA
m4 <- lm (citation_c_adj ~ click_b + pub_c_b, data = adj_dat_n)
summary(m4)

#---- Assumptions
#-Normality assumptions
res <- m4$residuals
hist(res)
# We can't assume normality of residuals

#-Deal with outliers
m4_1 <- augment(m4) %>%
  mutate(index = 1:n())
m4_1 %>% top_n(3, .cooksd)
list_4 <- m4_1 %>%
  filter(abs(.std.resid) > 3)
index <- list_4$index
list_4_n <- data.frame(index)
adj_dat_n_4 <- bind_rows(adj_dat_n, list_4_n)

# Extract the rows which appear only once to remove influential values
adj_dat_n_4  <- adj_dat_n_4 [!(duplicated(adj_dat_n_4$index ) | duplicated(adj_dat_n_4$index , fromLast = TRUE)), ]
str(adj_dat_n_4)

#-Run the model again without outliers.
# Results are reported in Table 5 of the manuscript
m4_2 <- lm (citation_c_adj ~ click_b + pub_c_b, data = adj_dat_n_4)
summary(m4_2)
anova(m4_2)


#-------Citation effects based on availability of "other" types of OA.
m5 <- lm (citation_c_adj ~ click_b + other_c_b, data = adj_dat_n)
summary(m5)

#---- Assumptions
#-Normality assumptions
res <- m5$residuals
hist(res)
# We can't assume normality of residuals

#-Deal with outliers
m5_1 <- augment(m5) %>%
  mutate(index = 1:n())
m5_1 %>% top_n(3, .cooksd)
list_5 <- m5_1 %>%
  filter(abs(.std.resid) > 3)
index <- list_5$index
list_5_n <- data.frame(index)
adj_dat_n_5 <- bind_rows(adj_dat_n, list_5_n)

# Extract the rows which appear only once to remove influential values
adj_dat_n_5  <- adj_dat_n_5 [!(duplicated(adj_dat_n_5$index ) | duplicated(adj_dat_n_5$index , fromLast = TRUE)), ]

#-Run the model again without outliers.
# Results are reported in Table 5 of the manuscript.
m5_2 <- lm (citation_c_adj ~ click_b + other_c_b, data = adj_dat_n_5)
summary(m5_2)
anova(m5_2)

###########---------------------linear models as one way ANOVA

#---Citation effects based on number of clicks received.
m6 <- lm(citation_c_adj ~ click_b, data = adj_dat_n) 
anova(m6)

#---- Assumptions
#-Normality assumptions
res <- m6$residuals
hist(res)
# We can't assume normality of residuals

#-Deal with outliers.
m6_1 <- augment(m6) %>%
  mutate(index = 1:n())
m6_1 %>% top_n(3, .cooksd)
list_6 <- m6_1 %>%
  filter(abs(.std.resid) > 3)
index <- list_6$index
list_6_n <- data.frame(index)
adj_dat_n_6 <- bind_rows(adj_dat_n, list_6_n)

# Extract the rows which appear only once to remove influential values
adj_dat_n_6  <- adj_dat_n_6 [!(duplicated(adj_dat_n_6$index ) | duplicated(adj_dat_n_6$index , fromLast = TRUE)), ]


#-Run the model again without outliers.
# Results are reported in Table 4 in the manuscript.
m6_2 <- lm (citation_c_adj ~ click_b, data = adj_dat_n_6)
anova(m6_2)
summary(m6_2)


# No new analysis from here forward.
# Remaining code draws tables for the manuscript.
# Note: Tables are not included in the github repository.

# Table 1: Open Access Availability by Host Type 
# Desc stats - count of OA copies per host type, % of total
t1_data <- adj_dat_n %>% 
  summarize("Items with OA availability" = sum(!is.na(oa_c_adj)),
            "Items hosted by IR" = sum(ir_c_adj > 0),
            "Items hosted by disciplinary repositories" = sum(dr_c > 0),
            "Items hosted by publisher OA repositories" = sum(pub_c > 0),
            "Items hosted by other types of OA repositories" = sum(other_c >0)) %>% 
  pivot_longer(
    cols = c(starts_with("Items")),
    names_to = "OA Host Type",
    values_to = "Frequency"
  )

t1_data$"Percentage of Observations" <- round((t1_data$Frequency/nrow(adj_dat_n))*100, 2)


t1_flex <- flextable(t1_data)
t1_flex <- set_caption(t1_flex, caption = "Table 1: Open Access Availability by Host Type (N = 13451)")

t1_flex
#autofit(t1_flex)
set_table_properties(t1_flex, layout = "autofit")

sect_properties <- prop_section(
  page_size = page_size(orient = "portrait",
                        width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar()
)
save_as_docx(t1_flex, values = NULL, 
             path = "../figures/Table_1.docx",
             pr_section = sect_properties)


# Table 2: Citation rate mean differences across click groups
t2_data <- adj_dat_n %>%
  dplyr::select(citation_c_adj, click_b) %>% 
  group_by(click_b) %>% 
  rename("Click group" = click_b) %>% 
  summarise(N = n(), 
            Mean = round(mean(citation_c_adj), 2), 
            SD = round(sd(citation_c_adj), 2),
            Median = round(median(citation_c_adj), 2),
            Min = round(min(citation_c_adj), 0), 
            Max = round(max(citation_c_adj), 0))

t2_flex <- flextable(t2_data)
t2_flex <- set_caption(t2_flex, caption = "Table 2: Citation rate mean differences across click groups")

t2_flex
set_table_properties(t2_flex, layout = "autofit")

sect_properties <- prop_section(
  page_size = page_size(orient = "portrait",
                        width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar()
)
save_as_docx(t2_flex, values = NULL, 
             path = "../figures/Table_2.docx",
             pr_section = sect_properties)

# Table 3
# Citation rate mean differences across sub-groups of
# different types of OA repositories
# Including % of total observations

# All OA hosts
t3_oa_data <- adj_dat_n %>%
  dplyr::select(citation_c_adj, oa_c_adj_n) %>% 
  group_by(oa_c_adj_n) %>% 
  summarise(N = n(),
            #"Pct of Observations" = round((n()/nrow(adj_dat_n))*100, 0),
            Mean = round(mean(citation_c_adj), 2), 
            SD = round(sd(citation_c_adj), 2),
            Median = round(median(citation_c_adj), 2),
            Min = round(min(citation_c_adj), 0),
            Max = round(max(citation_c_adj), 0))
t3_oa_data$oa_c_adj_n <- as.character(t3_oa_data$oa_c_adj_n)
t3_oa_data <- t3_oa_data %>% rename(Category = oa_c_adj_n)
t3_oa_data$Host <- "All OA hosts"
  
# IR
t3_ir_data <- adj_dat_n %>%
  dplyr::select(citation_c_adj, ir_c_adj_c) %>% 
  group_by(ir_c_adj_c) %>% 
  summarise(N = n(),
            #"Pct of Observations" = round((n()/nrow(adj_dat_n))*100, 0),
            Mean = round(mean(citation_c_adj), 2), 
            SD = round(sd(citation_c_adj), 2),
            Median = round(median(citation_c_adj), 2),
            Min = round(min(citation_c_adj), 0), 
            Max = round(max(citation_c_adj), 0))
t3_ir_data$ir_c_adj_c <- as.character(t3_ir_data$ir_c_adj_c)
t3_ir_data <- t3_ir_data %>% rename(Category = ir_c_adj_c)
t3_ir_data$Host <- "Institutional repositories"

# DR
t3_dr_data <- adj_dat_n %>%
  dplyr::select(citation_c_adj, dr_c_b) %>% 
  group_by(dr_c_b) %>%  
  summarise(N = n(),
            #"Pct of Observations" = round((n()/nrow(adj_dat_n))*100, 0),
            Mean = round(mean(citation_c_adj), 2), 
            SD = round(sd(citation_c_adj), 2),
            Median = round(median(citation_c_adj), 2),
            Min = round(min(citation_c_adj), 0), 
            Max = round(max(citation_c_adj), 0))
t3_dr_data$dr_c_b <- as.character(t3_dr_data$dr_c_b)
t3_dr_data <- t3_dr_data %>% rename(Category = dr_c_b)
t3_dr_data$Host <- "Disciplinary repositories"

# Pub
t3_pub_data <- adj_dat_n %>%
  dplyr::select(citation_c_adj, pub_c_b) %>% 
  group_by(pub_c_b) %>%  
  summarise(N = n(),
            #"Pct of Observations" = round((n()/nrow(adj_dat_n))*100, 0),
            Mean = round(mean(citation_c_adj), 2), 
            SD = round(sd(citation_c_adj), 2),
            Median = round(median(citation_c_adj), 2),
            Min = round(min(citation_c_adj), 0), 
            Max = round(max(citation_c_adj), 0))
t3_pub_data$pub_c_b <- as.character(t3_pub_data$pub_c_b)
t3_pub_data <- t3_pub_data %>% rename(Category = pub_c_b)
t3_pub_data$Host <- "Publisher OA"

# Other
t3_oth_data <- adj_dat_n %>%
  dplyr::select(citation_c_adj, other_c_b) %>% 
  group_by(other_c_b) %>%  
  summarise(N = n(),
            #"Pct of Observations" = round((n()/nrow(adj_dat_n))*100, 0),
            Mean = round(mean(citation_c_adj), 2), 
            SD = round(sd(citation_c_adj), 2),
            Median = round(median(citation_c_adj), 2),
            Min = round(min(citation_c_adj), 0), 
            Max = round(max(citation_c_adj), 0))
t3_oth_data$other_c_b <- as.character(t3_oth_data$other_c_b)
t3_oth_data <- t3_oth_data %>% rename(Category = other_c_b)
t3_oth_data$Host <- "Other OA"

library(plyr)
dfs <- list(t3_oa_data, t3_ir_data, t3_dr_data, t3_pub_data, t3_oth_data)
t3_data <- ldply(dfs, rbind)
detach("package:plyr", unload = TRUE)

# Combined table for descriptive stats of citations 
# based on OA availability

t3_flex <- as_grouped_data(t3_data, groups = "Host") %>%
  as_flextable() %>% 
  valign(valign = "top") %>% 
  autofit()
t3_flex

t3_flex <- t3_flex %>% 
  fontsize(i = ~ !is.na(Host), size = 11) %>% 
  font(i = ~ !is.na(Host), fontname = "Times") %>% 
  bold(i = ~ !is.na(Host), bold = TRUE) %>% 
  italic(i = ~ !is.na(Host), italic = TRUE)

t3_flex <- set_caption(t3_flex, caption = "Table 3: Citation rate mean differences by OA host type.")
autofit(t3_flex)
save_as_docx(t3_flex, values = NULL, path = "../figures/Table_3.docx", 
             pr_section = NULL)


# ANCOVA Citation mean differences between click groups
t4_flex <- as_flextable(m6_2)
t4_flex <- set_caption(t4_flex, caption = "Table 4: Citation mean differences by click groups.")
t4_flex
set_table_properties(t4_flex, layout = "autofit")

sect_properties <- prop_section(
  page_size = page_size(orient = "portrait",
                        width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar()
)
save_as_docx(t4_flex, values = NULL, 
             path = "../figures/Table_4.docx",
             pr_section = sect_properties)


# Table 5
library(stargazer)
stargazer(m1_2,
          m2_2,
          m3_2,
          m4_2,
          m5_2,
          type="html",
          dep.var.labels = "Per-year citation rates per availability of OA copies by repository type",
          covariate.labels = c('Intercept',
                               'Clicks above median',
                               'Total OA copies above median',
                               'Count IR copies above median',
                               'DR OA available',
                               'Publisher OA available',
                               'Other OA available'),
          #ci = TRUE,
          #single.row = TRUE,
          intercept.bottom = FALSE,
          intercept.top = TRUE,
          align = TRUE,
          report = "vcst*",
          out = "../figures/Table_5.doc",
          notes = "Table 5: Citation Impact of OA Copies of Items Held by Repository Type.")
