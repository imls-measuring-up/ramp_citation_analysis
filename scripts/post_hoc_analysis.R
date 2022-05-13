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
library(lmtest)
library(sandwich)
library(car) 
library(broom)
getwd()

# Import the aggregated dataset.
dat <- import("../data/ramp_crossref_unpaywall_by_hosts.csv")
str(dat)
summary(dat)

#------verify counts of DOIs across hosts are the same as reported
total_unique_dois <- dat %>% 
  group_by(doi) %>% 
  summarise(count = n())

# count unique dois 13452 matches reported number

total_ir_dois <- dat %>% 
  filter(repo_subtype == 'institutional') %>% 
  group_by(doi) %>% 
  summarise(count = n())

# count 11590 doesn't match reported - recall that some
# IR hosts are not indexed by unpaywall
# so we can check the difference (1862 DOIs)
# see how many doi in analyzed dataset have 0 IR copies
# we can do this here - read and filter the analyzed dataset
# as done before analysis
analyzed_dat <- import("../data/ramp_crossref_unpaywall_merged.csv")
analyzed_dat$count_error <- (analyzed_dat$ir_is_oa_loc == TRUE & analyzed_dat$ct_ir_oa_copies == 0)
analyzed_dat <- analyzed_dat %>% filter(count_error != TRUE)
analyzed_dat_adj <- dplyr::select(analyzed_dat,
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

analyzed_dat_adj <- analyzed_dat_adj %>%
  filter(ir_pub_year < 2017)

analyzed_dat_adj$ir_pub_year <- as.numeric(analyzed_dat_adj$ir_pub_year)
analyzed_dat_adj$cref_created_year <- as.numeric(analyzed_dat_adj$cref_created_year)

analyzed_dat_adj$pub_yr_diff <- analyzed_dat_adj$ir_pub_year - analyzed_dat_adj$cref_created_year
analyzed_dat_adj <- analyzed_dat_adj %>% filter(pub_yr_diff == 0 | pub_yr_diff == 1)

# get the count of DOIs with 0 IR copies before adjustment
ir_not_hosts <- analyzed_dat_adj %>% 
  filter(ct_ir_oa_copies == 0) %>% # recall that this is before adjusting for known IR copies 
  group_by(doi) %>% 
  summarise(count = n())

# count of 1862 matches our difference above

total_dr_dois <- dat %>% 
  filter(repo_subtype == 'disciplinary') %>% 
  group_by(doi) %>% 
  summarise(count = n())

# count of 3496 matches reported number

total_pub_dois <- dat %>% 
  filter(repo_subtype == 'publisher') %>% 
  group_by(doi) %>% 
  summarise(count = n())

# count of 3999 matches reported number

total_oth_dois <- dat %>% 
  filter(repo_subtype == 'other') %>% 
  group_by(doi) %>% 
  summarise(count = n())

# count of 3755 matches reported number

# Finally, use set operations to compare 1:1 DOI matching
host_dois <- unique(dat$doi)
host_dois

analyzed_dois <- unique(analyzed_dat_adj$doi)
analyzed_dois

dois_sets_intersection <- intersect(host_dois, analyzed_dois) # should be eq to total_unique_dois
setequal(host_dois, analyzed_dois) # should be TRUE
setdiff(analyzed_dois, host_dois) # should be 0 length char vector
setdiff(host_dois, analyzed_dois) # should be 0 length char vector


#------explore impacts of disciplines on citations
dat_disciplinary <- dat%>%
  filter(repo_subtype=="disciplinary")

dat_disciplinary$repo_name <- as.factor(dat_disciplinary$repo_name)
table(dat_disciplinary$repo_name)

#----categorize repositories by disciplines
dat_disciplinary$dr_type <- ifelse(dat_disciplinary$repo_name=="PubMed Central"|dat_disciplinary$repo_name=="PubMed Central - Europe PMC", 
                                   "Medical and Health Sciences",
                                   ifelse(dat_disciplinary$repo_name=="arXiv.org"|dat_disciplinary$repo_name=="Cornell University - arXiv",
                                          "Engineering, Technology, and Natural Sciences", "Others"))
table(dat_disciplinary$dr_type)
View(dat_disciplinary)

#----count manuscripts by disciplines
disc_c <- dat_disciplinary%>%
  group_by(doi, dr_type)%>%
  count()

dat_disciplinary <- left_join(dat_disciplinary, disc_c, by = c("doi", "dr_type"))


#---avoid double counting observations in each type of disciplinary repositories
dat_disciplinary_no_dup <- dat_disciplinary[!duplicated(dat_disciplinary[c(1,21)]),]
table(dat_disciplinary_no_dup$dr_type)


#---select only variables of interest
dat_disciplinary_no_dup <- dat_disciplinary_no_dup[, c(1, 3, 5, 21, 22)]
str(dat_disciplinary_no_dup)


dat_disciplinary_no_dup$dr_c <- as.factor(dat_disciplinary_no_dup$dr_c)

#---explore citation differences across disciplinary repositories
dat_disciplinary_no_dup%>%
  group_by(dr_type)%>%
  summarise(mean_citation = mean(citation_c_adj))

#---rows with duplicated DOI
duplicate_doi <- dat_disciplinary_no_dup[duplicated(dat_disciplinary_no_dup$doi),]
View(duplicate_doi)

#---remove observations which have # of copies deposited into more than one type of disciplinary repositories
#---to avoid having same DOIs in more than one disciplinary repositories
#---observations should be independent across conditions
clean_dat <- anti_join(dat_disciplinary_no_dup, duplicate_doi, by = "doi")

clean_dat%>%
  group_by(dr_type)%>%
  summarise(mean_citation = mean(citation_c_adj))

#---explore citation differences and citation differences due to number of copies across disciplinary repositories
clean_dat%>%
  group_by(dr_type)%>%
  summarise(mean_citation = mean(citation_c_adj), mean_copy = mean(n), median_copy = median(n))



clean_dat$disc_c <- ifelse(clean_dat$n==1, "1", "Above 1")

clean_dat%>%
  group_by(dr_type, disc_c)%>%
  count()
#-------Is this interesting that manuscripts in medical and health sciences tend to be deposited in more than one disciplinary repositories while 
#-------it is not a common practice for other disciplines.


clean_dat%>%
  group_by(dr_type, disc_c)%>%
  summarise(mean_citation = mean(citation_c_adj))
#----It is not a good idea to look at citation differences due to the number of copies in each category of disciplinary repositories due to
#----conflicting results


#-----testing to see if there are citation differences across disciplinaries
clean_dat <- cbind(index = 1:nrow(clean_dat), clean_dat)
m_disc <- lm (citation_c_adj ~ dr_type, data = clean_dat)
summary(m_disc )
anova(m_disc )

#---- Assumptions
#-Normality assumptions
res <- m_disc $residuals
hist(res)
# We can't assume normality of residuals

#-Deal with outliers
m_disc_1 <- augment(m_disc) %>%
  mutate(index = 1:n())
m_disc_1 %>% top_n(3, .cooksd)
list_disc <- m_disc_1 %>%
  filter(abs(.std.resid) > 3)
index <- list_disc$index
list_1_n <- data.frame(index)
clean_dat_n <- bind_rows(clean_dat, list_1_n)

# Extract the rows which appear only once to remove influential values
clean_dat_n  <- clean_dat_n [!(duplicated(clean_dat_n$index ) | duplicated(clean_dat_n$index , fromLast = TRUE)), ]


#-Run the model again without outliers.
# Results are presented in Table 5 of the manuscript.
m_disc_1 <- lm (citation_c_adj ~ dr_type, data = clean_dat_n)
summary(m_disc_1)
anova(m_disc_1)

# check variance homogeneity
leveneTest(m_disc_1) # this assumption is violated

oneway.test(citation_c_adj ~ dr_type, data = clean_dat_n) # run the oneway.test when variance homogeneity is violated. 
# results are similary to lm and anovo test.

#adjust standard erros in the lm model
coeftest(m_disc_1, vcov = vcovHC(m_disc_1, type = "HC0")) # citation advantages vary across disciplines
# manuscripts in medical and health sciences have higher citations than engineering, technology, and natural sciences
# which have higher citations than other disciplines.

#-------check impact of number of copies in disciplinary repositories again

#---avoid double counting observations in each type of disciplinary repositories
dat_disc_n <- dat_disciplinary[!duplicated(dat_disciplinary[c(1)]),]
View(dat_disc_n)
summary(dat_disc_n$dr_c)

dat_disc_n <- cbind(index = 1:nrow(dat_disc_n), dat_disc_n)

m_disc <- lm (citation_c_adj ~dr_c, data = dat_disc_n )
summary(m_disc)

# handle outliers
res <- m_disc $residuals
hist(res)
# We can't assume normality of residuals

#-Deal with outliers
m_disc_1 <- augment(m_disc) %>%
  mutate(index = 1:n())
m_disc_1 %>% top_n(3, .cooksd)
list_disc <- m_disc_1 %>%
  filter(abs(.std.resid) > 3)
index <- list_disc$index

index <- list_disc$index
list_1_n <- data.frame(index)
dat_disc_n <- bind_rows(dat_disc_n, list_1_n)

# Extract the rows which appear only once to remove influential values
dat_disc_n  <- dat_disc_n [!(duplicated(dat_disc_n$index ) | duplicated(dat_disc_n$index , fromLast = TRUE)), ]


#-Run the model again without outliers.
# Results are presented in Table 5 of the manuscript.
m_disc_1 <- lm (citation_c_adj ~ dr_c, data = dat_disc_n)
summary(m_disc_1)

# check error distribution again
error <- resid(m_disc_1)
shapiro.test(error) # errors are not normally distributed

#test homoscedasticity
bptest(m_disc_1, studentize = FALSE) # variance homoscedasticity violation

coeftest(m_disc_1, df = Inf, vcov = vcovHC(m_disc_1, type = "HC3"))
#---the increase in number of copies in disciplinary repositories lead to the increase in the number of citations

