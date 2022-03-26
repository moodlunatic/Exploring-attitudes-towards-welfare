library(haven)
library(dplyr)
library(psych)
library(lavaan)
library(stringr)
library(tidySEM)
library(corrplot)
ess_df <- haven::read_sav("https://github.com/albertostefanelli/SEM_labs/raw/master/ESS4_belgium.sav")

head(ess_df, 20) 

nrow(ess_df)
ncol(ess_df)
names(ess_df)
ess_df_selected <- ess_df %>% select(
  ## Welfare support items ##
  gvslvol, # the old
  gvslvue, # the unemployed
  gvhlthc, # the sick
  gvcldcr, # working parents
  gvjbevn, # job for everyone
  gvpdlwk, # paid sick leave  
  ##    Economic criticism items ##
  sbstrec, # strain on economy
  sbbsntx, # too much taxes
  ##    Social criticism items ## 
  sbprvpv, # poverty
  sbeqsoc, # more equal society
  sbcwkfm, # work and family
  ##    Moral criticism items ##
  sblazy,  # people lazy 
  sblwcoa, # care for others
  sblwlka  # look after others
)

descriptive_ess <- as.data.frame(psych::describe(ess_df_selected))

descriptive_ess <- dplyr::select(descriptive_ess, 
                                 n,
                                 mean,
                                 sd,
                                 median,
                                 min,
                                 max,
                                 skew,
                                 kurtosis)

descriptive_ess


# let's select the welfare support items 

ess_df_welfare_supp <- ess_df %>% select(
  ## Welfare support items ##
  gvslvol, # the old
  gvslvue, # the unemployed
  gvhlthc, # the sick
  gvcldcr, # working parents
  gvjbevn, # job for everyone
  gvpdlwk  # paid sick leave  
)

# let's calculate the sample implied covariance matrix 
welfare_supp_cov <- cov(ess_df_welfare_supp,          # data frame 
                        use = "pairwise.complete.obs" # remove NAs 
)

welfare_supp_cov

# visual inspection is sometimes more useful
# we can use the corrplot package.
# it it designed for correlation matrices 
# but they can also plot covariance matrices 

welfare_supp_cor <- cov2cor( welfare_supp_cov)
welfare_supp_cor


corrplot::corrplot(welfare_supp_cor, 
                   is.corr = FALSE,       # whether is a correlation matrix 
                   method = "circle",     # magnitude of covariances as circles 
                   type = "upper",        # remove the bottom of the covariance matrix
                   addCoef.col = "black"  # add to the plot the coefficients
)




model_ws_3 <-'welf_supp =~ gvslvol + gvslvue + gvhlthc'

fit_ws_3 <- cfa(model_ws_3,      # model formula
                data=ess_df_selected  # data frame
)

summary(fit_ws_3)

model_ws_3_alt <-'welf_supp =~ NA*gvslvol + gvslvue + gvhlthc
welf_supp~~1*welf_supp
'

fit_ws_3_alt <- cfa(model_ws_3_alt, # model formula
                    data=ess_df_selected     # data frame
)

summary(fit_ws_3_alt)


model_ws_6 <-'welf_supp =~ gvslvol + gvslvue + gvhlthc + gvcldcr + gvjbevn + gvpdlwk'


fit_ws_6 <- cfa(model_ws_6,      # model formula
                data=ess_df_selected  # data frame
)
model_wc <-'
## Economic criticism ##
wc_econo =~ sbstrec + sbbsntx
## Social criticism ## 
wc_socia =~ sbprvpv + sbeqsoc + sbcwkfm
##  Moral criticism ##
wc_moral =~ sblazy + sblwcoa + sblwlka
'

fit_wc <- cfa(model_wc,          # model formula
              data=ess_df_selected  # data frame
)

summary(fit_wc)
# Returns the observed covariance matrix.
lavInspect(fit_ws_6, "sampstat")

# first, let's define our plot layout 
lay <- get_layout("wc_econo", "", "", "wc_socia","", "","wc_moral", "",
                  "sbstrec", "sbbsntx", "sbprvpv", "sbeqsoc", "sbcwkfm", "sblazy", "sblwcoa", "sblwlka", rows = 2)

# let's take a look at our plot layout.
lay
plot_wc <- graph_sem(model = fit_wc,      # model fit
                     layout = lay,        # layout 
                     angle = 170          # adjust the arrows 
                     #label = "est_std",  # get standardized results (not rounded)
)   

plot_wc

graph_data <- prepare_graph(fit_wc)

edges(graph_data) <- graph_data %>% 
  edges() %>%
  mutate(colour = "black") %>%
  mutate(colour = replace(colour, from == "wc_socia" & to == "sbprvpv", "red"))

plot(graph_data,
     layout = lay,        # layout 
     angle = 170          # adjust the arrows 
)





