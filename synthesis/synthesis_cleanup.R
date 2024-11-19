####################################################
####################################################
#
#
# Date: 02.22.2023
# Author: Joshua Snoke
# Description: Takes HH synthetic file and converts back to individual file
#
#
####################################################
####################################################

library(tidyverse)
library(here)
library(synthpop)
library(randomForest)

i_am('synthesis/synthesis_cleanup.R')

source(here('synthesis', 'synthesis_helper_funs.R'))

## 1. start with the confidential data
## load in hh level file create from original LASI data
#hh_unsynthesized_lasi = read_rds('')
#indiv_unsynthesized_lasi = readRDS('')

## get hh base weights to use for model weighting
## add to original data
#hh_base_weights = haven::read_dta('')
hh_unsynthesized_lasi = hh_unsynthesized_lasi %>%
  left_join(hh_base_weights)

## log linear risk rates
#target_records = readRDS('')
hh_unsynthesized_lasi = hh_unsynthesized_lasi %>%
  left_join(target_records %>%
              select(hhid,
                     r2))

filter_ids = hh_unsynthesized_lasi$hhid

## which state variable
state_syn_var = 'hh1state'

synth_max_person = 8

## synthesis process
conf_data = prepare_data(input_data = hh_unsynthesized_lasi %>%
                           select(-ends_with(paste('_', (synth_max_person + 1):17, sep = ''))),
                         filter_ids,
                         state_syn_var,
                         max_person_size = synth_max_person)

## create predictor matrix
lasi_predictor_matrix = create_predictor_matrix(state_syn_var,
                                                person_var = c('r1agey_bin', 
                                                               'ragender', 
                                                               'raedyrs',
                                                               'raeduc_l',
                                                               'r1shlt',
                                                               'r1adlfive',
                                                               'r1work',
                                                               'r1iearn',
                                                               'r1wtresp'),
                                                hh_var = c('hhhgender',
                                                           'hh1hhres',
                                                           'hh1wthh'),
                                                structure_var = c('hh1rural', 
                                                                  'hhsize',
                                                                  'personexists_1'),
                                                max_person_size = synth_max_person)

## unweighted
raw_syn_data = synth_region(input_data = conf_data,
                            random_seed = 864558,
                            state_syn_var = state_syn_var,
                            lasi_predictor_matrix = lasi_predictor_matrix,
                            max_person_size = synth_max_person,
                            weight_var = 'r2',
                            weight_cutoff = 0,
                            synth_function = 'weighted_synthesis')

## cleanup and reshape
hh_synthesized_lasi = cleanup_hh(raw_syn_data,
                                 hh_unsynthesized_lasi,
                                 state_syn_var = state_syn_var,
                                 max_person_size = synth_max_person)

indiv_synthesized_lasi = cleanup_indiv(hh_synthesized_lasi,
                                       indiv_unsynthesized_lasi)

#saveRDS('')
#saveRDS('')



