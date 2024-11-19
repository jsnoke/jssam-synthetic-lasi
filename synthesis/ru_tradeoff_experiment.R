####################################################
####################################################
#
#
# Date: 01.26.2024
# Author: Joshua Snoke
# Description: Create Synthetic Data Sets with Different Synthesis Methods
#               Unweighted, Grouped, Weighted r2 (0.9, 0.5, 0), Sample
#
#
####################################################
####################################################

library(tidyverse)
library(here)
library(synthpop)

i_am('synthesis/ru_tradeoff_experiment.R')

source(here('synthesis', 'synthesis_helper_funs.R'))

## load in hh level file create from original LASI data
#hh_unsynthesized_lasi = read_rds('')
#indiv_unsynthesized_lasi = readRDS('')

## log linear risk rates
#target_records = readRDS('')
hh_unsynthesized_lasi = hh_unsynthesized_lasi %>%
  left_join(target_records %>%
              select(hhid,
                     r2))

## first do all the non_grouped approaches
## which state variable
state_syn_var = 'hh1state'
filter_ids = hh_unsynthesized_lasi$hhid
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

## unweighted synthesis
set.seed(998579)
unweighted_syn_data = syn_wrapper(conf_data,
                                  m = 30,
                                  lasi_predictor_matrix = lasi_predictor_matrix,
                                  max_person_size = synth_max_person,
                                  state_syn_var = state_syn_var,
                                  synth_function = 'unweighted_synthesis')

## cleanup and reshape
hh_synthesized_lasi = lapply(unweighted_syn_data,
                             cleanup_hh,
                             hh_unsynthesized_lasi,
                             state_syn_var = state_syn_var,
                             max_person_size = synth_max_person)

indiv_synthesized_lasi = lapply(hh_synthesized_lasi,
                                cleanup_indiv,
                                indiv_unsynthesized_lasi)

#saveRDS('')
#saveRDS('')


## run weighted syntheses at three levels
temp_cutoff = c(0.9, 0.5, 0)
for(temp in temp_cutoff){
  
  weighted_syn_data = syn_wrapper(conf_data,
                                  m = 30,
                                  lasi_predictor_matrix = lasi_predictor_matrix,
                                  max_person_size = synth_max_person,
                                  state_syn_var = state_syn_var,
                                  weight_var = 'r2',
                                  weight_cutoff = temp,
                                  synth_function = 'weighted_synthesis')
  
  ## cleanup and reshape
  hh_synthesized_lasi = lapply(weighted_syn_data,
                               cleanup_hh,
                               hh_unsynthesized_lasi,
                               state_syn_var = state_syn_var,
                               max_person_size = synth_max_person)
  
  indiv_synthesized_lasi = lapply(hh_synthesized_lasi,
                                  cleanup_indiv,
                                  indiv_unsynthesized_lasi)
  
  #saveRDS('')
  #saveRDS('')
  cat(temp, '\n')
}


## run sample synthesis
sample_syn_data = syn_wrapper(conf_data,
                              m = 30,
                              lasi_predictor_matrix = lasi_predictor_matrix,
                              max_person_size = synth_max_person,
                              state_syn_var = state_syn_var,
                              synth_function = 'sample_synthesis')

## cleanup and reshape
hh_synthesized_lasi = lapply(sample_syn_data,
                             cleanup_hh,
                             hh_unsynthesized_lasi,
                             state_syn_var = state_syn_var,
                             max_person_size = synth_max_person)

indiv_synthesized_lasi = lapply(hh_synthesized_lasi,
                                cleanup_indiv,
                                indiv_unsynthesized_lasi)

#saveRDS('')
#saveRDS('')



## run unweighted GROUEPD synthesis
state_syn_var = 'hh1state_grouped'

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

unweighted_syn_data = syn_wrapper(conf_data,
                                  m = 30,
                                  lasi_predictor_matrix = lasi_predictor_matrix,
                                  max_person_size = synth_max_person,
                                  state_syn_var = state_syn_var,
                                  synth_function = 'unweighted_synthesis')

## cleanup and reshape
hh_synthesized_lasi = lapply(unweighted_syn_data,
                             cleanup_hh,
                             hh_unsynthesized_lasi,
                             state_syn_var = state_syn_var,
                             max_person_size = synth_max_person)

indiv_synthesized_lasi = lapply(hh_synthesized_lasi,
                                cleanup_indiv,
                                indiv_unsynthesized_lasi)

#saveRDS('')
#saveRDS('')







