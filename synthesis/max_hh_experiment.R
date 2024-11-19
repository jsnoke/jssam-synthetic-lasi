####################################################
####################################################
#
#
# Date: 11.30.2023
# Author: Joshua Snoke
# Description: Create Synthetic Data Sets with Different Max HH
#
#
####################################################
####################################################

library(tidyverse)
library(here)
library(synthpop)

i_am('synthesis/synthesis_cleanup.R')

source(here('synthesis', 'synthesis_helper_funs.R'))

## load in hh level file create from confidential LASI data
#hh_unsynthesized_lasi = read_rds('')
#indiv_unsynthesized_lasi = readRDS('')

## which state variable
state_syn_var = 'hh1state'
filter_ids = hh_unsynthesized_lasi$hhid

## set max (hh slots)
synth_max_person = seq(2, 12, 1)

for(temp_max in synth_max_person){
  ## synthesis process
  conf_data = prepare_data(input_data = hh_unsynthesized_lasi %>%
                             ## drop unused hh slots
                             select(-ends_with(paste('_', (temp_max + 1):max(17, temp_max + 1), sep = ''))),
                           filter_ids,
                           state_syn_var,
                           max_person_size = temp_max)
  
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
                                                  max_person_size = temp_max)
  
  ## unweighted synthesis
  set.seed(864558)
  system.time({unweighted_syn_data = syn_wrapper(conf_data,
                                                 m = 30,
                                                 lasi_predictor_matrix = lasi_predictor_matrix,
                                                 max_person_size = temp_max,
                                                 state_syn_var = state_syn_var,
                                                 synth_function = 'unweighted_synthesis')})
  
  ## cleanup and reshape
  hh_synthesized_lasi = lapply(unweighted_syn_data,
                               cleanup_hh,
                               hh_unsynthesized_lasi,
                               state_syn_var = state_syn_var,
                               max_person_size = temp_max)
  
  indiv_synthesized_lasi = lapply(hh_synthesized_lasi,
                                  cleanup_indiv,
                                  indiv_unsynthesized_lasi)
  
  #saveRDS(hh_synthesized_lasi, '')
  #saveRDS(indiv_synthesized_lasi, '')
  cat(temp_max, '\n')
}







