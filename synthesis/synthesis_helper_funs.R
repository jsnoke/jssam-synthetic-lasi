####################################################
####################################################
#
#
# Date: 11.15.2023
# Author: Joshua Snoke
# Description: Helper Functions for Synthesis
#
#
####################################################
####################################################

person_exists = function(varr){
  case_when(is.na(varr) ~ '0',
            TRUE ~ '1')
  }

## subset confidential data
## plus change characters to ordered or factors
## some stuff is hard-coded here
prepare_data = function(input_data, 
                        filter_ids,
                        state_syn_var,
                        max_person_size = 17){
  input_data = input_data %>%
    ## dropping records (if any)
    filter(hhid %in% filter_ids) %>%
    ## dropping variables
    select(-contains(c('hhhpresent',
                       'hhposition'))) %>%
    mutate_at(vars(!!sym(state_syn_var),
                   contains(c('hh1rural',
                              'hhhgender',
                              'hhsize',
                              'ragender',
                              'r1work'))), as.factor) %>%
    mutate_at(vars(contains(c('r1shlt',
                              'raeduc_l',
                              'r1adlfive'))), as.ordered) %>%
    ## add variables for "this person exists"
    mutate(across(num_range('r1agey_', 1:max_person_size), 
                  person_exists, 
                  .names = "{sub('r1agey', 'personexists', col)}"))
}

## some stuff is hard-coded
create_predictor_matrix = function(state_syn_var,
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
                                   max_person_size = 17){
  
  ## add state to hh var
  structure_var = c(state_syn_var, structure_var)
  
  ## create person variable synthesis order vector
  ## number of person slots
  person_var_order = paste(person_var, 
                           1, sep = '_')
  for(a in 2:max_person_size){
    structure_var = c(structure_var,
                      paste('personexists',
                            a,
                            sep = '_'))
    person_var_order = c(person_var_order,
                         paste(person_var, 
                               a, sep = '_'))
  }
  
  ##  set predictor matrix manually
  lasi_predictor_matrix = matrix(0, 
                                 nrow = length(c(hh_var,
                                                 person_var_order)),
                                 ncol = length(c(structure_var,
                                                 hh_var,
                                                 person_var_order)),
                                 dimnames = list(c(hh_var,
                                                   person_var_order),
                                                 c(structure_var,
                                                   hh_var,
                                                   person_var_order)))
  
  ## remaining hh variables (except weight)
  count = length(structure_var)
  for(temp_var in hh_var){
    lasi_predictor_matrix[temp_var, 1:count] = 1
    count = count + 1
  }
  
  ## each indiv variable after the first is predicted by prev. variables
  ## within person and prev. persons within variable and hh variables
  all_people = c(1:max_person_size)
  all_var = setdiff(person_var, 
                    c('r1wtresp'))
  hh_pred = setdiff(c(structure_var,hh_var), 'hh1wthh')
  weight_var = NULL
  for(temp_person in all_people){
    sub_var = NULL
    for(temp_var in all_var){
      current_var = paste(temp_var, temp_person, sep = '_')
      
      ## set predictors
      ## if not first variable
      if(length(sub_var) > 0){
        ## if not first person
        if(temp_person > 1){
          ## get this var for all previous people
          other_person_var = paste(temp_var, seq(temp_person - 1, 1, -1), sep = '_')
          ## get all variables for couple partner (if second person in couple)
          if(temp_person %% 2 == 0){
            partner_var = paste(all_var, temp_person - 1, sep = '_')
          } else{
            partner_var = NULL
          }
          ## combine all
          keep_var = c(hh_pred,
                       paste(sub_var, sep = '_'), 
                       partner_var,
                       other_person_var)
          
        } else{
          keep_var = c(hh_pred,
                       paste(sub_var, sep = '_'))
        }
      } else{
        if(temp_person > 1){
          ## get this var for all previous people
          other_person_var = paste(temp_var, seq(temp_person - 1, 1, -1), sep = '_')
          ## get all variables for couple partner (if second person in couple)
          if(temp_person %% 2 == 0){
            partner_var = paste(all_var, temp_person - 1, sep = '_')
          } else{
            partner_var = NULL
          }
          ## combine all
          keep_var = c(hh_pred,
                       partner_var,
                       other_person_var)
          
        } else{
          keep_var = c(hh_pred)
        }
      }
      
      lasi_predictor_matrix[current_var, keep_var] = 1
      
      ## add var as predictor for next
      sub_var = c(sub_var, 
                  current_var)
    }
    ## lastly add predictors for individual weights
    lasi_predictor_matrix[paste('r1wtresp', 
                                temp_person, 
                                sep = '_'), c('hh1wthh', 
                                              weight_var, 
                                              sub_var)] = 1
    weight_var = c(weight_var, paste('r1wtresp', temp_person, sep = '_'))
  }
  
  return(lasi_predictor_matrix)
}

sample_synthesis = function(temp_data,
                            state_syn_var,
                            lasi_predictor_matrix,
                            max_person_size = 17){
  person_grep = paste('_', 2:max_person_size, collapse = '|', sep = '')
  ## create object to hold the new values
  syn_data = temp_data %>%
    ## first take random sample of households within state and rurality 
    ## for distribution of hhsize and personexistence
    group_by(!!sym(state_syn_var),
             hh1rural) %>%
    sample_n(size = n(), replace = TRUE) %>%
    as.data.frame
  
  ## iterate over each variable one at a time
  for(temp_var in 1:nrow(lasi_predictor_matrix)){
    ## variable to model/synthesize
    temp_var_name = rownames(lasi_predictor_matrix)[temp_var]
    ## predictors
    pred_names = names(which(lasi_predictor_matrix[temp_var_name, ] == 1))
    
    ## keep all rows for household, person exists, and first person variables
    if(!(grepl(person_grep, temp_var_name)) | grepl('personexists', temp_var_name)){
      temp_syn = syn.sample(y = temp_data[, temp_var_name],
                            xp = nrow(syn_data))
      ## add synthetic values
      syn_data[, temp_var_name] = temp_syn$res
    } else if(grepl(person_grep, temp_var_name) & !(grepl('personexists', temp_var_name))){
      temp_person = paste('personexists_', sub(".*\\_", "", temp_var_name), sep = '')
      ## only synthesis if there are any "persons" in this slot
      if(nrow(syn_data[syn_data[, temp_person] == 1, ]) > 0){
        ## run synthesis only for records where "person exists"
        ## run sample if there is no variation in variable
        temp_syn = syn.sample(y = temp_data[temp_data[, temp_person] == 1, temp_var_name],
                              xp = sum(syn_data[, temp_person] == 1))
        ## first set all synthetic values to NA to preserve correct missing pattern
        if(class(temp_syn$res) == 'factor'){
          syn_data[, temp_var_name] = factor(NA, levels = levels(temp_data[, temp_var_name]))
        } else if(class(temp_syn$res) == 'numeric'){
          syn_data[, temp_var_name] = NA_real_
        }
        ## add non-missing synthetic values
        syn_data[syn_data[, temp_person] == 1, temp_var_name] = temp_syn$res
      } else{
        if(class(temp_data[, temp_var_name]) == 'factor'){
          syn_data[, temp_var_name] = factor(NA, levels = levels(temp_data[, temp_var_name]))
        } else if(class(temp_data[, temp_var_name]) == 'numeric'){
          syn_data[, temp_var_name] = NA_real_
        }
      }
    }
  }
  return(syn_data)
}


unweighted_synthesis = function(temp_data,
                                state_syn_var,
                                lasi_predictor_matrix,
                                max_person_size = 17){
  person_grep = paste('_', 2:max_person_size, collapse = '|', sep = '')
  ## create object to hold the new values
  syn_data = temp_data %>%
  ## first take random sample of households within state and rurality 
  ## for distribution of hhsize and personexistence
  group_by(!!sym(state_syn_var),
           hh1rural) %>%
    sample_n(size = n(), replace = TRUE) %>%
    as.data.frame
    
  ## iterate over each variable one at a time
  for(temp_var in 1:nrow(lasi_predictor_matrix)){
    ## variable to model/synthesize
    temp_var_name = rownames(lasi_predictor_matrix)[temp_var]
    ## predictors
    pred_names = names(which(lasi_predictor_matrix[temp_var_name, ] == 1))
    
    ## keep all rows for household, person exists, and first person variables
    if(!(grepl(person_grep, temp_var_name)) | grepl('personexists', temp_var_name)){
      if(length(unique(temp_data[, temp_var_name])) > 1){
        temp_syn = syn.cart(y = temp_data[, temp_var_name],
                            x = temp_data[, pred_names, drop = FALSE],
                            xp = syn_data[, pred_names, drop = FALSE],
                            minbucket = 15)
      } else{
        temp_syn = syn.sample(y = temp_data[, temp_var_name],
                              xp = nrow(syn_data))
      }
      ## add synthetic values
      syn_data[, temp_var_name] = temp_syn$res
    } else if(grepl(person_grep, temp_var_name) & !(grepl('personexists', temp_var_name))){
      temp_person = paste('personexists_', sub(".*\\_", "", temp_var_name), sep = '')
      ## only synthesis if there are any "persons" in this slot
      if(nrow(syn_data[syn_data[, temp_person] == 1, ]) > 0){
        ## run synthesis only for records where "person exists"
        ## run sample if there is no variation in variable
        if(length(unique(na.omit(temp_data[temp_data[, temp_person] == 1, temp_var_name]))) > 1){
          temp_syn = syn.cart(y = temp_data[temp_data[, temp_person] == 1, temp_var_name],
                              x = temp_data[temp_data[, temp_person] == 1, pred_names],
                              xp = syn_data[syn_data[, temp_person] == 1, pred_names],
                              minbucket = 15) 
        } else{
          temp_syn = syn.sample(y = temp_data[temp_data[, temp_person] == 1, temp_var_name],
                                xp = sum(syn_data[, temp_person] == 1))
        }
        ## first set all synthetic values to NA to preserve correct missing pattern
        if(class(temp_syn$res) == 'factor'){
          syn_data[, temp_var_name] = factor(NA, levels = levels(temp_data[, temp_var_name]))
        } else if(class(temp_syn$res) == 'numeric'){
          syn_data[, temp_var_name] = NA_real_
        }
        ## add non-missing synthetic values
        syn_data[syn_data[, temp_person] == 1, temp_var_name] = temp_syn$res
      } else{
        if(class(temp_data[, temp_var_name]) == 'factor'){
          syn_data[, temp_var_name] = factor(NA, levels = levels(temp_data[, temp_var_name]))
        } else if(class(temp_data[, temp_var_name]) == 'numeric'){
          syn_data[, temp_var_name] = NA_real_
        }
      }
    }
  }
  return(syn_data)
}

## weight function currently hard-coded
weighted_synthesis = function(temp_data,
                              state_syn_var,
                              lasi_predictor_matrix,
                              weight_var = 'hh_baseweight',
                              weight_cutoff = 0.9,
                              max_person_size = 17){
  person_grep = paste('_', 2:max_person_size, collapse = '|', sep = '')
  
  weight_data = temp_data %>%
    select(!!sym(weight_var),
           contains('personexists_'))
  
  ## create object to hold the new values
  syn_data = temp_data %>%
    select(-!!sym(weight_var)) %>%
  ## first take random sample of households within state and rurality 
  ## for distribution of hhsize and personexistence
  group_by(!!sym(state_syn_var),
           hh1rural) %>%
    sample_n(size = n(), replace = TRUE) %>%
    as.data.frame
  
  ## iterate over each variable one at a time
  for(temp_var in 1:nrow(lasi_predictor_matrix)){
    ## variable to model/synthesize
    temp_var_name = rownames(lasi_predictor_matrix)[temp_var]
    ## predictors
    pred_names = names(which(lasi_predictor_matrix[temp_var_name,] == 1))
    
    ## keep all rows for household, person exists, and first person variables
    if(!(grepl(person_grep, temp_var_name)) | grepl('personexists', temp_var_name)){
      ## get weights
      syn_weights = weight_data %>% 
        mutate(case_weight = case_when(is.na(!!sym(weight_var)) ~ 1,
                                       !!sym(weight_var) > weight_cutoff ~ 0,
                                       !!sym(weight_var) <= weight_cutoff ~ (1 - (!!sym(weight_var)) ^ 2))) %>%
        pull(case_weight)
      
      if(length(unique(temp_data[, temp_var_name])) > 1 & sum(syn_weights > 0) > 1){
        temp_syn = syn.cart(y = temp_data[, temp_var_name],
                            x = temp_data[, pred_names, drop = FALSE],
                            xp = syn_data[, pred_names, drop = FALSE],
                            weights = syn_weights,
                            minbucket = 15)
      } else{
        temp_syn = syn.sample(y = temp_data[, temp_var_name],
                              xp = nrow(syn_data),
                              prob = syn_weights)
      }
      ## add synthetic values
      syn_data[, temp_var_name] = temp_syn$res
    } else if(grepl(person_grep, temp_var_name) & !(grepl('personexists', temp_var_name))){
      temp_person = paste('personexists_', sub(".*\\_", "", temp_var_name), sep = '')
      ## get weights
      syn_weights = weight_data %>% 
        filter(!!sym(temp_person) == 1) %>%
        mutate(case_weight = case_when(is.na(!!sym(weight_var)) ~ 1,
                                       !!sym(weight_var) > weight_cutoff ~ 0,
                                       !!sym(weight_var) <= weight_cutoff ~ (1 - (!!sym(weight_var)) ^ 2))) %>%
        pull(case_weight)
      
      ## only synthesis if there are any "persons" in this slot
      if(nrow(syn_data[syn_data[, temp_person] == 1, ]) > 0){
        ## run synthesis only for records where "person exists"
        ## run sample if there is no variation in variable
        if(length(unique(na.omit(temp_data[temp_data[, temp_person] == 1, temp_var_name]))) > 1 & sum(syn_weights > 0) > 1){
          temp_syn = syn.cart(y = temp_data[temp_data[, temp_person] == 1, temp_var_name],
                              x = temp_data[temp_data[, temp_person] == 1, pred_names],
                              xp = syn_data[syn_data[, temp_person] == 1, pred_names],
                              weights = syn_weights,
                              minbucket = 15) 
        } else{
          temp_syn = syn.sample(y = temp_data[temp_data[, temp_person] == 1, temp_var_name],
                                xp = sum(syn_data[, temp_person] == 1),
                                prob = syn_weights)
        }
        ## first set all synthetic values to NA to preserve correct missing pattern
        if(class(temp_syn$res) == 'factor'){
          syn_data[, temp_var_name] = factor(NA, levels = levels(temp_data[, temp_var_name]))
        } else if(class(temp_syn$res) == 'numeric'){
          syn_data[, temp_var_name] = NA_real_
        }
        ## add non-missing synthetic values
        syn_data[syn_data[, temp_person] == 1, temp_var_name] = temp_syn$res
      } else{
        if(class(temp_data[, temp_var_name]) == 'factor'){
          syn_data[, temp_var_name] = factor(NA, levels = levels(temp_data[, temp_var_name]))
        } else if(class(temp_data[, temp_var_name]) == 'numeric'){
          syn_data[, temp_var_name] = NA_real_
        }
      }
    }
  }
  return(syn_data)
}

## again some stuff is hard-coded
synth_region = function(m = 1,
                        input_data,
                        random_seed = NULL,
                        lasi_predictor_matrix,
                        state_syn_var,
                        weight_var = 'hh_baseweight',
                        weight_cutoff = 0.9,
                        synth_function = 'unweighted_synthesis',
                        max_person_size = 17){
  
  if(!is.null(random_seed)){
    set.seed(random_seed) 
  }
  synth_regions = unique(input_data$hh1state_region)
  
  ## subset data to region and columns for synthesis
  if(synth_function == 'weighted_synthesis'){
    region_data = input_data %>%
      ## ensures order is right
      select(hh1state_region,
             !!sym(weight_var),
             all_of(colnames(lasi_predictor_matrix))) %>%
      mutate_if(is.character, as.factor) %>%
      mutate_if(is.ordered, factor, ordered = FALSE) %>%
      ## split by region
      group_by(hh1state_region) %>% 
      group_split(.keep = FALSE) %>%
      lapply(., as.data.frame) 
    ## iterate over state regions, synthesizing separately
    cluster_syn = lapply(region_data,
                         get(synth_function),
                         state_syn_var = state_syn_var,
                         lasi_predictor_matrix = lasi_predictor_matrix,
                         weight_var = weight_var,
                         weight_cutoff = weight_cutoff,
                         max_person_size = max_person_size)
  } else{
    region_data = input_data %>%
      ## ensures order is right
      select(hh1state_region,
             all_of(colnames(lasi_predictor_matrix))) %>%
      mutate_if(is.character, as.factor) %>%
      mutate_if(is.ordered, factor, ordered = FALSE) %>%
      ## split by region
      group_by(hh1state_region) %>% 
      group_split(.keep = FALSE) %>%
      lapply(., as.data.frame) 
    ## iterate over state regions, synthesizing separately
    cluster_syn = lapply(region_data,
                         get(synth_function),
                         state_syn_var = state_syn_var,
                         lasi_predictor_matrix = lasi_predictor_matrix,
                         max_person_size = max_person_size)
  }
  
  ## combine different state regions
  syn_combined = bind_rows(cluster_syn) %>% 
    tibble
  
  cat('synthesis done \n')
  
  return(syn_combined)
}


syn_wrapper = function(input_df, 
                       m = 1,
                       #random_seed = NULL, 
                       lasi_predictor_matrix,
                       max_person_size = 17,
                       state_syn_var,
                       weight_var = 'hh_baseweight',
                       weight_cutoff = 0.9,
                       synth_function = 'unweighted_synthesis'){
  lapply(1:m,
         synth_region,
         input_data = input_df,
         lasi_predictor_matrix = lasi_predictor_matrix,
         max_person_size = max_person_size,
         weight_var = weight_var,
         weight_cutoff = weight_cutoff,
         state_syn_var = state_syn_var,
         synth_function = synth_function)
}


## edit hh, recompute weights
cleanup_hh = function(input_data,
                      conf_data,
                      state_syn_var,
                      grouped_states,
                      max_person_size = 17){
  
  temp = input_data %>% 
    select(contains('ragender')) %>% 
    is.na %>% 
    rowSums
  input_data$hhsize = max_person_size - temp
  
  if(state_syn_var == 'hh1state_grouped'){
    ## sample states that were grouped from empirical distribution
    input_data$hh1state = as.character(input_data$hh1state_grouped)
    for(state_grouping in grouped_states){
      emp_dist = prop.table(table(conf_data$hh1state[conf_data$hh1state_grouped == state_grouping]))
      sample_states = sample(as.numeric(names(emp_dist)), 
                             sum(input_data$hh1state_grouped == state_grouping, na.rm = TRUE), 
                             prob = emp_dist,
                             replace = TRUE) 
      input_data$hh1state[input_data$hh1state_grouped == state_grouping] = sample_states
    } 
  }
  
  hh_synthesized_character = input_data %>%
    select(any_of(colnames(conf_data))) %>%
    arrange(hh1state, 
            desc(hh1rural)) %>%
    ## add synthetic hhid back in
    mutate(syn_hhid = 1:nrow(input_data), 
           .before = hh1state) %>%
    mutate_at(vars(-syn_hhid,
                   -hh1hhres,
                   -hh1wthh,
                   -contains('r1agey'),
                   contains('r1agey_bin'),
                   -contains('raedyrs'),
                   -contains('r1iearn'),
                   -contains('r1wtresp')),
              as.character)
  
  ## compute new weights
  new_weights = hh_synthesized_character %>%
    select(syn_hhid,
           hh1state,
           hh1rural,
           hhhgender,
           hh1wthh) %>% 
    left_join(hh_synthesized_character %>%
                group_by(hh1state,
                         hh1rural,
                         hhhgender) %>%
                reframe(total_n = n(),
                        syn_total_weight = sum(hh1wthh)) %>%
                left_join(conf_data %>%
                            group_by(hh1state,
                                     hh1rural,
                                     hhhgender) %>%
                            reframe(conf_total_weight = sum(hh1wthh))) %>%
                mutate(weight_ratio = conf_total_weight / syn_total_weight)) %>%
    mutate(hh1wthh_new = case_when(!is.na(weight_ratio) ~ weight_ratio * hh1wthh,
                                   is.na(weight_ratio) ~ hh1wthh)) %>%
    select(syn_hhid,
           hh1state,
           hh1wthh_new)
  
  ## trim and redistribute (not within each state)
  temp_weight = new_weights$hh1wthh_new
  upper_bound = quantile(temp_weight, 0.99)
  while(sum(temp_weight > upper_bound) > 0){
    temp_weight[temp_weight > upper_bound] = upper_bound
    lost_weight = sum(new_weights$hh1wthh_new - temp_weight)
    temp_weight[temp_weight != upper_bound] = temp_weight[temp_weight != upper_bound] + 
      lost_weight / length(temp_weight[temp_weight != upper_bound])
  }
  new_weights$hh1wthh = temp_weight

  ## rescale to 1
  new_weights$hh1wthh_new = new_weights$hh1wthh_new * 
    (nrow(hh_synthesized_character) / sum(new_weights$hh1wthh_new))
  
  ## add new weights
  hh_synthesized_character = hh_synthesized_character %>%
    ## drop old weights
    select(-hh1wthh) %>%
    left_join(new_weights %>%
                select(-hh1wthh_new))
  
  return(hh_synthesized_character)
}


## edit indiv, recompute weights
cleanup_indiv = function(hh_synth_data,
                         conf_indiv_data){
  ## first convert to row per indiv and rename some columns
  indiv_synthesized_temp = hh_synth_data %>%
    select(-contains('personexists'),
           -contains('hhsize')) %>%
    pivot_longer(-c(syn_hhid,
                    contains('hh1state'),
                    hh1rural,
                    hh1hhres,
                    hhhgender,
                    hh1wthh),
                 names_to = c('.value',
                              'num'),
                 names_pattern = '([a-zA-Z]+)_(\\d+)') %>%
    ## to match lasi extract
    rename(raeduc_l = l,
           #r1agey = agey,
           r1agey_bin = bin,
           r1shlt = shlt,
           r1adlfive = adlfive,
           r1work = work,
           r1iearn = iearn,
           r1wtresp = wtresp) %>%
    ## add couple indicators
    mutate(couple = ceiling(num / 2))
  
  ## step two
  indiv_synthesized_character = indiv_synthesized_temp %>%
    left_join(indiv_synthesized_temp %>%
                group_by(syn_hhid, 
                         couple) %>%
                reframe(number = 1) %>%
                ## add couple id
                mutate(h1coupid = cumsum(number)),
              by = c('syn_hhid', 
                     'couple')) %>%
    select(-num,
           -couple,
           -number) %>%
    mutate(prim_key = 1:nrow(indiv_synthesized_temp), 
           .before = h1coupid,
           hhid = syn_hhid) %>%
    mutate(raeducl = case_when(raeduc_l %in% c(0, 1, 2) ~ '1',
                               raeduc_l %in% c(3, 4, 5, 6) ~ '2',
                               raeduc_l %in% c(7, 8, 9) ~ '3')) %>%
    mutate_at(vars(hh1hhres,
                   hh1wthh,
                   #r1agey,
                   raedyrs,
                   r1iearn,
                   r1wtresp),
              as.numeric) %>%
    select(any_of(colnames(conf_indiv_data))) %>%
    na.omit()
  
  
  ## now add weights
  ## compute new weights
  indiv_weights = conf_indiv_data %>%
    mutate(age_weight_bins = case_when(r1agey_bin %in% c('(17,44]', '(44,49]') ~ 'less50',
                                       r1agey_bin %in% c('(49,54]', '(54,59]') ~ '50_59',
                                       r1agey_bin %in% c('(59,64]', '(64,69]') ~ '60_69',
                                       r1agey_bin %in% c('(69,74]', '(74,79]', '(79,120]') ~ '70up')) %>%
    group_by(hh1state,
             ragender,
             age_weight_bins) %>%
    reframe(conf_total_weight = sum(r1wtresp)) %>%
    pivot_wider(names_from = c(ragender, age_weight_bins), 
                values_from = conf_total_weight) %>%
    left_join(conf_indiv_data %>%
                mutate(hh1rural = as.factor(paste('rural', hh1rural, sep = '_'))) %>%
                group_by(hh1state,
                         hh1rural) %>%
                reframe(conf_total_weight = sum(r1wtresp)) %>%
                pivot_wider(names_from = c(hh1rural), 
                            values_from = conf_total_weight)) %>%
    left_join(conf_indiv_data %>%
                mutate(educ_weight_bins = case_when(raeduc_l %in% c('0') ~ 'No school',
                                                    raeduc_l %in% c('1', '2') ~ 'Primary or less',
                                                    raeduc_l %in% c('3') ~ 'Middle',
                                                    raeduc_l %in% c('4', '5', '6', '7', '8', '9') ~ 'Secondary or more')) %>%
                group_by(hh1state,
                         ragender,
                         educ_weight_bins) %>%
                reframe(conf_total_weight = sum(r1wtresp)) %>%
                pivot_wider(names_from = c(ragender, educ_weight_bins), 
                            values_from = conf_total_weight))
  
  ## now get variables to weight from synthetic data
  synth_counts = indiv_synthesized_character %>%
    mutate(age_weight_bins = case_when(r1agey_bin %in% c('(17,44]', '(44,49]') ~ 'less50',
                                       r1agey_bin %in% c('(49,54]', '(54,59]') ~ '50_59',
                                       r1agey_bin %in% c('(59,64]', '(64,69]') ~ '60_69',
                                       r1agey_bin %in% c('(69,74]', '(74,79]', '(79,120]') ~ '70up'),
           gender_age_bin = interaction(ragender, 
                                        age_weight_bins, 
                                        sep = '_')) %>%
    select(prim_key, 
           hh1state,
           gender_age_bin) %>%
    left_join(indiv_synthesized_character %>%
                mutate(hh1rural = as.factor(paste('rural', hh1rural, sep = '_'))) %>%
                select(prim_key,
                       hh1state,
                       hh1rural)) %>%
    left_join(indiv_synthesized_character %>%
                mutate(educ_weight_bins = case_when(raeduc_l %in% c('0') ~ 'No school',
                                                    raeduc_l %in% c('1', '2') ~ 'Primary or less',
                                                    raeduc_l %in% c('3') ~ 'Middle',
                                                    raeduc_l %in% c('4', '5', '6', '7', '8', '9') ~ 'Secondary or more'),
                       gender_education_bin = interaction(ragender, 
                                                          educ_weight_bins, 
                                                          sep = '_')) %>%
                select(prim_key,
                       hh1state,
                       gender_education_bin))
  
  
  ## re-weight by state
  out_weights = vector('list', length(unique(indiv_synthesized_character$hh1state)))
  names(out_weights) = unique(indiv_synthesized_character$hh1state)
  for(temp_state in unique(indiv_synthesized_character$hh1state)){
    temp_df = synth_counts %>%
      filter(hh1state == temp_state) %>%
      select(-prim_key, 
             -hh1state) %>%
      as.data.frame()
    
    temp_totals = indiv_weights %>%
      filter(hh1state == temp_state)
    temp_target = list('gender_age_bin' = temp_totals[, grepl(paste(unique(temp_df$gender_age_bin), collapse = '|'), 
                                                              colnames(temp_totals))] %>% unlist,
                       'hh1rural' = temp_totals[, grepl(paste(unique(temp_df$hh1rural), collapse = '|'), 
                                                        colnames(temp_totals))] %>% unlist,
                       'gender_education_bin' = temp_totals[, grepl(paste(unique(temp_df$gender_education_bin), collapse = '|'), 
                                                                    colnames(temp_totals))] %>% unlist)
    test = anesrake::anesrake(temp_target,
                              temp_df,
                              caseid = synth_counts %>%
                                filter(hh1state == temp_state) %>%
                                pull(prim_key),
                              weightvec = indiv_synthesized_character %>%
                                filter(hh1state == temp_state) %>%
                                pull(r1wtresp),
                              pctlim = 0.01)
    
    out_weights[[temp_state]] = tibble(prim_key = test$caseid, 
                                       r1wtresp_new = test$weightvec)
    ## rescale to right total
    out_weights[[temp_state]]$r1wtresp_new = out_weights[[temp_state]]$r1wtresp_new * 
      (sum(temp_target$hh1rural) / sum(out_weights[[temp_state]]$r1wtresp_new))
    ## trim and redistribute
    temp_weight = out_weights[[temp_state]]$r1wtresp_new
    upper_bound = quantile(temp_weight, 0.99)
    while(sum(temp_weight > upper_bound) > 0){
      temp_weight[temp_weight > upper_bound] = upper_bound
      lost_weight = sum(out_weights[[temp_state]]$r1wtresp_new - temp_weight)
      temp_weight[temp_weight != upper_bound] = temp_weight[temp_weight != upper_bound] + 
        lost_weight / length(temp_weight[temp_weight != upper_bound])
    }
    out_weights[[temp_state]]$r1wtresp_new = temp_weight
    cat(temp_state, '\n')
  }
  
  ## compute new weights
  new_weights = bind_rows(out_weights) %>%
    arrange(prim_key)
  ## rescale to mean 1
  new_weights$r1wtresp = new_weights$r1wtresp_new * (nrow(indiv_synthesized_character) / sum(new_weights$r1wtresp_new))
  
  ## add
  indiv_synthesized_character = indiv_synthesized_character %>%
    ## drop old weights
    select(-r1wtresp) %>%
    left_join(new_weights %>%
                select(-r1wtresp_new))
  
  return(indiv_synthesized_character)
}





