compute_single_epa_transient <- function(
  abos_epa_index, #index as used in Heise, 2007
  coeff_matrix, #the coeffiencients for the equation
  coeff_selection_list, #A list of which coefficients to use      
  epa_codes  #fundamentals for abos 
){
  coeffs <- coeff_matrix[abos_epa_index,]
  ##assume for now that the first elt of coeff_selection_list is the constant
  transient = coeffs[1]
  for(i in seq(2,length(coeffs))){
    transient <- transient + coeffs[i] * prod(epa_codes[coeff_selection_list[[i]]] )
  }
  return(transient)
}

deflection_computation <- function(coeff_matrix,coeff_selection_list,epa_codes){
  return(
    sum((unlist(lapply(1:9, compute_single_epa_transient, coeff_matrix,coeff_selection_list,epa_codes))-epa_codes)^2)
  )
}


get_epa_for_single_entity <- function(name, data){
  r <- data[term == name]
  return(c(r$e,r$p,r$a))
}

get_epa_for_abo_combination <- function(input_line, data){
  return(c(
    get_epa_for_single_entity(input_line$actor,data),
    get_epa_for_single_entity(input_line$behavior,data),
    get_epa_for_single_entity(input_line$object,data)
  ))
}

get_deflection_for_traits <- function(row){
  actor_epa <- c(get_epa_for_single_entity(row$actor, epa_data),get_epa_for_single_entity(row$choice, epa_data))
  fundamental <- c(compute_single_epa_transient(1,modifier_coeff_matrix,modifier_coeff_list, actor_epa),
                   compute_single_epa_transient(2,modifier_coeff_matrix,modifier_coeff_list, actor_epa),
                   compute_single_epa_transient(3,modifier_coeff_matrix,modifier_coeff_list, actor_epa),
                   get_epa_for_single_entity(row$behavior, epa_data),
                   get_epa_for_single_entity(row$object, epa_data))
  return(deflection_computation(coeff_matrix,coeff_list,fundamental))
}
