# Instalar las librerías si no están instaladas
required_packages <- c("dplyr", "tidyr", "sjlabelled", "glue", "collapse")

# Instalar y cargar los paquetes si no están instalados
for (package in required_packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    }
  library(package, character.only = TRUE)
  }

rm(required_packages, package)

# Rename reg component
rename_reg <- function(data, x, vars) {
  # Loop through each variable
  for (variable in vars) {
    # Get the label of the variable
    rep <- sjlabelled::get_label(data %>% dplyr::select(!!sym(variable))) %>%
      as.character()
    
    # Replace variable names with their labels in the 'term' column of x
    x <- x %>%
      mutate(term = gsub(pattern = variable, replacement = rep, term))
    }
  return(x)
  }

### String encode factor
encode <- function(var, lab = NA_character_) {
  # Identify non-NA values and convert them to factor
  non_na_values <- !is.na(var)
  factor_var <- as.factor(var[non_na_values])
  
  # Create numeric labels for non-NA values
  numeric_labels <- integer(length(var))
  numeric_labels[non_na_values] <- as.integer(factor_var)
  
  # Create a named vector for labels
  labels <- setNames(1:length(levels(factor_var)), levels(factor_var))
  
  # Create labelled variable using haven::labelled
  labelled_var <- labelled(numeric_labels, labels, label = lab)
  
  return(labelled_var)
  }
# Definir la función report_summarise con la opción de pesos
report_summarise <- function(data, group, variable, wt = NULL) {
  group <- rlang::enquo(group)
  variable <- rlang::enquo(variable)
  wt <- rlang::enquo(wt)
  data = data
  lev = sjlabelled::get_labels(data %>% select(!!variable)) %>%
    unlist() %>%
    as.vector()
  
  if (length(lev) > 0) {
    if (rlang::quo_is_null(wt)) {
      result = data %>%
        group_by(myvar = !!variable, !!group) %>%
        tally() %>%
        group_by(!!group) %>%
        mutate(
          share = n / collapse::fsum(n),
          nlab = glue("{scales::comma(n)} [{scales::percent(x = share, accuracy = 0.01)}]")
        ) %>%
        dplyr::select(-c(n, share)) %>%
        pivot_wider(
          id_cols = myvar,
          names_from = !!group,
          values_from = nlab
        ) %>%
        mutate_if(haven::is.labelled, ~ haven::as_factor(.x)) %>%
        ## Include label variable name
        add_row(
          myvar = sjlabelled::get_label(select(data, !!variable)) %>%
            as.vector(),
          .before = 1
        ) %>%
        mutate_all(as.character)
    } else {
      result = data %>%
        group_by(myvar = !!variable, !!group) %>%
        tally(wt = !!wt, name = "n") %>%
        group_by(!!group) %>%
        mutate(
          share = n / collapse::fsum(n),
          nlab = glue("{scales::comma(n)} [{scales::percent(x = share, accuracy = 0.01)}]")
        ) %>%
        dplyr::select(-c(n, share)) %>%
        pivot_wider(
          id_cols = myvar,
          names_from = !!group,
          values_from = nlab
        ) %>%
        mutate_if(haven::is.labelled, ~ haven::as_factor(.x)) %>%
        ## Include label variable name
        add_row(
          myvar = sjlabelled::get_label(select(data, !!variable)) %>%
            as.vector(),
          .before = 1
        ) %>%
        mutate_all(as.character)
    }
  } else {
    result = data %>%
      group_by(!!group) %>%
      summarise_at(vars(!!variable), ~ collapse::fmean(.x, w = !!wt)) %>%
      ungroup() %>%
      mutate(
        myvar = sjlabelled::get_label(select(data, !!variable)) %>%
          as.vector()
      ) %>%
      pivot_wider(
        id_cols = myvar,
        names_from = !!group,
        values_from = !!variable
      ) %>%
      mutate_if(is.numeric, ~ as.character(round(.x, digits = 2)))
  }
  return(result)
}

report_continuous <- function(data, group, variable, yvar, wt = NULL, fun = mean) {
  group <- rlang::enquo(group)
  variable <- rlang::enquo(variable)
  yvar <- rlang::enquo(yvar)
  wt <- rlang::enquo(wt)
  fun <- match.fun(fun)
  data = data
  lev = sjlabelled::get_labels(data %>% select(!!variable)) %>%
    unlist() %>%
    as.vector()
  
  data = data %>%
    mutate_at(vars(!!yvar), ~ as.numeric(.x))
  
  if (length(lev) > 0) {
    if (rlang::quo_is_null(wt)) {
      result = data %>%
        group_by(myvar = !!variable, !!group) %>%
        summarise(
          result_yvar = fun(!!yvar)
          ) %>%
        ungroup() %>%
        pivot_wider(
          id_cols = myvar,
          names_from = !!group,
          values_from = result_yvar
        ) %>%
        mutate_if(haven::is.labelled, ~ haven::as_factor(.x)) %>%
        mutate_if(is.numeric, ~ as.character(round(.x, digits = 2))) %>%
        ## Include label variable name
        add_row(
          myvar = sjlabelled::get_label(select(data, !!variable)) %>%
            as.vector(),
          .before = 1
        ) %>%
        mutate_all(as.character)
    } else {
      result = data %>%
        group_by(myvar = !!variable, !!group) %>%
        summarise(
          result_yvar = fun(!!yvar, w = !!wt)
          ) %>%
        ungroup() %>%
        pivot_wider(
          id_cols = myvar,
          names_from = !!group,
          values_from = result_yvar
        ) %>%
        mutate_if(haven::is.labelled, ~ haven::as_factor(.x)) %>%
        mutate_if(is.numeric, ~ as.character(round(.x, digits = 2))) %>%
        ## Include label variable name
        add_row(
          myvar = sjlabelled::get_label(select(data, !!variable)) %>%
            as.vector(),
          .before = 1
        ) %>%
        mutate_all(as.character)
    }
  } else {
    if (rlang::quo_is_null(wt)) {
      result = data %>%
        group_by(myvar = !!variable, !!group) %>%
        summarise(
          result_yvar = fun(!!yvar)
        ) %>%
        ungroup() %>%
        pivot_wider(
          id_cols = myvar,
          names_from = !!group,
          values_from = result_yvar
        ) %>%
        mutate_if(haven::is.labelled, ~ haven::as_factor(.x)) %>%
        ## Include label variable name
        add_row(
          myvar = sjlabelled::get_label(select(data, !!variable)) %>%
            as.vector(),
          .before = 1
        ) %>%
        mutate_if(is.numeric, ~ as.character(round(.x, digits = 2))) %>%
        mutate_all(as.character)
    } else {
      result = data %>%
        group_by(myvar = !!variable, !!group) %>%
        summarise(
          result_yvar = fun(!!yvar, w = !!wt)
          ) %>%
        ungroup() %>%
        mutate(
          myvar = sjlabelled::get_label(select(data, !!variable)) %>%
            as.vector()
        ) %>%
        pivot_wider(
          id_cols = myvar,
          names_from = !!group,
          values_from = !!variable
        ) %>%
        mutate_if(is.numeric, ~ as.character(round(.x, digits = 2))) %>%
        mutate_all(as.character)
    }
  }
  return(result)
}
