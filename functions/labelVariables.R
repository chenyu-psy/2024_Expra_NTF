

labelConditions <- function(.data) {
  
  data_col_names <- colnames(.data)
  
  output <- .data
  
  if ("condition" %in% data_col_names) {
    output <- output %>% 
      mutate(
        condition = factor(
          condition,
          levels = c("positive", "negative", "neutral"),
          labels = c("Positive", "Negative", "Neutral")
        )
      )
  }
  
  if ("ratio" %in% data_col_names) {
    output <- output %>% 
      mutate(
        ratio = factor(
          ratio,
          levels = c("positive", "negative", "neutral", "even"),
          labels = c("Mainly Positive", "Mainly Negative", "Mainly Neutral", "Even")
        )
      )
  }
  
  if ("layout" %in% data_col_names) {
    output <- output %>% 
      mutate(
        layout = factor(
          layout,
          levels = c("separated","intermixed"),
          labels = c("Separated", "Intermixed")
        )
      )
  }
  
  if ("colorCon" %in% data_col_names) {
    output <- output %>% 
      mutate(
        colorCon = factor(
          colorCon,
          levels = c("fixed","switch","random"),
          labels = c("Consistent", "Switch", "Random")
        )
      )
  }
  
  if ("radius" %in% data_col_names) {
    output <- output %>% 
      mutate(
        radius = factor(
          radius,
          levels = c("small","medium","large"),
          labels = c("Small", "Medium", "Large")
        )
      )
  }
  
  if ('cueType' %in% data_col_names) {
    output <- output %>% 
      mutate(
        cueType = factor(
          cueType,
          levels = c("color","location"),
          labels = c("Color", "Location")
        )
      )
  }
  
  if ("benefit" %in% data_col_names) {
    output <- output %>% 
      mutate(
        benefit = factor(
          benefit,
          levels = c("PB","NB"),
          labels = c("Positive cue", "Negative cue")
        )
      )
  }
  
  return (output)
} 
