output$gloss <- DT::renderDataTable({
  
  gl<-matrix(data = c(
    "Inverse Distance Weighted (IDW) Analysis", "Inverse distance weighting is a type of deterministic method for multivariate interpolation with a known scattered set of points. The assigned values to unknown points are calculated with a weighted average of the values available at the known points."), 
    ncol = 2, byrow = T)
  colnames(gl)<-c("Phrase", "Definition")
  gl<-datatable(gl, options = list(pageLength = 50, dom = 'tip', dom='t',ordering=F, paging=F), 
                rownames = FALSE,
                # caption = 'Table 2: Defined terms used in web tool.', 
                escape = FALSE)
  return(gl)
})

output$acronyms<-DT::renderDataTable({
  # ABBREVIATIONS, ACRONYMS, AND SYMBOLS
  ac<-matrix(data = c(
    # '<em>W<sub>aud</sub>(f)</em>',	'Auditory weighting function',
    'WFA',	'Weighting factor adjustments'), 
    ncol = 2, byrow = T)
  colnames(ac) = c('Abbreviation', 'Description')
  ac<-datatable(ac, options = list(pageLength = 26, dom = 'tip', dom='t',ordering=F, paging=F), 
                rownames = FALSE,
                # caption = 'Table 1: Abbreviation used in web tool.', 
                escape = FALSE)
  return(ac)
})

output$infot2 <- DT::renderDataTable({
  lr<-data.frame(c(
    "Ward, W.D., A. Glorig, and D.L. Sklar. 1959. Temporary threshold shift from octave-band noise: Application to damage-risk criteria. Journal of the Acoustical Society of America 31: 522-528.",
    "Yost, W.A. 2007. Fundamentals of Hearing: An Introduction. New York: Academic Press."
  ))
  colnames(lr)<-""
  return(lr)
})