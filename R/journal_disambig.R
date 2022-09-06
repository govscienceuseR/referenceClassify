#' Disambiguate Journal Names
#'
#' Creates a new column for extended journal names based on known abbreviations from LINK AND LINK
#'
#' @param dt data frame with a column of potential journal names
#' @param column string representing the column name where potential journal names exist
#' @param remove_periods boolean for whether to remove all remaining periods (e.g., currently "^J." is replaced by "Journal of.")
#' @return data frame with a new column "journal.disam"
#'
#' @examples dt_clean <- journal_disambig()
#'
#'
#' @export

journal_disambig <- function(dt,column,remove_periods = T){

  j_index <- fread("~/Documents/Davis/R-Projects/citationClassify/data/indices/journal_abbr.csv")

  dt[[column]] <- base::trimws(dt[[column]])
  dt$journal.disam <- dt$container

  output <- which(dt$journal.disam %in% j_index$abbr)

  for(i in output){
    for(j in 1:nrow(j_index)){
      dt$journal.disam[i] <- ifelse(dt$journal.disam[i] == j_index$abbr[j],
                                    j_index$title[j], dt$journal.disam[i])
    }
  }

  # Adv[.]? should be Advances -- see Adv for some inspiration
  dt$journal.disam <- str_replace(dt$journal.disam, "Adv\\b|Advn\\b", "Advances in")
  # Agric[.]? for Agricultur
  dt$journal.disam <- str_replace(dt$journal.disam, "Agric\\b", "Agriculture")
  # Anim. = Animal
  dt$journal.disam <- str_replace(dt$journal.disam, "Anim\\b", "Animal")
  # Am J = American journal of
  dt$journal.disam <- str_replace(dt$journal.disam, "Am\\sJ\\b", "American Journal of")
  # Am = America at end
  dt$journal.disam <- str_replace(dt$journal.disam, "Am$|Amer$", "America")
  # Am = American
  dt$journal.disam <- str_replace(dt$journal.disam, "Am\\b|Amer\\b", "American")
  # Ann. is Annals of -- removing because impossible to discern between Annual, which can also be Ann.
  #dt$journal <- str_replace(dt$journal, "Ann\\b", "Annals")
  # Annu. is annual
  dt$journal.disam <- str_replace(dt$journal.disam, "Annu\\b", "Annual")
  # Atmos.is Atmospheric
  dt$journal.disam <- str_replace(dt$journal.disam, "Atmos\\b", "Atmospheric")
  # Assoc.= Association
  dt$journal.disam <- str_replace(dt$journal.disam, "Assoc\\b", "Association")
  # Appl. is Applied
  dt$journal.disam <- str_replace(dt$journal.disam, "Appl\\b", "Applied")
  # Biol. = Biology
  dt$journal.disam <- str_replace(dt$journal.disam, "Biol\\b", "Biology")
  #Behav = Behavior
  dt$journal.disam <- str_replace(dt$journal.disam, "Behav\\b", "Behavior")
  #Bull = Bulletin at end
  dt$journal.disam <- str_replace(dt$journal.disam, "Bull$", "Bulletin")
  #Bull = Bulletin
  dt$journal.disam <- str_replace(dt$journal.disam, "Bull\\b", "Bulletin of")
  df$journal.disam <- str_replace(df$journal.disam, "Bull\\b", "Bulletin of")
  # Can. = Canadian (keys on period to ensure note just word "can")
  df$journal.disam <- str_replace(df$journal.disam, "Can\\.", "Canadian")
  # Cem Bas Mat
  dt$journal.disam <- str_replace(dt$journal.disam, "Cem\\sBas\\sMat[a-z]?\\b", "Cement-Based Materials")
  # Chem = Chemistry
  df$journal.disam <- str_replace(df$journal.disam, "Chem\\b", "Chemistry")
  # Civ = Civil
  dt$journal.disam <- str_replace(dt$journal.disam, "Civ\\b", "Civil")
  # Climatol = Climatology
  dt$journal.disam <- str_replace(dt$journal.disam, "Climatol\\b", "Climatology")
  # Conf = Consference
  dt$journal.disam <- str_replace(dt$journal.disam, "Conf\\b", "Conference")
  # Conserv = Conservation
  dt$journal.disam <- str_replace(dt$journal.disam, "Conserv\\b", "Conservation")
  # Comput = Computing
  dt$journal.disam <- str_replace(dt$journal.disam, "Comput\\b", "Computing")
  # Constr = Constructions
  dt$journal.disam <- str_replace(dt$journal.disam, "Constr\\b", "Construction")
  # Corro = Corrosion
  dt$journal.disam <- str_replace(dt$journal.disam, "Corros?\\b", "Corrosion")
  # Croat == Croation
  dt$journal.disam <- str_replace(dt$journal.disam, "Croat?\\b", "Croatian")
  # Earthq.= Earthquake
  dt$journal.disam <- str_replace(dt$journal.disam, "Earthq\\b", "Earthquake")
  # Ecol[.]? should be Ecology
  dt$journal.disam <- str_replace(dt$journal.disam, "Ecol\\b", "Ecology")
  # Eng[.]? should be Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "Eng\\b", "Engineering")
  # Ent[.]? should be Entomology
  dt$journal.disam <- str_replace(dt$journal.disam, "Ent\\b|Entomol\\b", "Entomology")
  # Environ. = Environment at end
  dt$journal.disam <- str_replace(dt$journal.disam, "Environ$|Envt$|Envir$", "Environment")
  # Environ. = Environmtnal
  dt$journal.disam <- str_replace(dt$journal.disam, "Environ\\b|Env\\b|Envir\\b", "Environmental")
  # Ergon Ergonomics
  dt$journal.disam <- str_replace(dt$journal.disam, "Ergon\\b", "Ergonomics")
  # Epidemiol
  dt$journal.disam <- str_replace(dt$journal.disam, "Epidemiol\\b", "Epidemiology")
  # European Euro
  dt$journal.disam <- str_replace(dt$journal.disam, "Euro?\\b", "European")
  # Genet
  dt$journal.disam <- str_replace(dt$journal.disam, "Genet\\b", "Genetics")
  # For. = Forest (note this keys on period to ensure not just word "for")
  df$journal.disam <- str_replace(df$journal.disam, "For\\.", "Forest")
  # Geophys
  dt$journal.disam <- str_replace(dt$journal.disam, "Geophys\\b", "Geophysics")
  # Geol. = Geology
  dt$journal.disam <- str_replace(dt$journal.disam, "Geol\\b", "Geology")
  # Geoenv Geoenvi.
  dt$journal.disam <- str_replace(dt$journal.disam, "Geo[Ee]nvi?r?o?n?\\b", "Geoenvironmental")
  # Geotech
  dt$journal.disam <- str_replace(dt$journal.disam, "Geotech\\b", "Geotechnical")
  # Hous
  dt$journal.disam <- str_replace(dt$journal.disam, "Hous\\b", "Housing")
  # Hydrogeol
  dt$journal.disam <- str_replace(dt$journal.disam, "Hydrogeol\\b", "Hydrogeology")
  # Hydrol
  dt$journal.disam <- str_replace(dt$journal.disam, "Hydrol\\b", "Hydrology")
  # Ieee
  dt$journal.disam <- str_replace(dt$journal.disam, "^Ieee\\b", "IEEE")
  # Int = International
  dt$journal.disam <- str_replace(dt$journal.disam, "Int\\b", "International")
  # J[.]? should be journal, if at end
  dt$journal.disam <- str_replace(dt$journal.disam, "\\bJ$", "Journal")
  # J[.]? should be journal of, if at start
  df$journal.disam <- str_replace(df$journal.disam, "\\bJ\\,?\\b", "Journal of")
  # Manage, Man = Management
  df$journal.disam <- str_replace(df$journal.disam, "Man\\b|Manage\\b", "Management")
  # Mater = Materials
  dt$journal.disam <- str_replace(dt$journal.disam, "Mat\\b|Mater\\b", "Materials")
  # Mech = Mechanical
  dt$journal.disam <- str_replace(dt$journal.disam, "Mech\\b", "Mechanical")
  # Ornith = Ornithology
  dt$journal.disam <- str_replace(dt$journal.disam, "Ornith\\b", "Ornithology")
  # Psychol = Psychology
  dt$journal.disam <- str_replace(dt$journal.disam, "Psychol\\b", "Psychology")
  # Sci = Science
  dt$journal.disam <- str_replace(dt$journal.disam, "Sci\\b", "Science")
  # Seis.= Siesmic
  dt$journal.disam <- str_replace(dt$journal.disam, "Seism?\\b", "Seismological")
  # Soc = Society
  dt$journal.disam <- str_replace(dt$journal.disam, "Soc\\b", "Society")
  # Sociol = Sociology
  dt$journal.disam <- str_replace(dt$journal.disam, "Sociol\\b", "Sociology")
  # Softw = Software
  dt$journal.disam <- str_replace(dt$journal.disam, "Softw\\b", "Software")
  # Stud
  dt$journal.disam <- str_replace(dt$journal.disam, "Stud\\b", "Studies")
  # Struct = Structural
  dt$journal.disam <- str_replace(dt$journal.disam, "Struct\\b", "Structural")
  # Resour. = Resources
  dt$journal.disam <- str_replace(dt$journal.disam, "Resour\\b", "Resources")
  # Res. = Research
  dt$journal.disam <- str_replace(dt$journal.disam, "Res\\b", "Research")
  # Rev. = Review at end
  dt$journal.disam <- str_replace(dt$journal.disam, "Rev$", "Review")
  # Rev. = Review of
  dt$journal.disam <- str_replace(dt$journal.disam, "Rev\\b", "Review of")
  # Zool = Zoology
  dt$journal.disam <- str_replace(dt$journal.disam, "Zool\\b", "Zoology")

  # Should be "Accident Analysis and Prevention"
  pattern <- c("^Accid\\sAnal$", "^Accid\\sAnal\\sPrev$", "^Accident\\sAnal\\sPrev[a-z]*", "^AccidAnalPrev$", "^Accident\\sAnaly[Ss][Ii][Ss]\\sand\\sPrevention.*$", "^Spatial Patterns Accident Analysis and Prevention$")
  pattern <- paste(pattern, collapse = "|")
  dt$journal.disam <- str_replace(dt$journal.disam, pattern, "Accident Analysis and Prevention")

  # Should be ACI Structural Journal
  pattern <- c("^ACI\\sStructural\\sJournal\\sv$", "^ACI\\sStructural\\sJournal\\sMarchApril$", "^ACI\\sStructural\\sJournal\\sSP$", "ACI\\sStructural\\sJournal\\sTitle$", "^ACI\\sStructures\\sJournal$", "^American\\sConcrete\\sInstitute\\sACI\\sStructural\\sJournal$")
  pattern <- paste(pattern, collapse = "|")
  dt$journal.disam <- str_replace(dt$journal.disam, pattern, "ACI Structural Journal")

  # Shoudl be "ACI Materials Journal"
  pattern <- c("ACI Material Journal")
  pattern <- paste(pattern, collapse = "|")
  dt$journal.disam <- str_replace(dt$journal.disam, pattern, "ACI Materials Journal")

  # Should be "Concrete International"
  pattern <- c("^ACI Concrete International$", "^ACI Concrete Journal$", "^Concr Int$", "^Concrete International\\: Design and Construction$", "^Journal of American Concr Inst Proceedings$")
  pattern <- paste(pattern, collapse = "|")
  dt$journal.disam <- str_replace(dt$journal.disam, pattern, "Concrete International")

  # Should just be ACI
  pattern <- c("^ACI SP$", "^ACI Special Publication$", "^ACI Monograph$", "^ACI Journal Proceedings$", "^ACI Journal$", "^ACIJournal$", "^ACI Committee$")
  pattern <- paste(pattern, collapse = "|")
  dt$journal.disam <- str_replace(dt$journal.disam, pattern, "ACI")

  # Anything that starts with AHMCT Should be AHMCT Report
  dt$journal.disam <- str_replace(dt$journal.disam, "^AHMCT.*", "AHMCT Report")

  # Should be Engineering Journal
  dt$journal.disam <- str_replace(dt$journal.disam, "^ASHI Engineering Journal$", "Engineering Journal")

  # American Journal of Preventive Medicine
  dt$journal.disam <- str_replace(dt$journal.disam, "^American Journal of Prev.*", "American Journal of Preventive Medicine")

  # Remove ASCE or ASME before Journal or AISC
  dt$journal.disam <- str_remove(dt$journal.disam, "^ASCE\\s(?=J)|^ASME\\s(?=J)|\\sASCE$|\\sAISC$")

  # Journal of Geotechnical Engineering should add in envt
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Geotechnical Engineering$", "Journal of Geotechnical and Geoenvironmental Engineering")

  # BMJ
  dt$journal.disam <- str_replace(dt$journal.disam, "^Bmj$|^BMJournal$|^Systematic Review Bmj$|^Systematic Review British Medical Journal$", "BMJ Open")

  # Bulletin of the Seismological Society of America
  pattern <- c("^Bulletin of Seismological  Society America$", "^Bulletin of Seismological Society America", "^Bulletin of Seismological Society American Ti$", "^Bulletin of Seismological Society of America$", "^Bulletin of the Seismological Soceity of America$", "^Bulletin of the Seismological Society of American$")
  pattern <- paste(pattern, collapse = "|")
  dt$journal.disam <- str_replace(dt$journal.disam, pattern, "Bulletin of the Seismological Society of America")

  #Bulletin of Earthquake Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Bulletin of Earthquake Engineering DOI.*$", "Bulletin of Earthquake Engineering")

  #Bulletin of Engineering Geology and the Environment
  dt$journal.disam <- str_replace(dt$journal.disam, "^Bulletin of Engineering Geology Environment$", "Bulletin of Engineering Geology and the Environment")

  #Can Journal = Canadian Journal
  dt$journal.disam <- str_replace(dt$journal.disam, "^Can\\sJournal", "Canadian Journal")
  dt$journal.disam <- str_replace(dt$journal.disam, "^Canadian Journal of For Res$", "Canadian Journal of Forest Research")
  dt$journal.disam <- str_replace(dt$journal.disam, "^Canandian Journal of Public Health	$", "Canadian Journal of Public Health	")

  # Canadian Geotechnical Journal
  dt$journal.disam <- str_replace(dt$journal.disam, "^Can Geotechnical Journal$|^Can Geotechnical Journal of  Ottawa$|^Canadian Geotechnical Journal Journal$", "Canadian Geotechnical Journal")

  # Cement and Concrete Composites
  pattern <- c("^Cem Conc?r? Compo?s?$", "^Cement \\& Concrete Composites$", "^Cement and Concrete Composits$")
  pattern <- paste(pattern, collapse = "|")
  dt$journal.disam <- str_replace(dt$journal.disam, pattern, "Cement and Concrete Composites")

  # Cement and Concrete Research
  pattern <- c("^Cem Conc?r? Res$", "^Cement Concrete Research$")
  pattern <- paste(pattern, collapse = "|")
  dt$journal.disam <- str_replace(dt$journal.disam, pattern, "Cement and Concrete Research")

  # Climatic Change
  dt$journal.disam <- str_replace(dt$journal.disam, "^Climactic\\sChange$|^Climate\\sChange$", "Climatic Change")

  # Composites Part B: Engineering
  pattern <- c("^Composites\\: Part B", "Composites B$")
  pattern <- paste(pattern, collapse = "|")
  dt$journal.disam <- str_replace(dt$journal.disam, pattern, "Composites Part B\\: Engineering")

  # Composites Part A: Applied Science and Manufacturing
  dt$journal.disam <- str_replace(dt$journal.disam, "^Composites: Part A$", "Composites Part A: Applied Science and Manufacturing")

  # Comptes Rendus Geoscience
  dt$journal.disam <- str_replace(dt$journal.disam, "^Comptes Rendus Geoscience$", "Comptes Rendus  Geoscience
")

  # Computational Materials Science
  dt$journal.disam <- str_replace(dt$journal.disam, "^Computational Material Science$", "Computational Materials Science")

  # Remove Elsevier Ltd
  dt$journal.disam <- str_remove(dt$journal.disam, "\\sElsevier Ltd$")

  # Conservation Biology Pp
  dt$journal.disam <- str_replace(dt$journal.disam, "^Conservation Biology Pp$", "Conservation Biology")

  # Construction and Building Materials
  c("^Const Build Materials$", "^Construct Build Materials$", "^Construction and Building Material$", "^Construction Bldg Materials$")
  pattern <- paste(pattern, collapse = "|")
  dt$journal.disam <- str_replace(dt$journal.disam, pattern, "Construction and Building Materials")

  # Corro Review
  dt$journal.disam <- str_replace(dt$journal.disam, "Corrosion Review", "Corrosion Reviews")

  # Critical Reviews In Environmental Science and Technology
  dt$journal.disam <- str_replace(dt$journal.disam, "^Critical Reviews In Environmental Science and Toxicology$", "Critical Reviews In Environmental Science and Technology")

  #Earthquake Engineering and Structural Dynamics
  dt$journal.disam <- str_replace(dt$journal.disam, "^Earhquake Engineering Structural Dyn$|^Earthquake Engin$|^Earthquake Engineering and Structural Dynamics\\(40$|^Earthquake Engineering and Structural DynamicsVol$|^Earthquake Engineering Structural Dyn$|^Earthquake Engng Structural Dyn$|^Engineering Structural Dyn$|^Engineering Structural Dyn$", "Earthquake Engineering and Structural Dynamics")

  #Earth and Planetary Science Letters
  dt$journal.disam <- str_replace(dt$journal.disam, "^Earth Planet Science Lett$", "Earth and Planetary Science Letters")

  #Earth Surface Processes and Landforms
  dt$journal.disam <- str_replace(dt$journal.disam, "^Earth Surf Process Landforms$", "Earth Surface Processes and Landforms")

  # Ecological Applications (this is an issue from previous code)
  dt$journal.disam <- str_replace(dt$journal.disam, "^Ecology Applications$", "Ecological Applications")
  dt$journal.disam <- str_replace(dt$journal.disam, "^Ecology Econ$", "Ecological Economics")
  dt$journal.disam <- str_replace(dt$journal.disam, "^Ecology Modell$", "Ecological Modelling")
  dt$journal.disam <- str_replace(dt$journal.disam, "^Ecology Monogr$", "Ecological Monographs")
  dt$journal.disam <- str_replace(dt$journal.disam, "^Ecology Res$", "Ecological Research")
  dt$journal.disam <- str_replace(dt$journal.disam, "^Ecology Society$", "Ecology and Society")

  # Econometrica
  dt$journal.disam <- str_replace(dt$journal.disam, "^Econometrica\\: journal of the Econometric Society", "Econometrica")

  # EconPapers
  dt$journal.disam <- str_replace(dt$journal.disam, "^EconPapers$", "Economic Papers")

  # Ecotoxicology and Environmental Safety
  dt$journal.disam <- str_replace(dt$journal.disam, "^Ecotox\\/cologyand En vironmental Safety$|^Ecotoxlcology and Environmental Safety$", "Ecotoxicology and Environmental Safety")

  # Energy and Fuels
  dt$journal.disam <- str_replace(dt$journal.disam, "^Energy and Fuels$", "Energy \\& Fuels")

  # Energy Policy 33
  dt$journal.disam <- str_replace(dt$journal.disam, "^Energy Policy 33$", "Energy Policy")

  # Energy Research & Social Science
  dt$journal.disam <- str_replace(dt$journal.disam, "^Energy Research \\& Social Science$", "Energy Research and Social Science")

  # Energy Strategy Review
  dt$journal.disam <- str_replace(dt$journal.disam, "^Energy Strategy Review$", "Energy Strategy Reviews")

  # Engineering Failure Analysis
  dt$journal.disam <- str_replace(dt$journal.disam, "^Engineering Fail Anal$", "Engineering Failure Analysis")

  # Engineering Structures
  dt$journal.disam <- str_replace(dt$journal.disam, "^Engineering Structural$|^Engineering Structures.*", "Engineering Structures")

  # Environment 119 and Behavior
  dt$journal.disam <- str_replace(dt$journal.disam, "^Environment 119 and Behavior$", "Environment and Behavior")

  # Environment and Planning Part A
  dt$journal.disam <- str_replace(dt$journal.disam, "^Environment and Planning Part A$", "Environment and Planning A")

  # Environment and Planning B\\: Planning and Design
  pattern <- c("^Environment and Planning B$", "^Environment and Planning B\\: Planning and Design 25$", "^Environment and Planning\\: Part B$")
  pattern <- paste(pattern, collapse = "|")
  dt$journal.disam <- str_replace(dt$journal.disam, pattern, "Environment and Planning B\\: Planning and Design")

  # Environment and Planning C: Government and Policy
  dt$journal.disam <- str_replace(dt$journal.disam, "^Environment and Planning C\\: Government and Policy$", "Environment and Planning C\\: Government and Policy")

  # Environmental Health Perspectives
  dt$journal.disam <- str_replace(dt$journal.disam, "^Environmental Health Persp?e?c?t?$|^Environmental Health Perspective$", "Environmental Health Perspectives")

  # Environmental Modeling and Assessment
  dt$journal.disam <- str_replace(dt$journal.disam, "^Environmental Modeling and Assessment 8$", "Environmental Modeling and Assessment")

  # Environmental Research Letters
  dt$journal.disam <- str_replace(dt$journal.disam, "^Environmental Research Letter$|^Environmental Res Lett$", "Environmental Research Letters")

  # Environmental Science and Technoiogy
  dt$journal.disam <- str_replace(dt$journal.disam, "^Environmental Science and Technoiogy$|^Environmental Science and Techn?ology$|^Environmental Science Tech$|^Environmental Science Technology$", "Environmental Science & Technology")

  #Environmental Technology
  dt$journal.disam <- str_replace(dt$journal.disam, "^Environmental Technology$", "Environmental Technology (United Kingdom)")

  # Epidemiology (Cambridge
  dt$journal.disam <- str_replace(dt$journal.disam, "^Epidemiology \\(Cambridge$", "Epidemiology")

  # Estuaries Coasts
  dt$journal.disam <- str_replace(dt$journal.disam, "^Estuaries Coasts$", "Estuaries and Coasts")

  # European Journal of Operational Research
  pattern <- c("^European Journal of of Operational Research$", "^European Journal of Opera-Tional Research$", "^European Journal of Operation Research$|^European Journal of Operational$")
  pattern <- paste(pattern, collapse = "|")
  dt$journal.disam <- str_replace(dt$journal.disam, pattern, "European Journal of Operational Research")

  # European Transport
  dt$journal.disam <- str_replace(dt$journal.disam, "^European Transport$", "European Transport  Trasporti Europei")

  # Experiment Smart Materials and Structures
  dt$journal.disam <- str_replace(dt$journal.disam, "^Experiment Smart Materials and Structures$", "Smart Materials and Structures")

  # Experimental Techniques Structural Testing Series: Part
  dt$journal.disam <- str_replace(dt$journal.disam, "^Experimental Techniques Structural Testing Series: Part$", "Experimental Techniques")

  # Expert Syst Applied
  dt$journal.disam <- str_replace(dt$journal.disam, "^Expert Systems with Applications$", "Expert Syst Applied")

  # Explor Geophysics
  dt$journal.disam <- str_replace(dt$journal.disam, "^Explor Geophysics$", "Exploration Geophysics")

  # Federal register
  dt$journal.disam <- str_replace(dt$journal.disam, "^Federal Register.*", "Federal Register")

  # Forthcoming Transportation Research Record: Journal of the Transportation Research Board
  dt$journal.disam <- str_replace(dt$journal.disam, "^Forthcoming Transportation Research Record: Journal of the Transportation Research Board$|^Transportation Research Rec.*|^Metropolitan Area Transportation Research Record Journal of the Transportation Research Board$|^Research Rec.*|^The Journal of Transportation Research Board$", "Transportation Research Record")

  # Foundations and Trends R in Machine Learning
  dt$journal.disam <- str_replace(dt$journal.disam, "^Foundations and Trends R in Machine Learning$", "Foundations and Trends in Machine Learning")

  # Generation? Journal of the American Planning Association
  dt$journal.disam <- str_replace(dt$journal.disam, "^Generation\\? Journal of the American Planning Association$|^Longer View Journal of the American Planning Association$", "Journal of the American Planning Association")

  # Geophysics Journal of Int
  dt$journal.disam <- str_replace(dt$journal.disam, "^Geophysics Journal of Int.*$|^Geophysics Journal of R Astron Society$", "Geophysical Journal International")

  # Geophysical Prospecting
  dt$journal.disam <- str_replace(dt$journal.disam, "^Geophysics Prosp$", "Geophysical Prospecting")

  # Geophysical Research Letters
  dt$journal.disam <- str_replace(dt$journal.disam, "^Geophysics Res L?e?t?t?e?r?s?$", "Geophysical Research Letters")

  # Geoscience Cananda
  dt$journal.disam <- str_replace(dt$journal.disam, "^Geosciences$", "Geoscience Canada")

  # Geosynthet Int
  dt$journal.disam <- str_replace(dt$journal.disam, "^Geosynthet International$", "Geosynthetics International")

  # Geotechnical Geology Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Geotechnical Geology Engineering$", "Geotechnical and Geological Engineering")

  # Geotechnical Test Journal
  dt$journal.disam <- str_replace(dt$journal.disam, "^Geotechnical Test Journal$|^Geotechnical Testing Journal American Society for Testing and Materials$|^Geotechnical Testing Journal ASTM$|^Geotechnical Testing Journal GTJODJournal$", "Geotechnical Testing Journal")

  # Géotechnique
  dt$journal.disam <- str_replace(dt$journal.disam, "^Géotechnique$", "Geotechnique")

  # Global Ecology and Biogeography Letters
  dt$journal.disam <- str_replace(dt$journal.disam, "^Global Ecology and Biogeography Letters$", "Global Ecology and Biogeography")

  # Highway Res Rec
  dt$journal.disam <- str_replace(dt$journal.disam, "^Highway Res Rec$|^Highway Research Record", "Highway Research Record")

  # Human Factors: The Journal of the Human Factors and Ergonomics Society
  dt$journal.disam <- str_replace(dt$journal.disam, "^Human Factors: The Journal of the Human Factors and Ergonomics Society$", "Human Factors")

  # Hydrogeology Journal
  dt$journal.disam <- str_replace(dt$journal.disam, "^Hydrogeol Journal$", "Hydrogeology Journal")

  # Hydrol Process
  dt$journal.disam <- str_replace(dt$journal.disam, "^Hydrology Process$", "Hydrology Processes")

  # IEEE Conference on Intelligent Transportation System
  dt$journal.disam <- str_replace(dt$journal.disam, "^IEEE Conference on Intelligent Transportation System$|^IEEE Intelligent Transportation Systems|^IEEE ITS Conference$", "IEEE Conference on Intelligent Transportation Systems Proceedings ITSC")

  # IEEE 66th Vehicular Technology Conference (VTC
  dt$journal.disam <- str_replace(dt$journal.disam, "^IEEE \\d\\dth Vehicular Technology Conference|^IEEE \\d\\dnd Vehicular Technology Conference", "IEEE Vehicular Technology Conference")

  #IEEE Intelligent Vehicles Symposium Proceedings
  dt$journal.disam <- str_replace(dt$journal.disam, "^IEEE Intelligent Vehicles Symposium|^IEEE on Intelligent Vehicles Symposium", "IEEE Intelligent Vehicles Symposium Proceedings")

  # IEEE Journal of Select Topics Applied Earth Obs Remote Sens
  dt$journal.disam <- str_replace(dt$journal.disam, "^IEEE Journal of Select Topics Applied Earth Obs Remote Sens$", "IEEE Journal of Selected Topics in Applied Earth Observations and Remote Sensing")

  # IEEE Journal on Selected Areas in Communications/Supplement
  dt$journal.disam <- str_replace(dt$journal.disam, "^IEEE Journal on Selected Areas in Communications/Supplement$", "IEEE Journal on Selected Areas in Communications")

  # IEEE Trans Geosci Remote Sens
  dt$journal.disam <- str_replace(dt$journal.disam, "^IEEE Trans Geosci Remote Sens$|^IEEE Transact Geosci Remote Sens$|^Journal of IEEE Transact Geosci Remote Sensing$", "IEEE Transactions on Geoscience and Remote Sensing")

  # IEEE Trans Instrum Meas
  dt$journal.disam <- str_replace(dt$journal.disam, "^IEEE Trans Instrum Meas$", "IEEE Transactions on Instrumentation and Measurement")

  # IEEE Trans Intell Transp Syst
  dt$journal.disam <- str_replace(dt$journal.disam, "^IEEE Trans Intell Transp Syst$|^IEEE Trans on Intel Trans Syst?
$|^IEEE Trans on Intelligent Transportation Systems$|^IEEE Transactions in ITS$|^IEEE Transaction on Intelligent Transportation Systems$|^IEEE Transportation on Intelligent Transportation Systems$|^Intelligent Transportation Systems IEEE Transactions on$", "IEEE Transactions on Intelligent Transportation Systems")

  # IEEE Trans on Education
  dt$journal.disam <- str_replace(dt$journal.disam, "^IEEE Trans on Education$", "IEEE Transactions on Education")

  # IEEE Trans on Inform Theory
  dt$journal.disam <- str_replace(dt$journal.disam, "^IEEE Trans on Inform Theory$", "IEEE Transactions on Information Theory")

  # IEEE Trans on Signal Processing
  dt$journal.disam <- str_replace(dt$journal.disam, "^IEEE Trans on Signal Processing$", "IEEE Transactions on Signal Processing")

  # IEEE Trans on Veh Tech
  dt$journal.disam <- str_replace(dt$journal.disam, "^IEEE Trans on Veh Tech$|^IEEE Transactions on v Ehicular Technology$", "IEEE Transactions on Vehicular Technology")

  # IEEE Transactions Automatic Control to Appear
  dt$journal.disam <- str_replace(dt$journal.disam, "^IEEE Transactions Automatic Control to Appear$", "IEEE Transactions on Automatic Control")

  # IEEE Transactions on Control System Technology
  dt$journal.disam <- str_replace(dt$journal.disam, "^IEEE Transactions on Control System Technology$|^IEEE Transactions on Control Systems$|^IEEE TRANSACTIONS on CONTROL SYSTEMS TECHNOLOGY$", "IEEE Transactions on Control Systems Technology")

  # IEEE Transactions on Systems Man and Cybernetics
  dt$journal.disam <- str_replace(dt$journal.disam, "^IEEE Transactions on Systems Man and Cybernetics$", "IEEE Transactions on Systems Man and Cybernetics\\: Systems")

  # IEEE Vehicular Networking Conference (VNC) Amsterdam (the Netherlands
  dt$journal.disam <- str_replace(dt$journal.disam, "^IEEE Vehicular Networking Conference \\(VNC\\) Amsterdam \\(the Netherlands$", "IEEE Vehicular Networking Conference VNC")

  # IEEE Vehicular Technol Mag
  dt$journal.disam <- str_replace(dt$journal.disam, "^Vehicular Technol Mag$", "Vehicular Technology Magazine")

  # IEEE Wireess Communications
  dt$journal.disam <- str_replace(dt$journal.disam, "^IEEE Wireess Communications$|^IEEEE Wireless Telecommunications Symposium$", "IEEE Wireless Communications")

  # IEEE/ASME Transactions on Mechanics
  dt$journal.disam <- str_replace(dt$journal.disam, "^IEEE\\/ASME Transactions on Mechanics$|^IEEE/ASME TRANSACTIONS on MECHATRONICS$", "IEEE\\/ASME Transactions on Mechatronics")

  # IEEJ Transactions on Electrical and Electronic Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^IEEJournal of Transactions on Electric and Environmental Engineering$", "IEEJ Transactions on Electrical and Electronic Engineering")

  # IET Journal of Intelligent Transportation Systems
  dt$journal.disam <- str_replace(dt$journal.disam, "^IET Journal of Intelligent Transportation Systems$", "IET Intelligent Transportation Systems")

  #  Industrial & Engineering Chemistry Research
  dt$journal.disam <- str_replace(dt$journal.disam, "^Indust Engineering Chem$|^Industrial and Engineering Chemistry Research$", "Industrial & Engineering Chemistry Research")

  # Industrial Electronics IEEE Transactions on
  dt$journal.disam <- str_replace(dt$journal.disam, "^Industrial Electronics IEEE Transactions on$", "IEEE Transactions on Industrial Electronics")

  # Inhalation Toxicology
  dt$journal.disam <- str_replace(dt$journal.disam, "^Inhal Toxicol$", "Inhalation Toxicology")

  # Inj Prev
  dt$journal.disam <- str_replace(dt$journal.disam, "^Inj Prev$|^Injury Prevention \\[serial Online$", "Injury Prevention")

  # Internation Journal of Project Management
  dt$journal.disam <- str_replace(dt$journal.disam, "^Internation Journal of Project Management$", "International Journal of Project Management")

  # International Journal of Solids and Structures
  dt$journal.disam <- str_replace(dt$journal.disam, "^Internation Journals of Solids and Structures$", "International Journal of Solids and Structures")

  # ITE Journal (Institute of Transportation Engineers)
  pattern <- c("^Institute of Transportation Engineering Journal$", "^Institute of Transportation Engineers ITE Journal$", "^Institute of Transportation Engineers Journal$")
  pattern <- paste(pattern, collapse = "|")
  dt$journal.disam <- str_replace(dt$journal.disam, pattern, "ITE Journal \\(Institute of Transportation Engineers\\)")

  # International Archives of the Photogrammetry Remote Sensing and Spatial Information Sciences  ISPRS Archives
  dt$journal.disam <- str_replace(dt$journal.disam, "^International Archives of Photogrammetry and Remote Sensing$|^International Archives of Photogrammetry Remote Sensing and Spatial Information Sciences$", "International Archives of the Photogrammetry Remote Sensing and Spatial Information Sciences  ISPRS Archives")

  # International Journal of Autonomous and Adaptive Communications Systems
  dt$journal.disam <- str_replace(dt$journal.disam, "^International Journal of Autonomous and Adaptive Communications Systems \\(IJAACS$", "International Journal of Autonomous and Adaptive Communications Systems")

  # International Journal of Environmental Research and Public Health
  dt$journal.disam <- str_replace(dt$journal.disam, "^International Journal of Environmental Res Public Health$", "International Journal of Environmental Research and Public Health")

  # International Journal of Health Geographics
  dt$journal.disam <- str_replace(dt$journal.disam, "^International Journal of Health Geogr$|^International Journal of Health Geographic Associations Between Street Governments Office of Planning and Research$|^International Journal of Health Geography$", "International Journal of Health Geographics")

  # International Journal of Industrial Engineering : Theory Applications and Practice
  dt$journal.disam <- str_replace(dt$journal.disam, "^International Journal of Industrial Engineering$", "International Journal of Industrial Engineering \\: Theory Applications and Practice")

  # International Journal of ITS
  dt$journal.disam <- str_replace(dt$journal.disam, "^International Journal of ITS$|^International Journal of ITS Research$|^International Journal of of Intelligent Transportation Systems$", "International Journal of Intelligent Transportation Systems Research")

  # International Journal of Life Cycle Assess
  dt$journal.disam <- str_replace(dt$journal.disam, "^International Journal of Life Cycle Assess$", "International Journal of Life Cycle Assessment")

  # International Journal of Logistics: Research and Applications
  dt$journal.disam <- str_replace(dt$journal.disam, "^International Journal of Logistics\\: Research and Applications$", "International Journal of Logistics Research and Applications")

  # International Journal of of Civil Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^International Journal of of Civil Engineering$", "International Journal of Civil Engineering")

  # International Journal of Hydrogen Energy
  dt$journal.disam <- str_replace(dt$journal.disam, "^International Journal of of Hydrogen Energy$", "International Journal of Hydrogen Energy")

  # International Journal of of Steel Structures
  dt$journal.disam <- str_replace(dt$journal.disam, "^International Journal of of Steel Structures$|^International Journal of Steel Structural$|^International Journal of Steel Structure$|^International Journal of Steel Structures KSSC$", "International Journal of Steel Structures")

  # International Journal of Plant Science
  dt$journal.disam <- str_replace(dt$journal.disam, "^International Journal of Plant Science$", "International Journal of Plant Sciences")

  # International Journal of Remote Sens
  dt$journal.disam <- str_replace(dt$journal.disam, "^International Journal of Remote Sens$|^International Journal of Remote Sensing  in Review$", "International Journal of Remote Sensing")

  # International Journal of Rock Mechanics and Minings Sciences
  dt$journal.disam <- str_replace(dt$journal.disam, "^International Journal of Rock Mechanical Min Science$|^International Journal of Rock Mechanical Mining Science$|^International Journal of Rock Mechanical Mining Science Geomech Abs$|^International Journal of Rock Mechanics and Mining Sciences and Geomechanics$", "International Journal of Rock Mechanics and Minings Sciences")

  # International Journal of Sustain Transp
  dt$journal.disam <- str_replace(dt$journal.disam, "^International Journal of Sustain Transp$", "International Journal of Sustainable Transportation")

  # Intl
  dt$journal.disam <- str_replace(dt$journal.disam, "Intl\\b", "International")

  # ITE Journal (Institute of Transportation Engineers)
  dt$journal.disam <- str_replace(dt$journal.disam, "^ITE Journal.*", "ITE Journal \\(Institute of Transportation Engineers\\)")

  # Journal of Environmental and Engineering Geophysics
  dt$journal.disam <- str_replace(dt$journal.disam, "^JEEG$|^JGeotechnical and Geoenvironmental Engineering$", "Journal of Environmental and Engineering Geophysics")

  # Journal of Materials in Civil Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Jorunal of Material in Civil Engineering$", "Journal of Materials in Civil Engineering")

  # Journal
  dt$journal.disam <- str_replace(dt$journal.disam, "Jour$", "Journal")
  dt$journal.disam <- str_replace(dt$journal.disam, "Jour\\b", "Journal of")

  # Wild Mgmt
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Wild Mgmt$", "Journal of Wildlife Management")

  # Journal of Engineering Mechanics  ASCE
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal ASCE Engineering Mechanics Div$|^Theory Journal of Engineering MechanicsAsce$", "Journal of Engineering Mechanics  ASCE")

  # Journal of the Acoustical Society of America
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Acoust Society America$|^Journal of Acoustical Society of America$|^The Journal of the Acoustical Society of America$", "Journal of the Acoustical Society of America")

  # Journal of Advanced Concrete Technology
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Advances in Concr Tech$", "Journal of Advanced Concrete Technology")

  # Journal of Agricultural Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Agricult Engineering Res$|^Journal of Agricultural Engineering Research$|^Journal of Agriculture Engng Res$", "Journal of Agricultural Engineering")

  # JAMA  Journal of the American Medical Association
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of American Med Association$|^Journal of the American Medical Association$", "JAMA  Journal of the American Medical Association")

  # JAMA  Journal of the American Medical Association
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of American Plan Association$|^Journal of American Planning Association$", "Journal of the American Planning Association")

  # Journal of the American Society for Horticultural Science
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of American Society Hort Science$|^Journal of American Society Hortic Science$", "Journal of the American Society for Horticultural Science")

  # App
  dt$journal.disam <- str_replace(dt$journal.disam, "App\\b", "Applied")
  # Electrochem
  dt$journal.disam <- str_replace(dt$journal.disam, "Electrochem\\b", "Electrochemistry")

  # Journal of Applied Mechanics Transactions ASME
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Applied Mechanics$|^Journal of Applied Mechanics ASME$|^Journal of Applied MechanicsTransactions of the Asme$", "Journal of Applied Mechanics Transactions ASME")

  # Journal of Applied Meteor
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Applied Meteor.*", "Journal of Applied Meteorology and Climatology")

  # Phys
  dt$journal.disam <- str_replace(dt$journal.disam, "Phys\\b$", "Physics")
  # Sens
  dt$journal.disam <- str_replace(dt$journal.disam, "Sens\\b$", "Sensing")
  # Archaeol
  dt$journal.disam <- str_replace(dt$journal.disam, "Archaeol\\b$", "Archaeological")

  # Journal of Arid Environment
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Arid Environment.*", "Journal of Arid Environments")

  # Journal of Atmospheric and Oceanic Technology
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Atmospheric Oceanic Technol$", "Journal of Atmospheric and Oceanic Technology")

  # Journal of Bridge Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Bridge Engineeing$|^Journal of Bridge Engineering ©$", "Journal of Bridge Engineering")

  # Journal of Can Pet Technol
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Can Pet Technol$", "Journal of Canadian Petroleum Technology")

  # Journal of Central South University
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Cent South Univ$", "Journal of Central South University")

  # KSCE Journal of Civil Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Civil Engineering$", "KSCE Journal of Civil Engineering")

  # Journal of Clean Prod
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Clean Prod$", "Journal of Cleaner Production")

  # Journal of Clim
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Clim$", "Journal of Climate")

  # Journal of Clinical Epidemihttp://Wwwnhtsadotgov/Portal/Site/Nhtsa/Menuitemdfedd570f698cabbbf Ology
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Clinical Epid.*", "Journal of Clinical Epidemiology")

  # Journal of Cold Regions Engineering  ASCE
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Cold Regions Engineering$", "Journal of Cold Regions Engineering  ASCE")

  # Journal of Combinatorial Theory Series B
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Combinatorial Theory B$", "Journal of Combinatorial Theory Series B")

  # Journal of Comp Electro
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Comp Electro$", "Journal of Computational Electronics")

  # Journal of Compos Construction
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Compos Construction$|^Journal of Composite for Construction$|^StateoftheArt Review Journal of Composites for Construction$", "Journal of Composites for Construction")

  # Journal of Computers and Structures
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Computers and Structures$", "Computers and Structures")

  # Journal of Computing Civil Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Computing Civil Engineering$", "Journal of Computing in Civil Engineering")

  # Journal of Computational and Graphical Statistics
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Computational and Graphical Statistics$", "Journal of Computational and Graphical Statistics")

  # Journal of Construction Engineering and Management  ASCE
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Construction Engi$|^Journal of Construction Engineering and Management$|^Journal of Construction Engineering Manage$|^Journalof Construction Engineering and Management$", "Journal of Construction Engineering and Management  ASCE")

  # Journal of Structural Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Constructional Steel$", "Journal of Structural Engineering")

  # Journal of Contaminant Hydrology
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Conta?m? Hydrology$", "Journal of Contaminant Hydrology")

  # Journal of Dynamic Systems Measurement and Control Transactions of the ASME
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Dynamics? Systems Measurement and Control$", "Journal of Dynamic Systems Measurement and Control Transactions of the ASME")

  # Journal of Earthquake Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Earthquake$|^Journal of Earthquake Engineering Taylor and Francis UK$", "Journal of Earthquake Engineering")

  # Econ
  dt$journal.disam <- str_replace(dt$journal.disam, "Econ\\b", "Economic")

  # Journal of Emerg Nurs
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Emerg Nurs$", "Journal of Emergency Nursing")

  # Journal of Engineering Gas Turbines Power
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Engineering Gas Turbines Power$", "Journal of Engineering for Gas Turbines and Power")

  # Journal of Engineering Mechanics  ASCE
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Engineering Mechanical$|^Journal of Engineering Mechanics$|^Journal of Engineering MechanicsAsce$", "Journal of Engineering Mechanics  ASCE")

  # Journal of Engineering and Technology Management  JETM
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Engineering Tech Manage$", "Journal of Engineering and Technology Management  JETM")

  # Journal of Environmental Engineering ASCE
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Environmental Engineering$", "Journal of Environmental Engineering ASCE")

  # Journal of Environmental and Engineering Geophysics
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Environmental Engineering Geophysics$", "Journal of Environmental and Engineering Geophysics")

  # Hort
  dt$journal.disam <- str_replace(dt$journal.disam, "Hort\\b", "Horticulture")

  #  Journal of Environmental Plann Manage
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Environmental Plann Manage$", "Journal of Environmental Planning and Management")

  # Journal of Epidemiology and Community Health
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Epidemiology Commun$|^Journal of Epidemiology Community$", "Journal of Epidemiology and Community Health")

  # Journal of Fluid Mechanical
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Fluid Mechanical$", "Journal of Fluid Mechanics")

  # Journal of Fluids Engineering Transactions of the ASME
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Fluids Engineering$", "Journal of Fluids Engineering Transactions of the ASME")

  # Journal of Forestry
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Forestry.*", "Journal of Forestry")

  # Journal of General Internal Medicine
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Gen Intern Med$", "Journal of General Internal Medicine")

  # Journal of Geophysical Resear
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Geophysical Resear$|^Journal of Geophysical Research\\: Earth Surface$|^Journal of Geophysics Res$|^Journal of Geophysics Res: Solid Earth$", "Journal of General Internal Medicine")

  # Journal of Geotechnical and Geoenvironmental Engineering  ASCE
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Geotechnical and Geo Environmental Engineering Division$|^Journal of Geotechnical and Geo[Ee]nvironmental Engineering$|^Journal of Geotechnical and Geoenvironmental Engrg$|^Journal of Geotechnical Geoen Engineering$|^Journal of Geotechnical Geoenvironmental$|^Journal of Geotechnical Geoenvironmental Engineering$|^Journal of Geot Engineering Div$|^Journal of Geotechnical Engineering Div$|^Journal of Geotechnical Engrg\\s?D?i?v?$|^Journal of Geotechnical Geoen Engineering$|^Journal of Geotechnical Geoenvironmental$|^Journal of Geotechnical Geoenvironmental Engineering$|^Journal of Geotechnical and Geoenvironmental Engineering JOUR American Society of Civil Engineers$|^Journal of Geotechnical Engg$|^Journal of Geotechnical Geoenvironmental Engineering$", "Journal of Geotechnical and Geoenvironmental Engineering  ASCE")

  # Journal of Geotechnical Engineering , I am calling the one abocve

  # Journal of Health and Social Behavior
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Health Society$", "Journal of Health and Social Behavior")

  # Journal of Housing and the Built Environment
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Housing Built Environment$", "Journal of Housing and the Built Environment")

  # Journal of Housing Economic
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Housing Economic$", "Journal of Housing Economics")

  # Journal of Housing Economic
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Hydraulic Research$", "Journal of Hydraulic Research\\/De Recherches Hydrauliques")

  # Hydro
  dt$journal.disam <- str_replace(dt$journal.disam, "Hydro\\b", "Hydrology")

  # Journal of Hydrologic Engineering  ASCE
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Hydrologic Engineering$", "Journal of Hydrologic Engineering  ASCE")

  # Hydrometeorology
  dt$journal.disam <- str_replace(dt$journal.disam, "Hydrometeor\\b", "Hydrometeorology")

  # Journal of Industrial and Engineering Chemistry
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Ind Engineering Chem$", "Journal of Industrial and Engineering Chemistry")

  # Ind
  dt$journal.disam <- str_replace(dt$journal.disam, "\\bInd\\b", "Industrial")

  # Journal of Insect
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Insect$", "Journal of Insect Conservation")

  # Journal of Intell Transp Syst: Technol Planning and Operations
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Intell Transp Syst\\: Technol Planning and Operations$|^Journal of Intelligent Transportation System$", "Journal of Intelligent Transportation Systems")

  # Journal of Intelligent and Robotic Systems: Theory and Applications
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Intell Transp Syst: Technol Planning and Operations$|^Journal of Intelligent and Robotic Systems$", "Journal of Intelligent and Robotic Systems\\: Theory and Applications")

  # Journal of Irrigation and Drainage Engineering  ASCE
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Irrig and Drain Engrg$|^Journal of Irrigation and Drainage Engineering$", "Journal of Irrigation and Drainage Engineering  ASCE")

  # Journal of Mach Learn Res
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Mach Learn Res$", "Journal of Machine Learning Research")

  # Journal of Management in Engineering  ASCE
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Management in Engineering$", "Journal of Management in Engineering  ASCE")

  # Journal of Materials in Civil Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Materials Civil Engineering$|^Journal of Materials Civil Engrg$|^Journal of Materials in Civil Engrg$", "Journal of Materials in Civil Engineering")

  # Mathematics and Computers in Simulation
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Mathematics and Computers in Simulation$", "Mathematics and Computers in Simulation")

  # Journal of Mechanical Design Transactions of the ASME
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Mechanical Design$", "Journal of Mechanical Design Transactions of the ASME")

  # Journal of Mechanical Engineering and Sciences
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Mechanical Engineering Science$", "Journal of Mechanical Engineering and Sciences")

  # Journal of Modern Transportation
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Mod Transp$", "Journal of Modern Transportation")

  # Journal of Modern Transportation
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of of", "Journal of")

  # Journal of Multivariate Analysis
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Multivar Anal$", "Journal of Multivariate Analysis")

  # Journal of Petroleum Science and Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Pet Science Engineering$", "Journal of Petroleum Science and Engineering")

  # Journal of Petroleum Technology -- not a journal but worth standarizing
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Petr? Techn?o?l?$|^Journal of Pet Tech Transactions AIME$", "Journal of Petroleum Technology")

  # Journal of Petroleum Science and Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Petrol Science Engineering$", "Journal of Petroleum Science and Engineering")

  # Journal of Pharmaceutical Sciences
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Pharm Science$", "Journal of Pharmaceutical Sciences")

  # Phycol
  dt$journal.disam <- str_replace(dt$journal.disam, "Phycol\\b", "Phycology")

  # Journal of Phys Act Health
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Phys Act Health$|^Journal of Physical Activity and Health$|^Journal of Physical Activity$", "Journal of Physical Activity & Health")

  # Journal of Plan Educ
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Plan Educ$|^Journal of Plan Educ Res$", "Journal of Planning Education and Research")

  # Journal of Planning Literature
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Plan Lit$", "Journal of Planning Literature")

  # Journal of Policy Analysis and Managment
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Policy Analysis and Managment$", "Journal of Policy Analysis and Management")

  # Journal of Public Economic
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Public Economic$", "Journal of Public Economics")

  # Journal of Public Health Management and Practice : JPHMP
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Public Health Management and Practice$", "Journal of Public Health Management and Practice \\: JPHMP")

  # Journal of Public Transportation
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Public Transp$|^Journal of Public Transportation Research$", "Journal of Public Transportation")

  #Journal of the Royal Statistical Society Series a (Statistics in Society)
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of R Statist Society a$", "Journal of the Royal Statistical Society Series a \\(Statistics in Society\\)")

  # Journal of R Stat Society Ser B
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of R Stat Society Ser B$|^Journal of the Royal Statistical Society Series B.*|^Journal of the Royal Statistical Society$", "Journal of the Royal Statistical Society Series B\\: Statistical Methodology")

  # Journal of Real Estate Economics
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Real Estate Economics$|^Journal of Real Estate Financ Economic$|^The Journal of Real Estate Finance and Economics$|^The Journal of Real Estate Finance Economics$", "Journal of Real Estate Finance and Economics")

  # Journal of Real Estate Research
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Real Estate Res$|^The Journal of Real Estate Research$", "Journal of Real Estate Research")

  # Journal of Rock Mechanics and Geotechnical Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Rock Mechanical Geotechnical Engineering$", "Journal of Rock Mechanics and Geotechnical Engineering")

  # Res
  dt$journal.disam <- str_replace(dt$journal.disam, "Res\\b", "Research")

  # Journal of Soils and Water Conservation
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Soil and Water Conservation.*", "Journal of Soils and Water Conservation")

  # Journal of the Soil Mechanics and Foundations Division -- not a journal in scimago but really popular
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Soil$|^Journal of Soil Mechanical Fdns Div$|^Journal of Soil Mechanical Found$|^Journal of Soil Mechanical Found Div$|^Journal of Soil Mechanical Found Engin Div American Society Civil Engin$|^Journal of Soil Mechanical Nc Fdns Div$|^Journal of Soil Mechanics and Foundation.*|^Journal of the Soil Mechanics and Foundation$|^Journal of the Soil Mechanics and Foundations Division.*|^JSoil Mechanical and Foundations Div$", "Journal of the Soil Mechanics and Foundations Division")

  # Journal of Sound and Vibration
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Sound Vib$", "Journal of Sound and Vibration")

  # Journal of Southeast University (English Edition)
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Southeast University \\(English Edition$", "Journal of Southeast University \\(English Edition\\)")

  # Journal of Strain Analysis for Engineering Design
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Strain Analysis$", "Journal of Strain Analysis for Engineering Design")

  # Journal of Structural and Construction Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Structural Construction Engineering$", "Journal of Structural and Construction Engineering")

  # Journal of Structural Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Structural Div$|^Journal of Structural Engineering.*|^Journal of Structural Engrg$|^Journal of the Structural Engineering$|^Journal of the Structural Division Proceedings of the American Society of Civil Engineers$", "Journal of Structural Engineering")

  # Journal of Studies on Alcohol and Drugs
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Studies Alcohol$", "Journal of Studies on Alcohol and Drugs")

  # Journal of the American Concrete Institute
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of American Concrete Institute$|^Journal of the American Concrete Institute$|^Materials Journal$", "ACI Materials Journal")

  # Journal of the American Water Resources Association
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of the American Water Resources Association \\(JAWRA
$", "Journal of the American Water Resources Association")

  # Asphalt Paving Technology: Association of Asphalt Paving TechnologistsProceedings of the Technical Sessions
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of the Association of Asphalt Pavement Technologists
$|^Journal of the Association of Asphalt Paving Technologists$", "Asphalt Paving Technology\\: Association of Asphalt Paving TechnologistsProceedings of the Technical Sessions")

  # Journal of the Chinese Institute of Engineers Transactions of the Chinese Institute of EngineersSeries a/Chungkuo Kung Ch'eng Hsuch K'an
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of the Chinese Institute of Engineers$", "Journal of the Chinese Institute of Engineers Transactions of the Chinese Institute of EngineersSeries a\\/Chungkuo Kung Ch\\'eng Hsuch K\\'an")

  # Journal of the South African Institution of Civil Engineers
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of the South African Institution of Civil Engineers$", "Journal of the South African Institution of Civil Engineering")

  # Transportation Research Record
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of the Transportation Research Board$|^Journal of the Transportation Research Forum$|^Journal of Transportation Research$|^Journal of Transportation Research Record$", "Transportation Research Record")

  # Journal of Toxicology and Environmental Health
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Toxicology and Environmental HealthPart aCurrent Issues$|^Journal of Toxicology and Environmental Health$", "Journal of Toxicology and Environmental Health  Part a")

  # Trans
  dt$journal.disam <- str_replace(dt$journal.disam, "\\bTransp?\\b", "Transportation")
  # Geog
  dt$journal.disam <- str_replace(dt$journal.disam, "\\bGeog\\b", "Geography")

  # Transportation Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Transportation$|^Journal of Transportation Enginee1ing$|^Journal of Transportation Engineering American Society of Civil Engineers$|^Journal of Transportation EngineeringAsce$", "Journal of Transportation Engineering")

  # Journal of Transport and Land Use
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Transpotation L Use$|^Journal of Transport and Land Use \\(JTLU", "Journal of Transport and Land Use")

  # Journal of Transp Stat -- this is a government joirnal
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Transp Stat$", "Journal of Transportation and Statistics")

  # Journal of Transport Economics Policy
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Transport Economics Policy$", "Journal of Transport Economics and Policy")

  # Journal of Transportation Geography
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Transportation Geography$", "Journal of Transport Geography")

  # Journal of Trauma and Acute Care Surgery
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Trauma$", "Journal of Trauma and Acute Care Surgery")

  # Journal of Urban Technology
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Urban$", "Journal of Urban Technology")

  # Journal of Urban Economic
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Urban Economic.*", "Journal of Urban Economics")

  # Journal of Urban Planning and
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Urban Planning and$|^Journal of Urban Planning and DevelopmentAsce$", "Journal of Urban Planning and Development")

  # Journal of Vibration and Acoustics Transactions of the ASME
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Vibration and Acoustics.*", "Journal of Vibration and Acoustics Transactions of the ASME")

  # Journal of Water Resources Planning and Management  ASCE
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Water Resources Planning and ManagementAsce", "Journal of Water Resources Planning and Management  ASCE")

  # Journal of Waterway Port Coastal and Ocean Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Waterway Port Coastal and Ocean Engineering American Society of Civil Engineering$", "Journal of Waterway Port Coastal and Ocean Engineering")

  # Journal of Wil
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Wil.*|^JWidl Mgmt$", "Journal of Wildlife Management")

  # Journal of Wind Engineering and Industrial Aerodynamics
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Wind Engineering Industrial Aerodyn$", "Journal of Wind Engineering and Industrial Aerodynamics")

  # Journal of Zoology (London
  dt$journal.disam <- str_replace(dt$journal.disam, "^Journal of Zoology \\(London$", "Journal of Zoology")

  # Land Economics
  dt$journal.disam <- str_replace(dt$journal.disam, "^Land Economic$", "Land Economics")

  # Landscape and Urban Planning
  dt$journal.disam <- str_replace(dt$journal.disam, "^Landsc Urban Plan$|^Landscale and Urban Planning$|^Landscape and Planning$|^Landscaping and Urban Planning$", "Landscape and Urban Planning")

  # Lect Notes in Control and Inf
  dt$journal.disam <- str_replace(dt$journal.disam, "^Lect Notes in Control and Inf$", "Lecture Notes in Control and Information Sciences")

  # New England Journal of Medicine
  dt$journal.disam <- str_replace(dt$journal.disam, "^Life Expectancy the New England Journal of Medicine$|^NEJM
$", "New England Journal of Medicine")

  # Lighting Design and Application: LD and a
  dt$journal.disam <- str_replace(dt$journal.disam, "^Lighting Design and Application$", "Lighting Design and Application: LD and a")

  # unclear why Lighting Research and Technology is not a match

  # Limnol Oceanogr Methods
  dt$journal.disam <- str_replace(dt$journal.disam, "^Limnol Oceanogr Methods$", "Limnology and Oceanography\\: Methods")

  # Mag Concr Research
  dt$journal.disam <- str_replace(dt$journal.disam, "^Mag Concr Research$", "Magazine of Concrete Research")

  # Mar = Marine (I hope this doesn't mess up future March tags)
  dt$journal.disam <- str_replace(dt$journal.disam, "Mar\\b", "Marine")

  # Marine Ecology  Progress Series
  dt$journal.disam <- str_replace(dt$journal.disam, "^Marine Ecology Prog Ser$|^Marine Ecology Progress Series$", "Marine Ecology  Progress Series")

  # Marine and Petroleum Geology
  dt$journal.disam <- str_replace(dt$journal.disam, "^Marine Pet Geology", "Marine and Petroleum Geology")

  # Materials and Structures
  dt$journal.disam <- str_replace(dt$journal.disam, "^Materials and Structures$|^Materials Structural$|^Materiaux et Constructions$", "Materials and Structures\\/Materiaux et Constructions")

  # Materials Chemistry and Physics
  dt$journal.disam <- str_replace(dt$journal.disam, "^Materials Chem Physics$", "Materials Chemistry and Physics")

  # Materials and Corrosion  Werkstoffe Und Korrosion
  dt$journal.disam <- str_replace(dt$journal.disam, "^Materials Corrosion$", "Materials and Corrosion  Werkstoffe Und Korrosion")

  # Materials Evaluation
  dt$journal.disam <- str_replace(dt$journal.disam, "^Materials Evaluation Journal American Society for Nondestructive Testing$", "Materials Evaluation")

  # Materials Perform
  dt$journal.disam <- str_replace(dt$journal.disam, "^Materials Perform$", "Materials Performance")

  # Materials Phys Chem
  dt$journal.disam <- str_replace(dt$journal.disam, "^Materials Phys Chem$", "Materials Chemistry and Physics")

  # Materials Science and Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Materials Science and Engineering$", "Materials Science & Engineering A\\: Structural Materials\\: Properties Microstructure and Processing")

  # Mathematical Methods in the Applied Sciences
  dt$journal.disam <- str_replace(dt$journal.disam, "^Math Meth Applied Science$", "Mathematical Methods in the Applied Sciences")

  # Mathematical Programming Series B
  dt$journal.disam <- str_replace(dt$journal.disam, "^Mathematical Programming$|^Mathematical Programming SerB$", "Mathematical Programming Series B")

  # Measurement Science and Technology
  dt$journal.disam <- str_replace(dt$journal.disam, "^Meas Science Technol$", "Measurement Science and Technology")

  # Mechanical Systems and Signal Processing
  dt$journal.disam <- str_replace(dt$journal.disam, "^Mechanical Syst Signal Process$", "Mechanical Systems and Signal Processing")

  # Mechatronics IEEE/ASME Transactions on
  dt$journal.disam <- str_replace(dt$journal.disam, "^Mechatronics IEEE/ASME Transactions on$", "Mechatronics")

  # Medicine and Science in Sports and Exercise
  dt$journal.disam <- str_replace(dt$journal.disam, "^Medicine and Science in Sports Exercise$", "Medicine and Science in Sports and Exercise")

  # Metallurgical and Materials Transactions a
  dt$journal.disam <- str_replace(dt$journal.disam, "^Metallurgical and Materials Transactions a$|^Metallurgical Transactions a$", "Metallurgical and Materials Transactions A\\: Physical Metallurgy and Materials Science")

  # Mol
  dt$journal.disam <- str_replace(dt$journal.disam, "Mol\\b", "Molecular")

  # Molecular Biology Evol
  dt$journal.disam <- str_replace(dt$journal.disam, "^Molecular Biology Evol$", "Molecular Biology and Evolution")

  # Nat Clim Chang
  dt$journal.disam <- str_replace(dt$journal.disam, "^Nat Clim Chang$", "Nature Climate Change")

  #Nat Research
  dt$journal.disam <- str_replace(dt$journal.disam, "^Nat Research$", "Natural Resources Forum")

  # Natural Hazards and Earth System Sciences
  dt$journal.disam <- str_replace(dt$journal.disam, "^Natural Hazards and Earth System Science$", "Natural Hazards and Earth System Sciences")

  # Natural Resource Journal
  dt$journal.disam <- str_replace(dt$journal.disam, "^Natural Resource Journal$", "Natural Resources Journal")

  # NCHRP Synthesis
  dt$journal.disam <- str_replace(dt$journal.disam, "^NCHRP.*", "National Cooperative Highway Research Program")

  # Near Surf Geophysics
  dt$journal.disam <- str_replace(dt$journal.disam, "^Near Surf Geophysics$", "Near Surface Geophysics")

  # Networks and Heterogeneous Media
  dt$journal.disam <- str_replace(dt$journal.disam, "^Netw Heterog Media$", "Networks and Heterogeneous Media")

  # Netw Spat Economic
  dt$journal.disam <- str_replace(dt$journal.disam, "^Netw Spat Economic$|^Network and Spatial Economics$", "Networks and Spatial Economics")

  # Noise Health
  dt$journal.disam <- str_replace(dt$journal.disam, "^Noise Health$", "Noise and Health")

  # Nonlinear Analysis Theory Methods and Applications
  dt$journal.disam <- str_replace(dt$journal.disam, "^Nonlinear Analysis$", "Nonlinear Analysis Theory Methods and Applications")

  # Numerishe Mathematik
  dt$journal.disam <- str_replace(dt$journal.disam, "^Numerishe Mathematik$", "Numerische Mathematik")

  # Operation Research
  dt$journal.disam <- str_replace(dt$journal.disam, "^Operation Research$|^Operations Reseaich$", "Operation Researchs")

  # Not a journal but an org Pacific Coast Archaeological Society Quarterly
  dt$journal.disam <- str_replace(dt$journal.disam, "^Pacific Coast Archaeological Society Quarterly$", "Pacific Coast Archaeological Society")

  # Pacific Earthquake Engineering Research
  dt$journal.disam <- str_replace(dt$journal.disam, "^Pacific Earthquake Engineering Research.*|^PEER.*", "Pacific Earthquake Engineering Research")
  dt$journal.disam <- ifelse(str_detect(dt$journal.disam, "PEER"), "Pacific Earthquake Engineering Research", dt$journal.disam)

  # Packaging Technology and Science
  dt$journal.disam <- str_replace(dt$journal.disam, "^Packaging and Technology Science$", "Packaging Technology and Science")

  # Pervasive and Mobile Computing
  dt$journal.disam <- str_replace(dt$journal.disam, "^Pervasive Computing$", "Pervasive and Mobile Computing")

  # Petroleum Engineer International
  dt$journal.disam <- str_replace(dt$journal.disam, "^Pet Engineering$", "Petroleum Engineer International")

  # Philosophical Transactions of the Royal Society A: Mathematical Physical and Engineering Sciences
  dt$journal.disam <- str_replace(dt$journal.disam, "^Philosophical Transactions of the Royal Society a$|^Phil Transportation R Society a$|^Philosophical Transactions of the Royal Society of London$|^Philosophical Transactions of the Royal Society of London Series [Aa]Mathematical Physical and Engineering Sciences$|^Philosophical Transactions: Mathematical Physical and Engineering Sciences$", "Philosophical Transactions of the Royal Society A\\: Mathematical Physical and Engineering Sciences")

  # Philosophical Transactions of the Royal Society B: Biological Sciences
  dt$journal.disam <- str_replace(dt$journal.disam, "^Philosophical Transactions of the Royal Society of London Series BBiological Sciences$", "Philosophical Transactions of the Royal Society B: Biological Sciences")

  # Phys D
  dt$journal.disam <- str_replace(dt$journal.disam, "^Phys D$", "Physica D: Nonlinear Phenomena")

  # Physics of the Earth and Planetary Interiors
  dt$journal.disam <- str_replace(dt$journal.disam, "^Phys Earth Planet International$", "Physics of the Earth and Planetary Interiors")

  # Physical Review Letters
  dt$journal.disam <- str_replace(dt$journal.disam, "^Phys Review of Lett$", "Physical Review Letters")

  # Physics and Chemistry of the Earth
  dt$journal.disam <- str_replace(dt$journal.disam, "^Physics and Chemistry of the Earth Parts.*$", "Physics and Chemistry of the Earth")

  # PLoS Medicine
  dt$journal.disam <- str_replace(dt$journal.disam, "^PLOS Med$", "PLoS Medicine")

  # PLoS ONE
  dt$journal.disam <- str_replace(dt$journal.disam, "^P[Ll][Oo][Ss] [Oo][Nn][Ee]$", "PLoS ONE")


  # Prepared for all look like they have authors, so I should just remove the prepared for
  dt$journal.disam <- ifelse(str_detect(dt$journal.disam, "^Prepared for t?h?e?"), str_remove(dt$journal.disam, "^Prepared for t?h?e?\\s?"), dt$journal.disam)

  # Preventative Medicine
  dt$journal.disam <- str_replace(dt$journal.disam, "^Preventative Medicine$", "Preventive Medicine")

  # Probabilistic Engineering Mechanics
  dt$journal.disam <- str_replace(dt$journal.disam, "^Probability Engineering Mechanics$", "Probabilistic Engineering Mechanics")

  # Proc
  dt$journal.disam <- str_replace(dt$journal.disam, "^Proc\\b", "Proceedings ")

  # Symp
  dt$journal.disam <- str_replace(dt$journal.disam, "Symp\\b", "Symposium")

  # Procedia  Social and Behavioral Sciences
  dt$journal.disam <- str_replace(dt$journal.disam, "^Procedia‐Social and Behavioral Sciences$|^ProcediaSocial and Behavioral Sciences$", "Procedia  Social and Behavioral Sciences")

  # Looking at the proceedings that are journals and matching those before throwing them into conference bucket later

  # Proceedings of the National Academy of Sciences of the United States of America
  dt$journal.disam <- str_replace(dt$journal.disam, "^PNAS.*|^Proceedings of the National Academy of Sciences$", "Proceedings of the National Academy of Sciences of the United States of America")

  # Proceedings of the IEEE Special Issue on Vehicular Communications
  dt$journal.disam <- str_replace(dt$journal.disam, "^Proceedings of the IEEE Special Issue on Vehicular Communications$|^Proceedings  IEEE$|^Proceedings  of the IEEE Journal", "Proceedings of the IEEE")

  # Proceedings of the Royal Society A: Mathematical Physical and Engineering Sciences
  # Proceedings  Royal Society
  dt$journal.disam <- str_replace(dt$journal.disam, "^Proceedings  Royal Society$|^Proceedings  Royal Society of London Series a$|^Proceedings of the Royal Society of London Series a Mathematical and Physical Sciences$|^Royal Society: Math Phys Engineering Science$|^Ser a Math Phys Engineering Science$|^Series a Mathematical and Physical Sciences$", "Proceedings of the Royal Society A\\: Mathematical Physical and Engineering Sciences")

  # Proceedings of the Royal Society B: Biological Sciences
  dt$journal.disam <- str_replace(dt$journal.disam, "^Proceedings of the Royal Society Biological Sciences$", "Proceedings of the Royal Society B\\: Biological Sciences")

  # Progress in Energy and Combustion Science
  dt$journal.disam <- str_replace(dt$journal.disam, "^Prog Energy Combust Science$", "Progress in Energy and Combustion Science")

  # Prog Structural Engng Materials
  dt$journal.disam <- str_replace(dt$journal.disam, "^Prog Energy Combust Science$", "Progress in Energy and Combustion Science")

  # Psychosomatic Medicine
  dt$journal.disam <- str_replace(dt$journal.disam, "^Psychosom Med$", "Psychosomatic Medicine")

  # Published by is shorter -- do tis and Report to and Submitted for
  dt$journal.disam <- ifelse(str_detect(dt$journal.disam, "^Published by"), str_remove(dt$journal.disam, "^Published by "), dt$journal.disam)
  dt$journal.disam <- ifelse(str_detect(dt$journal.disam, "^Published in"), str_remove(dt$journal.disam, "^Published in "), dt$journal.disam)

  # Pure and Applied Geophysics
  dt$journal.disam <- str_replace(dt$journal.disam, "^Pure Applied Geophysics$", "Pure and Applied Geophysics")

  # Pure and Applied Mathematics Quarterly
  dt$journal.disam <- str_replace(dt$journal.disam, "^Pure Applied Math$", "Pure and Applied Mathematics Quarterly")

  # Q Journal of Engineering Geology
  dt$journal.disam <- str_replace(dt$journal.disam, "^Q Journal of Engineering Geology$|^Quart Journal of Engineering Geology Hydrogeology$", "Quarterly Journal of Engineering Geology and Hydrogeology")

  # RAND Journal of Economics
  dt$journal.disam <- str_replace(dt$journal.disam, "^Rand Journal of Economics$|^The RAND Journal of Economics$", "RAND Journal of Economics")

  # Real Estate Economic
  dt$journal.disam <- str_replace(dt$journal.disam, "^Real Estate Economic$", "Real Estate Economics")

  # Regional Science and Urban Economics
  dt$journal.disam <- str_replace(dt$journal.disam, "^Reg Science Urban Economic$", "Regional Science and Urban Economics")

  # Reg Studies
  dt$journal.disam <- str_replace(dt$journal.disam, "^Reg Studies$", "Regional Studies")

  # Renewable and Sustainable Energy Reviews
  dt$journal.disam <- str_replace(dt$journal.disam, "^Renew Sustain Energy Review$|^Renewable and Sustainable Energy.*", "Renewable and Sustainable Energy Reviews")

  # Report by, for and to should all just be removed
  dt$journal.disam <- ifelse(str_detect(dt$journal.disam, "^Report by"), str_remove(dt$journal.disam, "^Report by "), dt$journal.disam)
  dt$journal.disam <- ifelse(str_detect(dt$journal.disam, "^Report for"), str_remove(dt$journal.disam, "^Report for "), dt$journal.disam)
  dt$journal.disam <- ifelse(str_detect(dt$journal.disam, "^Report to"), str_remove(dt$journal.disam, "^Report to "), dt$journal.disam)

  # Road Materials and Pavement Design
  dt$journal.disam <- str_replace(dt$journal.disam, "^Road Materials and Pavement Design \\(Online$", "Road Materials and Pavement Design")

  # Rock Mechanics and Rock Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Rock Mechanical Engineering Geology$|^Rock Mechanical Rock Engineering$", "Rock Mechanics and Rock Engineering")

  # Sun: STUVW to Within the Transportation

  # SAE Technical Paper
  dt$journal.disam <- str_replace(dt$journal.disam, "^SAE.*", "SAE Technical Papers")

  # Science Magazine
  dt$journal.disam <- str_replace(dt$journal.disam, "^Science Magazine$", "Science")

  # Environmental Science & Technology
  dt$journal.disam <- str_replace(dt$journal.disam, "^Science Technol$", "Environmental Science & Technology")

  # Science of the Total Environment
  dt$journal.disam <- str_replace(dt$journal.disam, "^Science Total Environment$", "Science of the Total Environment")

  # Seismological Research Letters
  dt$journal.disam <- str_replace(dt$journal.disam, "^Seismol Research Lett$|^Seismological Research Let.*", "Seismological Research Letters")

  # Shock and Vibration
  dt$journal.disam <- str_replace(dt$journal.disam, "^Shock Vib$", "Shock and Vibration")

  # SIAM Journal of Applied Math
  dt$journal.disam <- str_replace(dt$journal.disam, "^SIAM Journal of Applied Math$", "SIAM Journal on Applied Mathematics")

  # SIAM Journal of Control and Optimization
  dt$journal.disam <- str_replace(dt$journal.disam, "^SIAM Journal of Control and Optimization$|^SIAM Journal of Control Optim$", "SIAM Journal on Control and Optimization")

  # SIAM Journal of Matrix Anal Applied
  dt$journal.disam <- str_replace(dt$journal.disam, "^SIAM Journal of Matrix Anal Applied$", "SIAM Journal on Matrix Analysis and Applications")

  # SIAM Journal of Scientific Computing
  dt$journal.disam <- str_replace(dt$journal.disam, "^SIAM Journal on Scientific Computing$", "SIAM Journal of Scientific Computing")

  # Signal Processing IEEE Transactions on
  dt$journal.disam <- str_replace(dt$journal.disam, "^Signal Processing IEEE Transactions on$|^Signal Processing Magazine IEEE$", "Signal Processing")

  # Smart Materials and Structures
  dt$journal.disam <- str_replace(dt$journal.disam, "^Smart Materials Structural$|^Smart Structures and Materials$", "Smart Materials and Structures")

  # Society Science
  dt$journal.disam <- str_replace(dt$journal.disam, "^Society Science$|^Society Science Med$", "Social Science and Medicine")

  # Social Studies of Science
  dt$journal.disam <- str_replace(dt$journal.disam, "^Society Studies Science$", "Social Studies of Science")

  # Soils and Foundations
  dt$journal.disam <- str_replace(dt$journal.disam, "^Soil and Foundations$", "Soils and Foundations")

  # Soil Dynamics and Earthquake Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Soil Dyn Earthquake Engineering$", "Soil Dynamics and Earthquake Engineering")

  # Communications in Soil Science and Plant Analysis
  dt$journal.disam <- str_replace(dt$journal.disam, "^Soil Science Plant Anal$", "Communications in Soil Science and Plant Analysis")

  # Soil Science Society of America Journal
  dt$journal.disam <- str_replace(dt$journal.disam, "^Soil Science Society American Journal.*$|^Soil Science Society of American Journal of$", "Soil Science Society of America Journal")

  # Soil and Tillage Research
  dt$journal.disam <- str_replace(dt$journal.disam, "^Soil Till Research$", "Soil and Tillage Research")

  # Soils and Foundations
  dt$journal.disam <- str_replace(dt$journal.disam, "^Soils and Foundations.*$|^Soils Found$", "Soils and Foundations")

  # Spectra
  dt$journal.disam <- str_replace(dt$journal.disam, "^Spectra$", "Earthquake Spectra")

  # Standard and Poors PPP Credit Survey
  dt$journal.disam <- str_replace(dt$journal.disam, "^Standard and Poors PPP Credit Survey$", "SandP")

  # Stanford Law Review
  dt$journal.disam <- str_replace(dt$journal.disam, "^Stanford Law Policy Review$", "Stanford Law Review")

  # Statistics and Computing
  dt$journal.disam <- str_replace(dt$journal.disam, "^Stat Computing$", "Statistics and Computing")

  # Statistical Methods in Medical Research
  dt$journal.disam <- str_replace(dt$journal.disam, "^Stat Methods Med Research$|^Statistical Methods in Medical$", "Statistical Methods in Medical Research")

  # Structural Engineering/Earthquake Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Structural and Earthquake Engineering Proc JSCE$", "Structural Engineering\\/Earthquake Engineering")

  # Structural Concrete
  dt$journal.disam <- str_replace(dt$journal.disam, "^Structural Concr Journal of FIB$", "Structural Concrete")

  # Structural Control and Health Monitoring
  dt$journal.disam <- str_replace(dt$journal.disam, "^Structural Control and Health Monitoring Inpress$|^Structural Control Heal Monit$|^Structural Heal Monit$", "Structural Control and Health Monitoring")

  # Structural Design of Tall and Special Buildings
  dt$journal.disam <- str_replace(dt$journal.disam, "^Structural Design of Tall Buildings$", "Structural Design of Tall and Special Buildings")

  # Structural Dynamics @ 2000: Current Status and Future Directions Research Studies Press Ltd
  dt$journal.disam <- str_replace(dt$journal.disam, "^Structural Dynamics @ 2000: Current Status and Future Directions Research Studies Press Ltd$", "Structural Dynamics")

  # Structural Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Structural Engineering$|^Structural Engineering International$", "Structural Engineering International: Journal of the International Association for Bridge and Structural Engineering (IABSE)")

  # Structure and Infrastructure Engineering
  dt$journal.disam <- str_replace(dt$journal.disam, "^Structural Infrastruct Engineering$", "Structure and Infrastructure Engineering")

  # Studies Avian Biology -- I don't know why this is not being detected, I think it ended in 2008
  dt$journal.disam <- str_replace(dt$journal.disam, "^Studies Avian Biology$", "Studies in Avian Biology")

  # Submitted to
  dt$journal.disam <- ifelse(str_detect(dt$journal.disam, "^Submitted to"), str_remove(dt$journal.disam, "^Submitted to "), dt$journal.disam)

  # Studies in Symbolic Interaction
  dt$journal.disam <- str_replace(dt$journal.disam, "^Studies Symb Interact$", "Studies in Symbolic Interaction")

  # Supply Chain Management
  dt$journal.disam <- str_replace(dt$journal.disam, "^Supply Chain Management\\: An International Journal$", "Supply Chain Management")

  # Technometrics
  dt$journal.disam <- str_replace(dt$journal.disam, "^Technometric$", "Technometrics")

  # American Economic Review
  dt$journal.disam <- str_replace(dt$journal.disam, "^The American Economic Review$", "American Economic Review")

  # American Journal of Clinical Nutrition
  dt$journal.disam <- str_replace(dt$journal.disam, "^The American Journal of Clinical Nutrition$", "American Journal of Clinical Nutrition")

  # American Naturalist
  dt$journal.disam <- str_replace(dt$journal.disam, "^The American Naturalist$", "American Naturalist")

  # the
  dt$journal.disam <- ifelse(str_detect(dt$journal.disam, "^the"), str_remove(dt$journal.disam, "^the "), dt$journal.disam)

  # Annals of Porb
  dt$journal.disam <- str_replace(dt$journal.disam, "^The Annals of Probability$", "Annals of Probability")

  # Annals of Regional Science
  dt$journal.disam <- str_replace(dt$journal.disam, "^The Annals of Regional Science$", "Annals of Regional Science")

  # Annals of Regional Statistics
  dt$journal.disam <- str_replace(dt$journal.disam, "^The Annals of Statistics$", "Annals of Statistics")

  # Auk
  dt$journal.disam <- str_replace(dt$journal.disam, "^The Auk$", "Auk")

  # International Journal of Life Cycle Assessment
  dt$journal.disam <- str_replace(dt$journal.disam, "^The International Journal of Life Cycle Assessment$", "International Journal of Life Cycle Assessment")

  # International Journal of Logistics Management
  dt$journal.disam <- str_replace(dt$journal.disam, "^The International Journal of Logistics Management$", "International Journal of Logistics Management")

  # Robotics Research
  dt$journal.disam <- str_replace(dt$journal.disam, "^The International Journal of Robotics Research$", "International Journal of Robotics Research")

  # Journal of Economic Inequality
  dt$journal.disam <- str_replace(dt$journal.disam, "^The Journal of Economic Inequality$", "Journal of Economic Inequality")

  # The Journal of Economic Perspectives
  dt$journal.disam <- str_replace(dt$journal.disam, "^The Journal of Economic Perspectives$", "Journal of Economic Perspectives")

  # The Journal of Industrial Economics
  dt$journal.disam <- str_replace(dt$journal.disam, "^The Journal of Industrial Economics$", "Journal of Industrial Economics")

  # The Journal of Machine Learning Research
  dt$journal.disam <- str_replace(dt$journal.disam, "^The Journal of Machine Learning Research$", "Journal of Machine Learning Research")

  # The Journal of Supercomputing
  dt$journal.disam <- str_replace(dt$journal.disam, "^The Journal of Supercomputing$", "Journal of Supercomputing")

  # The Quarterly Journal of Economics
  dt$journal.disam <- str_replace(dt$journal.disam, "^The Quarterly Journal of Economics$", "Quarterly Journal of Economics")

  # Quarterly Review of Economics and Finance
  dt$journal.disam <- str_replace(dt$journal.disam, "^The Quarterly Review of Economics and Finance$", "Quarterly Review of Economics and Finance")

  # The Senses and Society
  dt$journal.disam <- str_replace(dt$journal.disam, "^The Senses and Society$", "Senses and Society")

  # The Social Science Journal
  dt$journal.disam <- str_replace(dt$journal.disam, "^ The Social Science Journal$", "Social Science Journal")

  # Traffic Engineering and Control
  dt$journal.disam <- str_replace(dt$journal.disam, "^Traffic Engineering Control$|^Traffic Engineeiing and Control$", "Traffic Engineering and Control")


  # SKIPPED MORE

  # Transportation Research Part A: Policy and Practice
  dt$journal.disam <- str_replace(dt$journal.disam, "^Transport Research APol$|^Transport Research\\:? Part a$|^Transportation Research a$|^Transportation Research A: Policy and Practice$|^Transportation Research Part [Aa].*|^Transportation Research\\: Part a", "Transportation Research Part A\\: Policy and Practice")

  # Transportation Research Part B: Methodological
  pattern <- c("^Transp01tation Research Part B$", "^Transpmtation Research Pait B: Methodological$", "^Transpn Research$", "^Transporation Research B$", "Transportation Re Search Part B\\: Methodological$", "^Transportation Reseaich Part B.*$", "^Transportation Research B$", "^Transportation Research Pait B.*|^Transportation Research\\:? Part\\s?B.*")
  pattern <- paste(pattern, collapse = "|")
  dt$journal.disam <- str_replace(dt$journal.disam, pattern, "Transportation Research Part B\\: Methodological")

  # Transportation Research Part C: Emerging Technologies
  pattern <- c("^Transpo1tation Research Part C\\: Emerging Technologies$", "^Transport Research Part C: Emerging Technol$", "Transportation Reseaich Pait C\\: Emerging Technologies$", "^Transportation Research C$|^Transportation Research C Emerging Technol$|^Transportation Research C: Emerging Technologies$|^Transportation Research\\:? Part C.*|^Transportation ResearchPart C")
  pattern <- paste(pattern, collapse = "|")
  dt$journal.disam <- str_replace(dt$journal.disam, pattern, "Transportation Research Part C\\: Emerging Technologies")

  # Transportation Research Part D: Transport and Environment -- I am including Transportation Research Part in here, even though not all are D, most seem to be
  dt$journal.disam <- str_replace(dt$journal.disam, "^Transportation Research D.*$|^Transportation Research Part$|^Transportation Research\\:? Part D.*", "Transportation Research Part D\\: Transport and Environment")

  # Transportation Research Part E: Logistics and Transportation Review
  dt$journal.disam <- str_replace(dt$journal.disam, "^Transport Research ELog$|^Transportation Research  Part E$|^Transportation Resear\\:? Part E.*|^Transportation Research Part E$|^Transportation Research Part ELogistics and Transportation Review$", "Transportation Research Part E\\: Logistics and Transportation Review")

  # Transportation Research Part F: Traffic Psychology and Behaviour
  dt$journal.disam <- str_replace(dt$journal.disam, "^Transportat Research F\\: Traff Psychology Behavior$|^Transportation Research Part F.*|^Transportation Research\\: Part F$", "Transportation Research Part F\\: Traffic Psychology and Behaviour")

  # Transportation Research Record
  dt$journal.disam <- str_replace(dt$journal.disam, "^Transp01tation Research Record: Journal of the Transportation Research Board$|^Transpn Research Rec$|^Transport Research Rec$|^Transport Research Rec: Journal of Transportation Res Board$|^Transportation Reasearch Record: Journal of the Transportation Research Board$|^Transportation Research Rec$|^Transportation Research Rec\\:? Journal of Transp Res Board$|^TRANSPORTATION RESEARCH RECORD|^Transportation Research Repord.*|^Transptn Research Rec$|^Transrortation Research Record64and$|^TRR\\b|^Transportation Research Record.*", "Transportation Research Record")

  # Transport Pol
  dt$journal.disam <- str_replace(dt$journal.disam, "^Transport Pol$|^Transport Policy17\\(2\\):7284$|^Transportation Policy$|^Transportation Transport Policy$", "Transport Policy")

  # Transport Science
  dt$journal.disam <- str_replace(dt$journal.disam, "^Transport Science$", "Transportation Science")

  # Transport Reviews
  dt$journal.disam <- str_replace(dt$journal.disam, "^Transport Reviews\\: A Transnational Transdisciplinary Journal$", "Transport Reviews")

  # Transportation (Amst
  dt$journal.disam <- str_replace(dt$journal.disam, "^Transportation \\(Amst$", "Transportation")

  # Transportmetrica A: Transport Science
  dt$journal.disam <- str_replace(dt$journal.disam, "^Transportation A\\: Transp Science$", "Transportmetrica A\\: Transport Science")

  #ASAE is the ASABE in Scimago!!!
  # Transactions of the ASABE
  dt$journal.disam <- str_replace(dt$journal.disam, "^Transportation ASAE$|^^Transportation of the ASAE$", "Transactions of the ASABE")

  # Transportation Letters: The International Journal of Transportation Research
  dt$journal.disam <- str_replace(dt$journal.disam, "^Transportation Letters\\: The International Journal of Transportation Research$", "Transportation Letters")

  # Transportation LJournal
  dt$journal.disam <- str_replace(dt$journal.disam, "^Transportation LJournal$", "Transportation Journal")

  # Transport in Porous Media
  dt$journal.disam <- str_replace(dt$journal.disam, "^Transportation Porous Media$", "Transport in Porous Media")

  # On its own "Transportation Research" cannot be connected to one journal. The titles link to multiple of the A-F above and Research Record

  # It looks like this Board runs into the research program, so I can assign this so it can be detected as an org later
  dt$journal.disam <- str_replace(dt$journal.disam, "^Transportation Research Board.*", "National Cooperative Highway Research Program")

  # Tunnelling and Underground Space Technology
  dt$journal.disam <- str_replace(dt$journal.disam, "^Tunnel Undergr Space Tech$|^Tunneling and Underground Space Technology$", "Tunnelling and Underground Space Technology")

  # URBAN STUDIES
  dt$journal.disam <- str_replace(dt$journal.disam, "^URBAN STUDIES", "Urban Studies")

  # World Applied Sciences Journal
  dt$journal.disam <- str_replace(dt$journal.disam, "^World Applied Science Journal$", "World Applied Sciences Journal")

  # World Dredging Mining and Constructions
  dt$journal.disam <- str_replace(dt$journal.disam, "^World Dredging Mining and Construction$", "World Dredging Mining and Constructions")

  # ZAMM Zeitschrift Für Angewandte Mathematik Und Mechanik
  dt$journal.disam <- str_replace(dt$journal.disam, "^ZAMM‐Journal of Applied Mathematics and Mechanics/Zeitschrift Fuf Grund Der Plastizitätsbedingung Fü$", "ZAMM Zeitschrift Für Angewandte Mathematik Und Mechanik")

  #Prepared by sections -- moving these over to author or publisher
  new.dt <- data.table()
  for (i in 1:nrow(dt)){
    prep.by <- str_detect(dt$journal.disam[i], "^Prepared by [a-z+]?")
    # Need to look up this anything followed by)
    author <- ifelse(prep.by == T & is.na(dt$author[i]),
                     str_extract(dt$journal.disam[i], "(?<=[Pp]repared\\s[Bb]y\\st?h?e?).*"),
                     dt$author[i])
    publisher <- ifelse(prep.by == T & !is.na(dt$author[i]) & is.na(dt$publisher[i]),
                        str_extract(dt$journal.disam[i], "(?<=[Pp]repared\\s[Bb]y\\s[a-z+]?).*"),
                        dt$publisher[i])
    journal.disam <- ifelse(prep.by == T,
                      str_extract(dt$journal.disam[i], "(?<=[Pp]repared\\s[Bb]y\\s[a-z+]?).*"),
                      dt$journal.disam[i])
    temp.dt <- cbind(author, publisher, journal.disam, i)
    new.dt <- rbind(temp.dt, new.dt)
  }

  dt$i = row.names(dt)

  dt <- left_join(new.dt, dt, by = "i") %>% select(-author.y, -journal.disam.y, -publisher.y)
  dt <- dt %>% rename("author" = "author.x", "journal.disam" = "journal.disam.x",
                      "publisher"= "publisher.x")

  return(dt)
}
