#' Disambiguate Journal Names
#'
#' Creates a new column for extended journal names based on known abbreviations from [ISO documentation from XpertScientific](https://xpertscientific.com/en/journal-abbreviations/)
#'
#' @param x a column containing (potential) journal names. If working through the govscienceuseR workflow, this column name is 'container'
#' @param remove_periods boolean for whether to remove all remaining periods (e.g., currently "^J." is replaced by "Journal of.")
#' @return a vector with disambiguated journal names
#'
#' @examples dt_clean <- journal_disambig(dt$container)
#'
#' @export

journal_disambig <- function(x, remove_periods = T){

  j_index <- fread("~/Documents/Davis/R-Projects/citationClassify/data/indices/journal_abbr.csv")

  x <- base::trimws(x)

  # 1. Looking for exact matches to an index of abbreviated journals
  output <- which(x %in% j_index$abbr)
  for(i in output){
    for(j in 1:nrow(j_index)){
      x[i] <- ifelse(x[i] == j_index$abbr[j],
                                    j_index$title[j], x[i])
    }
  }

  # 2. Remove published by-like prefixes (other function to re-appropriate)

  prep_pattern <- c("(?<=[Pp]repared ([Bb]y|[Ff]or)\\s?(the)?).*",
                  "(?<=[Pp]ublished ([Bb]y|[Ii]n)\\s?(the)?).*",
                  "(?<=[Rr]eport ([Bb]y|[Ff]or|[Tt]o)\\s?(the)?).*",
                  "(?<=[Ss]ubmitted ([Tt]o|[Ff]or)\\s?(the)?).*")
  prep_pattern <- paste(prep_pattern, collapse = "|")

  x <- str_remove_all(x, prep_pattern)

  # 3. Replace manually developed abbreviations

  # Running through item by item to see if this improves match
  # Adv[.]? should be Advances -- see Adv for some inspiration
  x <- str_replace(x, "Adv\\b|Advn\\b", "Advances in")
  # Agric[.]? for Agricultur
  x <- str_replace(x, "Agric\\b", "Agriculture")
  # Anim. = Animal
  x <- str_replace(x, "Anim\\b", "Animal")
  # Am J = American journal of
  x <- str_replace(x, "Am\\sJ\\b", "American Journal of")
  # Am = America at end
  x <- str_replace(x, "Am$|Amer$", "America")
  # Am = American
  x <- str_replace(x, "Am\\b|Amer\\b", "American")
  # Ann. is Annals of -- removing because impossible to discern between Annual, which can also be Ann.
  #dt$journal <- str_replace(dt$journal, "Ann\\b", "Annals")
  # Annu. is annual
  x <- str_replace(x, "Annu\\b", "Annual")
  # Atmos.is Atmospheric
  x <- str_replace(x, "Atmos\\b", "Atmospheric")
  # Assoc.= Association
  x <- str_replace(x, "Assoc\\b", "Association")
  # Appl. is Applied
  x <- str_replace(x, "Appl\\b", "Applied")
  # Biol. = Biology
  x <- str_replace(x, "Biol\\b", "Biology")
  #Behav = Behavior
  x <- str_replace(x, "Behav\\b", "Behavior")
  #Bull = Bulletin at end
  x <- str_replace(x, "Bull$", "Bulletin")
  #Bull = Bulletin
  x <- str_replace(x, "Bull\\b", "Bulletin of")
  # Can. = Canadian (keys on period to ensure note just word "can") -- this seems like a rabbithole
  x <- str_replace(x, "Can\\b", "Canadian")
  # Cem Bas Mat
  x <- str_replace(x, "Cem\\sBas\\sMat[a-z]?\\b", "Cement-Based Materials")
  # Chem = Chemistry
  x <- str_replace(x, "Chem\\b", "Chemistry")
  # Civ = Civil
  x <- str_replace(x, "Civ\\b", "Civil")
  # Climatol = Climatology
  x <- str_replace(x, "Climatol\\b", "Climatology")
  # Conf = Consference
  x <- str_replace(x, "Conf\\b", "Conference")
  # Conserv = Conservation
  x <- str_replace(x, "Conserv\\b", "Conservation")
  # Comput = Computing
  x <- str_replace(x, "Comput\\b", "Computing")
  # Constr = Constructions
  x <- str_replace(x, "Constr\\b", "Construction")
  # Corro = Corrosion
  x <- str_replace(x, "Corros?\\b", "Corrosion")
  # Croat == Croation
  x <- str_replace(x, "Croat?\\b", "Croatian")
  # Earthq.= Earthquake
  x <- str_replace(x, "Earthq\\b", "Earthquake")
  # Ecol[.]? should be Ecology
  x <- str_replace(x, "Ecol\\b", "Ecology")
  # Eng[.]? should be Engineering
  x <- str_replace(x, "Eng\\b", "Engineering")
  # Ent[.]? should be Entomology
  x <- str_replace(x, "Ent\\b|Entomol\\b", "Entomology")
  # Environ. = Environment at end
  x <- str_replace(x, "Environ$|Envt$|Envir$", "Environment")
  # Environ. = Environmtnal
  x <- str_replace(x, "Environ\\b|Env\\b|Envir\\b", "Environmental")
  # Ergon Ergonomics
  x <- str_replace(x, "Ergon\\b", "Ergonomics")
  # Epidemiol
  x <- str_replace(x, "Epidemiol\\b", "Epidemiology")
  # European Euro
  x <- str_replace(x, "Euro?\\b", "European")
  # Genet
  x <- str_replace(x, "Genet\\b", "Genetics")
  # For. = Forest (note this keys on period to ensure not just word "for")
  x <- str_replace(x, "For\\.", "Forest")
  # Geophys
  x <- str_replace(x, "Geophys\\b", "Geophysics")
  # Geol. = Geology
  x <- str_replace(x, "Geol\\b", "Geology")
  # Geoenv Geoenvi.
  x <- str_replace(x, "Geo[Ee]nvi?r?o?n?\\b", "Geoenvironmental")
  # Geotech
  x <- str_replace(x, "Geotech\\b", "Geotechnical")
  # Hous
  x <- str_replace(x, "Hous\\b", "Housing")
  # Hydrogeol
  x <- str_replace(x, "Hydrogeol\\b", "Hydrogeology")
  # Hydrol
  x <- str_replace(x, "Hydrol\\b", "Hydrology")
  # Ieee
  x <- str_replace(x, "^Ieee\\b", "IEEE")
  # Int = International
  x <- str_replace(x, "Int\\b", "International")
  # J[.]? should be journal, if at end
  x <- str_replace(x, "\\bJ$", "Journal")
  # J[.]? should be journal of, if at start
  x <- str_replace(x, "\\bJ\\,?\\b", "Journal of")
  # Manage, Man = Management
  x <- str_replace(x, "Man\\b|Manage\\b", "Management")
  # Mater = Materials
  x <- str_replace(x, "Mat\\b|Mater\\b", "Materials")
  # Mech = Mechanical
  x <- str_replace(x, "Mech\\b", "Mechanical")
  # Ornith = Ornithology
  x <- str_replace(x, "Ornith\\b", "Ornithology")
  # Psychol = Psychology
  x <- str_replace(x, "Psychol\\b", "Psychology")
  # Sci = Science
  x <- str_replace(x, "Sci\\b", "Science")
  # Seis.= Siesmic
  x <- str_replace(x, "Seism?\\b", "Seismological")
  # Soc = Society
  x <- str_replace(x, "Soc\\b", "Society")
  # Sociol = Sociology
  x <- str_replace(x, "Sociol\\b", "Sociology")
  # Softw = Software
  x <- str_replace(x, "Softw\\b", "Software")
  # Stud
  x <- str_replace(x, "Stud\\b", "Studies")
  # Struct = Structural
  x <- str_replace(x, "Struct\\b", "Structural")
  # Resour. = Resources
  x <- str_replace(x, "Resour\\b", "Resources")
  # Res. = Research
  x <- str_replace(x, "Res\\b", "Research")
  # Rev. = Review at end
  x <- str_replace(x, "Rev$", "Review")
  # Rev. = Review of
  x <- str_replace(x, "Rev\\b", "Review of")
  # Zool = Zoology
  x <- str_replace(x, "Zool\\b", "Zoology")

  # 4. More specific journal conversions

  # Should be "Accident Analysis and Prevention"
  pattern <- c("^Accid\\sAnal$", "^Accid\\sAnal\\sPrev$", "^Accident\\sAnal\\sPrev[a-z]*", "^AccidAnalPrev$", "^Accident\\sAnaly[Ss][Ii][Ss]\\sand\\sPrevention.*$", "^Spatial Patterns Accident Analysis and Prevention$")
  pattern <- paste(pattern, collapse = "|")
  x <- str_replace(x, pattern, "Accident Analysis and Prevention")

  # Should be ACI Structural Journal
  pattern <- c("^ACI\\sStructural\\sJournal\\sv$", "^ACI\\sStructural\\sJournal\\sMarchApril$", "^ACI\\sStructural\\sJournal\\sSP$", "ACI\\sStructural\\sJournal\\sTitle$", "^ACI\\sStructures\\sJournal$", "^American\\sConcrete\\sInstitute\\sACI\\sStructural\\sJournal$")
  pattern <- paste(pattern, collapse = "|")
  x <- str_replace(x, pattern, "ACI Structural Journal")

  # Shoudl be "ACI Materials Journal"
  pattern <- c("ACI Material Journal")
  pattern <- paste(pattern, collapse = "|")
  x <- str_replace(x, pattern, "ACI Materials Journal")

  # Should be "Concrete International"
  pattern <- c("^ACI Concrete International$", "^ACI Concrete Journal$", "^Concr Int$", "^Concrete International\\: Design and Construction$", "^Journal of American Concr Inst Proceedings$")
  pattern <- paste(pattern, collapse = "|")
  x <- str_replace(x, pattern, "Concrete International")

  # Should just be ACI
  pattern <- c("^ACI SP$", "^ACI Special Publication$", "^ACI Monograph$", "^ACI Journal Proceedings$", "^ACI Journal$", "^ACIJournal$", "^ACI Committee$")
  pattern <- paste(pattern, collapse = "|")
  x <- str_replace(x, pattern, "ACI")

  # Anything that starts with AHMCT Should be AHMCT Report
  x <- str_replace(x, "^AHMCT.*", "AHMCT Report")

  # Should be Engineering Journal
  x <- str_replace(x, "^ASHI Engineering Journal$", "Engineering Journal")

  # American Journal of Preventive Medicine
  x <- str_replace(x, "^American Journal of Prev.*", "American Journal of Preventive Medicine")

  # Remove ASCE or ASME before Journal or AISC
  x <- str_remove(x, "^ASCE\\s(?=J)|^ASME\\s(?=J)|\\sASCE$|\\sAISC$")

  # Journal of Geotechnical Engineering should add in envt
  x <- str_replace(x, "^Journal of Geotechnical Engineering$", "Journal of Geotechnical and Geoenvironmental Engineering")

  # BMJ
  x <- str_replace(x, "^Bmj$|^BMJournal$|^Systematic Review Bmj$|^Systematic Review British Medical Journal$", "BMJ Open")

  # Bulletin of the Seismological Society of America
  pattern <- c("^Bulletin of Seismological  Society America$", "^Bulletin of Seismological Society America", "^Bulletin of Seismological Society American Ti$", "^Bulletin of Seismological Society of America$", "^Bulletin of the Seismological Soceity of America$", "^Bulletin of the Seismological Society of American$")
  pattern <- paste(pattern, collapse = "|")
  x <- str_replace(x, pattern, "Bulletin of the Seismological Society of America")

  #Bulletin of Earthquake Engineering
  x <- str_replace(x, "^Bulletin of Earthquake Engineering DOI.*$", "Bulletin of Earthquake Engineering")

  #Bulletin of Engineering Geology and the Environment
  x <- str_replace(x, "^Bulletin of Engineering Geology Environment$", "Bulletin of Engineering Geology and the Environment")

  #Can Journal = Canadian Journal
  x <- str_replace(x, "^Can\\sJournal", "Canadian Journal")
  x <- str_replace(x, "^Canadian Journal of For Res$", "Canadian Journal of Forest Research")
  x <- str_replace(x, "^Canandian Journal of Public Health	$", "Canadian Journal of Public Health	")

  # Canadian Geotechnical Journal
  x <- str_replace(x, "^Can Geotechnical Journal$|^Can Geotechnical Journal of  Ottawa$|^Canadian Geotechnical Journal Journal$", "Canadian Geotechnical Journal")

  # Cement and Concrete Composites
  pattern <- c("^Cem Conc?r? Compo?s?$", "^Cement \\& Concrete Composites$", "^Cement and Concrete Composits$")
  pattern <- paste(pattern, collapse = "|")
  x <- str_replace(x, pattern, "Cement and Concrete Composites")

  # Cement and Concrete Research
  pattern <- c("^Cem Conc?r? Res$", "^Cement Concrete Research$")
  pattern <- paste(pattern, collapse = "|")
  x <- str_replace(x, pattern, "Cement and Concrete Research")

  # Climatic Change
  x <- str_replace(x, "^Climactic\\sChange$|^Climate\\sChange$", "Climatic Change")

  # Composites Part B: Engineering
  pattern <- c("^Composites\\: Part B", "Composites B$")
  pattern <- paste(pattern, collapse = "|")
  x <- str_replace(x, pattern, "Composites Part B\\: Engineering")

  # Composites Part A: Applied Science and Manufacturing
  x <- str_replace(x, "^Composites: Part A$", "Composites Part A: Applied Science and Manufacturing")

  # Comptes Rendus Geoscience
  x <- str_replace(x, "^Comptes Rendus Geoscience$", "Comptes Rendus  Geoscience
")

  # Computational Materials Science
  x <- str_replace(x, "^Computational Material Science$", "Computational Materials Science")

  # Remove Elsevier Ltd
  x <- str_remove(x, "\\sElsevier Ltd$")

  # Conservation Biology Pp
  x <- str_replace(x, "^Conservation Biology Pp$", "Conservation Biology")

  # Construction and Building Materials
  pattern <- c("^Const Build Materials$", "^Construct Build Materials$", "^Construction and Building Material$", "^Construction Bldg Materials$")
  pattern <- paste(pattern, collapse = "|")
  x <- str_replace(x, pattern, "Construction and Building Materials")

  # Corro Review
  x <- str_replace(x, "Corrosion Review", "Corrosion Reviews")

  # Critical Reviews In Environmental Science and Technology
  x <- str_replace(x, "^Critical Reviews In Environmental Science and Toxicology$", "Critical Reviews In Environmental Science and Technology")

  #Earthquake Engineering and Structural Dynamics
  x <- str_replace(x, "^Earhquake Engineering Structural Dyn$|^Earthquake Engin$|^Earthquake Engineering and Structural Dynamics\\(40$|^Earthquake Engineering and Structural DynamicsVol$|^Earthquake Engineering Structural Dyn$|^Earthquake Engng Structural Dyn$|^Engineering Structural Dyn$|^Engineering Structural Dyn$", "Earthquake Engineering and Structural Dynamics")

  #Earth and Planetary Science Letters
  x <- str_replace(x, "^Earth Planet Science Lett$", "Earth and Planetary Science Letters")

  #Earth Surface Processes and Landforms
  x <- str_replace(x, "^Earth Surf Process Landforms$", "Earth Surface Processes and Landforms")

  # Ecological Applications (this is an issue from previous code)
  x <- str_replace(x, "^Ecology Applications$", "Ecological Applications")
  x <- str_replace(x, "^Ecology Econ$", "Ecological Economics")
  x <- str_replace(x, "^Ecology Modell$", "Ecological Modelling")
  x <- str_replace(x, "^Ecology Monogr$", "Ecological Monographs")
  x <- str_replace(x, "^Ecology Res$", "Ecological Research")
  x <- str_replace(x, "^Ecology Society$", "Ecology and Society")

  # Econometrica
  x <- str_replace(x, "^Econometrica\\: journal of the Econometric Society", "Econometrica")

  # EconPapers
  x <- str_replace(x, "^EconPapers$", "Economic Papers")

  # Ecotoxicology and Environmental Safety
  x <- str_replace(x, "^Ecotox\\/cologyand En vironmental Safety$|^Ecotoxlcology and Environmental Safety$", "Ecotoxicology and Environmental Safety")

  # Energy and Fuels
  x <- str_replace(x, "^Energy and Fuels$", "Energy \\& Fuels")

  # Energy Policy 33
  x <- str_replace(x, "^Energy Policy 33$", "Energy Policy")

  # Energy Research & Social Science
  x <- str_replace(x, "^Energy Research \\& Social Science$", "Energy Research and Social Science")

  # Energy Strategy Review
  x <- str_replace(x, "^Energy Strategy Review$", "Energy Strategy Reviews")

  # Engineering Failure Analysis
  x <- str_replace(x, "^Engineering Fail Anal$", "Engineering Failure Analysis")

  # Engineering Structures
  x <- str_replace(x, "^Engineering Structural$|^Engineering Structures.*", "Engineering Structures")

  # Environment 119 and Behavior
  x <- str_replace(x, "^Environment 119 and Behavior$", "Environment and Behavior")

  # Environment and Planning Part A
  x <- str_replace(x, "^Environment and Planning Part A$", "Environment and Planning A")

  # Environment and Planning B\\: Planning and Design
  pattern <- c("^Environment and Planning B$", "^Environment and Planning B\\: Planning and Design 25$", "^Environment and Planning\\: Part B$")
  pattern <- paste(pattern, collapse = "|")
  x <- str_replace(x, pattern, "Environment and Planning B\\: Planning and Design")

  # Environment and Planning C: Government and Policy
  x <- str_replace(x, "^Environment and Planning C\\: Government and Policy$", "Environment and Planning C\\: Government and Policy")

  # Environmental Health Perspectives
  x <- str_replace(x, "^Environmental Health Persp?e?c?t?$|^Environmental Health Perspective$", "Environmental Health Perspectives")

  # Environmental Modeling and Assessment
  x <- str_replace(x, "^Environmental Modeling and Assessment 8$", "Environmental Modeling and Assessment")

  # Environmental Research Letters
  x <- str_replace(x, "^Environmental Research Letter$|^Environmental Res Lett$", "Environmental Research Letters")

  # Environmental Science and Technoiogy
  x <- str_replace(x, "^Environmental Science and Technoiogy$|^Environmental Science and Techn?ology$|^Environmental Science Tech$|^Environmental Science Technology$", "Environmental Science & Technology")

  #Environmental Technology
  x <- str_replace(x, "^Environmental Technology$", "Environmental Technology (United Kingdom)")

  # Epidemiology (Cambridge
  x <- str_replace(x, "^Epidemiology \\(Cambridge$", "Epidemiology")

  # Estuaries Coasts
  x <- str_replace(x, "^Estuaries Coasts$", "Estuaries and Coasts")

  # European Journal of Operational Research
  pattern <- c("^European Journal of of Operational Research$", "^European Journal of Opera-Tional Research$", "^European Journal of Operation Research$|^European Journal of Operational$")
  pattern <- paste(pattern, collapse = "|")
  x <- str_replace(x, pattern, "European Journal of Operational Research")

  # European Transport
  x <- str_replace(x, "^European Transport$", "European Transport  Trasporti Europei")

  # Experiment Smart Materials and Structures
  x <- str_replace(x, "^Experiment Smart Materials and Structures$", "Smart Materials and Structures")

  # Experimental Techniques Structural Testing Series: Part
  x <- str_replace(x, "^Experimental Techniques Structural Testing Series: Part$", "Experimental Techniques")

  # Expert Syst Applied
  x <- str_replace(x, "^Expert Systems with Applications$", "Expert Syst Applied")

  # Explor Geophysics
  x <- str_replace(x, "^Explor Geophysics$", "Exploration Geophysics")

  # Federal register
  x <- str_replace(x, "^Federal Register.*", "Federal Register")

  # Forthcoming Transportation Research Record: Journal of the Transportation Research Board
  x <- str_replace(x, "^Forthcoming Transportation Research Record: Journal of the Transportation Research Board$|^Transportation Research Rec.*|^Metropolitan Area Transportation Research Record Journal of the Transportation Research Board$|^Research Rec.*|^The Journal of Transportation Research Board$", "Transportation Research Record")

  # Foundations and Trends R in Machine Learning
  x <- str_replace(x, "^Foundations and Trends R in Machine Learning$", "Foundations and Trends in Machine Learning")

  # Generation? Journal of the American Planning Association
  x <- str_replace(x, "^Generation\\? Journal of the American Planning Association$|^Longer View Journal of the American Planning Association$", "Journal of the American Planning Association")

  # Geophysics Journal of Int
  x <- str_replace(x, "^Geophysics Journal of Int.*$|^Geophysics Journal of R Astron Society$", "Geophysical Journal International")

  # Geophysical Prospecting
  x <- str_replace(x, "^Geophysics Prosp$", "Geophysical Prospecting")

  # Geophysical Research Letters
  x <- str_replace(x, "^Geophysics Res L?e?t?t?e?r?s?$", "Geophysical Research Letters")

  # Geoscience Cananda
  x <- str_replace(x, "^Geosciences$", "Geoscience Canada")

  # Geosynthet Int
  x <- str_replace(x, "^Geosynthet International$", "Geosynthetics International")

  # Geotechnical Geology Engineering
  x <- str_replace(x, "^Geotechnical Geology Engineering$", "Geotechnical and Geological Engineering")

  # Geotechnical Test Journal
  x <- str_replace(x, "^Geotechnical Test Journal$|^Geotechnical Testing Journal American Society for Testing and Materials$|^Geotechnical Testing Journal ASTM$|^Geotechnical Testing Journal GTJODJournal$", "Geotechnical Testing Journal")

  # Géotechnique
  x <- str_replace(x, "^Géotechnique$", "Geotechnique")

  # Global Ecology and Biogeography Letters
  x <- str_replace(x, "^Global Ecology and Biogeography Letters$", "Global Ecology and Biogeography")

  # Highway Res Rec
  x <- str_replace(x, "^Highway Res Rec$|^Highway Research Record", "Highway Research Record")

  # Human Factors: The Journal of the Human Factors and Ergonomics Society
  x <- str_replace(x, "^Human Factors: The Journal of the Human Factors and Ergonomics Society$", "Human Factors")

  # Hydrogeology Journal
  x <- str_replace(x, "^Hydrogeol Journal$", "Hydrogeology Journal")

  # Hydrol Process
  x <- str_replace(x, "^Hydrology Process$", "Hydrology Processes")

  # IEEE Conference on Intelligent Transportation System
  x <- str_replace(x, "^IEEE Conference on Intelligent Transportation System$|^IEEE Intelligent Transportation Systems|^IEEE ITS Conference$", "IEEE Conference on Intelligent Transportation Systems Proceedings ITSC")

  # IEEE 66th Vehicular Technology Conference (VTC
  x <- str_replace(x, "^IEEE \\d\\dth Vehicular Technology Conference|^IEEE \\d\\dnd Vehicular Technology Conference", "IEEE Vehicular Technology Conference")

  #IEEE Intelligent Vehicles Symposium Proceedings
  x <- str_replace(x, "^IEEE Intelligent Vehicles Symposium|^IEEE on Intelligent Vehicles Symposium", "IEEE Intelligent Vehicles Symposium Proceedings")

  # IEEE Journal of Select Topics Applied Earth Obs Remote Sens
  x <- str_replace(x, "^IEEE Journal of Select Topics Applied Earth Obs Remote Sens$", "IEEE Journal of Selected Topics in Applied Earth Observations and Remote Sensing")

  # IEEE Journal on Selected Areas in Communications/Supplement
  x <- str_replace(x, "^IEEE Journal on Selected Areas in Communications/Supplement$", "IEEE Journal on Selected Areas in Communications")

  # IEEE Trans Geosci Remote Sens
  x <- str_replace(x, "^IEEE Trans Geosci Remote Sens$|^IEEE Transact Geosci Remote Sens$|^Journal of IEEE Transact Geosci Remote Sensing$", "IEEE Transactions on Geoscience and Remote Sensing")

  # IEEE Trans Instrum Meas
  x <- str_replace(x, "^IEEE Trans Instrum Meas$", "IEEE Transactions on Instrumentation and Measurement")

  # IEEE Trans Intell Transp Syst
  x <- str_replace(x, "^IEEE Trans Intell Transp Syst$|^IEEE Trans on Intel Trans Syst?
$|^IEEE Trans on Intelligent Transportation Systems$|^IEEE Transactions in ITS$|^IEEE Transaction on Intelligent Transportation Systems$|^IEEE Transportation on Intelligent Transportation Systems$|^Intelligent Transportation Systems IEEE Transactions on$", "IEEE Transactions on Intelligent Transportation Systems")

  # IEEE Trans on Education
  x <- str_replace(x, "^IEEE Trans on Education$", "IEEE Transactions on Education")

  # IEEE Trans on Inform Theory
  x <- str_replace(x, "^IEEE Trans on Inform Theory$", "IEEE Transactions on Information Theory")

  # IEEE Trans on Signal Processing
  x <- str_replace(x, "^IEEE Trans on Signal Processing$", "IEEE Transactions on Signal Processing")

  # IEEE Trans on Veh Tech
  x <- str_replace(x, "^IEEE Trans on Veh Tech$|^IEEE Transactions on v Ehicular Technology$", "IEEE Transactions on Vehicular Technology")

  # IEEE Transactions Automatic Control to Appear
  x <- str_replace(x, "^IEEE Transactions Automatic Control to Appear$", "IEEE Transactions on Automatic Control")

  # IEEE Transactions on Control System Technology
  x <- str_replace(x, "^IEEE Transactions on Control System Technology$|^IEEE Transactions on Control Systems$|^IEEE TRANSACTIONS on CONTROL SYSTEMS TECHNOLOGY$", "IEEE Transactions on Control Systems Technology")

  # IEEE Transactions on Systems Man and Cybernetics
  x <- str_replace(x, "^IEEE Transactions on Systems Man and Cybernetics$", "IEEE Transactions on Systems Man and Cybernetics\\: Systems")

  # IEEE Vehicular Networking Conference (VNC) Amsterdam (the Netherlands
  x <- str_replace(x, "^IEEE Vehicular Networking Conference \\(VNC\\) Amsterdam \\(the Netherlands$", "IEEE Vehicular Networking Conference VNC")

  # IEEE Vehicular Technol Mag
  x <- str_replace(x, "^Vehicular Technol Mag$", "Vehicular Technology Magazine")

  # IEEE Wireess Communications
  x <- str_replace(x, "^IEEE Wireess Communications$|^IEEEE Wireless Telecommunications Symposium$", "IEEE Wireless Communications")

  # IEEE/ASME Transactions on Mechanics
  x <- str_replace(x, "^IEEE\\/ASME Transactions on Mechanics$|^IEEE/ASME TRANSACTIONS on MECHATRONICS$", "IEEE\\/ASME Transactions on Mechatronics")

  # IEEJ Transactions on Electrical and Electronic Engineering
  x <- str_replace(x, "^IEEJournal of Transactions on Electric and Environmental Engineering$", "IEEJ Transactions on Electrical and Electronic Engineering")

  # IET Journal of Intelligent Transportation Systems
  x <- str_replace(x, "^IET Journal of Intelligent Transportation Systems$", "IET Intelligent Transportation Systems")

  #  Industrial & Engineering Chemistry Research
  x <- str_replace(x, "^Indust Engineering Chem$|^Industrial and Engineering Chemistry Research$", "Industrial & Engineering Chemistry Research")

  # Industrial Electronics IEEE Transactions on
  x <- str_replace(x, "^Industrial Electronics IEEE Transactions on$", "IEEE Transactions on Industrial Electronics")

  # Inhalation Toxicology
  x <- str_replace(x, "^Inhal Toxicol$", "Inhalation Toxicology")

  # Inj Prev
  x <- str_replace(x, "^Inj Prev$|^Injury Prevention \\[serial Online$", "Injury Prevention")

  # Internation Journal of Project Management
  x <- str_replace(x, "^Internation Journal of Project Management$", "International Journal of Project Management")

  # International Journal of Solids and Structures
  x <- str_replace(x, "^Internation Journals of Solids and Structures$", "International Journal of Solids and Structures")

  # ITE Journal (Institute of Transportation Engineers)
  pattern <- c("^Institute of Transportation Engineering Journal$", "^Institute of Transportation Engineers ITE Journal$", "^Institute of Transportation Engineers Journal$")
  pattern <- paste(pattern, collapse = "|")
  x <- str_replace(x, pattern, "ITE Journal \\(Institute of Transportation Engineers\\)")

  # International Archives of the Photogrammetry Remote Sensing and Spatial Information Sciences  ISPRS Archives
  x <- str_replace(x, "^International Archives of Photogrammetry and Remote Sensing$|^International Archives of Photogrammetry Remote Sensing and Spatial Information Sciences$", "International Archives of the Photogrammetry Remote Sensing and Spatial Information Sciences  ISPRS Archives")

  # International Journal of Autonomous and Adaptive Communications Systems
  x <- str_replace(x, "^International Journal of Autonomous and Adaptive Communications Systems \\(IJAACS$", "International Journal of Autonomous and Adaptive Communications Systems")

  # International Journal of Environmental Research and Public Health
  x <- str_replace(x, "^International Journal of Environmental Res Public Health$", "International Journal of Environmental Research and Public Health")

  # International Journal of Health Geographics
  x <- str_replace(x, "^International Journal of Health Geogr$|^International Journal of Health Geographic Associations Between Street Governments Office of Planning and Research$|^International Journal of Health Geography$", "International Journal of Health Geographics")

  # International Journal of Industrial Engineering : Theory Applications and Practice
  x <- str_replace(x, "^International Journal of Industrial Engineering$", "International Journal of Industrial Engineering \\: Theory Applications and Practice")

  # International Journal of ITS
  x <- str_replace(x, "^International Journal of ITS$|^International Journal of ITS Research$|^International Journal of of Intelligent Transportation Systems$", "International Journal of Intelligent Transportation Systems Research")

  # International Journal of Life Cycle Assess
  x <- str_replace(x, "^International Journal of Life Cycle Assess$", "International Journal of Life Cycle Assessment")

  # International Journal of Logistics: Research and Applications
  x <- str_replace(x, "^International Journal of Logistics\\: Research and Applications$", "International Journal of Logistics Research and Applications")

  # International Journal of of Civil Engineering
  x <- str_replace(x, "^International Journal of of Civil Engineering$", "International Journal of Civil Engineering")

  # International Journal of Hydrogen Energy
  x <- str_replace(x, "^International Journal of of Hydrogen Energy$", "International Journal of Hydrogen Energy")

  # International Journal of of Steel Structures
  x <- str_replace(x, "^International Journal of of Steel Structures$|^International Journal of Steel Structural$|^International Journal of Steel Structure$|^International Journal of Steel Structures KSSC$", "International Journal of Steel Structures")

  # International Journal of Plant Science
  x <- str_replace(x, "^International Journal of Plant Science$", "International Journal of Plant Sciences")

  # International Journal of Remote Sens
  x <- str_replace(x, "^International Journal of Remote Sens$|^International Journal of Remote Sensing  in Review$", "International Journal of Remote Sensing")

  # International Journal of Rock Mechanics and Minings Sciences
  x <- str_replace(x, "^International Journal of Rock Mechanical Min Science$|^International Journal of Rock Mechanical Mining Science$|^International Journal of Rock Mechanical Mining Science Geomech Abs$|^International Journal of Rock Mechanics and Mining Sciences and Geomechanics$", "International Journal of Rock Mechanics and Minings Sciences")

  # International Journal of Sustain Transp
  x <- str_replace(x, "^International Journal of Sustain Transp$", "International Journal of Sustainable Transportation")

  # Intl
  x <- str_replace(x, "Intl\\b", "International")

  # ITE Journal (Institute of Transportation Engineers)
  x <- str_replace(x, "^ITE Journal.*", "ITE Journal \\(Institute of Transportation Engineers\\)")

  # Journal of Environmental and Engineering Geophysics
  x <- str_replace(x, "^JEEG$|^JGeotechnical and Geoenvironmental Engineering$", "Journal of Environmental and Engineering Geophysics")

  # Journal of Materials in Civil Engineering
  x <- str_replace(x, "^Jorunal of Material in Civil Engineering$", "Journal of Materials in Civil Engineering")

  # Journal
  x <- str_replace(x, "Jour$", "Journal")
  x <- str_replace(x, "Jour\\b", "Journal of")

  # Wild Mgmt
  x <- str_replace(x, "^Journal of Wild Mgmt$", "Journal of Wildlife Management")

  # Journal of Engineering Mechanics  ASCE
  x <- str_replace(x, "^Journal ASCE Engineering Mechanics Div$|^Theory Journal of Engineering MechanicsAsce$", "Journal of Engineering Mechanics  ASCE")

  # Journal of the Acoustical Society of America
  x <- str_replace(x, "^Journal of Acoust Society America$|^Journal of Acoustical Society of America$|^The Journal of the Acoustical Society of America$", "Journal of the Acoustical Society of America")

  # Journal of Advanced Concrete Technology
  x <- str_replace(x, "^Journal of Advances in Concr Tech$", "Journal of Advanced Concrete Technology")

  # Journal of Agricultural Engineering
  x <- str_replace(x, "^Journal of Agricult Engineering Res$|^Journal of Agricultural Engineering Research$|^Journal of Agriculture Engng Res$", "Journal of Agricultural Engineering")

  # JAMA  Journal of the American Medical Association
  x <- str_replace(x, "^Journal of American Med Association$|^Journal of the American Medical Association$", "JAMA  Journal of the American Medical Association")

  # JAMA  Journal of the American Medical Association
  x <- str_replace(x, "^Journal of American Plan Association$|^Journal of American Planning Association$", "Journal of the American Planning Association")

  # Journal of the American Society for Horticultural Science
  x <- str_replace(x, "^Journal of American Society Hort Science$|^Journal of American Society Hortic Science$", "Journal of the American Society for Horticultural Science")

  # App
  x <- str_replace(x, "App\\b", "Applied")
  # Electrochem
  x <- str_replace(x, "Electrochem\\b", "Electrochemistry")

  # Journal of Applied Mechanics Transactions ASME
  x <- str_replace(x, "^Journal of Applied Mechanics$|^Journal of Applied Mechanics ASME$|^Journal of Applied MechanicsTransactions of the Asme$", "Journal of Applied Mechanics Transactions ASME")

  # Journal of Applied Meteor
  x <- str_replace(x, "^Journal of Applied Meteor.*", "Journal of Applied Meteorology and Climatology")

  # Phys
  x <- str_replace(x, "Phys\\b$", "Physics")
  # Sens
  x <- str_replace(x, "Sens\\b$", "Sensing")
  # Archaeol
  x <- str_replace(x, "Archaeol\\b$", "Archaeological")

  # Journal of Arid Environment
  x <- str_replace(x, "^Journal of Arid Environment.*", "Journal of Arid Environments")

  # Journal of Atmospheric and Oceanic Technology
  x <- str_replace(x, "^Journal of Atmospheric Oceanic Technol$", "Journal of Atmospheric and Oceanic Technology")

  # Journal of Bridge Engineering
  x <- str_replace(x, "^Journal of Bridge Engineeing$|^Journal of Bridge Engineering ©$", "Journal of Bridge Engineering")

  # Journal of Can Pet Technol
  x <- str_replace(x, "^Journal of Can Pet Technol$", "Journal of Canadian Petroleum Technology")

  # Journal of Central South University
  x <- str_replace(x, "^Journal of Cent South Univ$", "Journal of Central South University")

  # KSCE Journal of Civil Engineering
  x <- str_replace(x, "^Journal of Civil Engineering$", "KSCE Journal of Civil Engineering")

  # Journal of Clean Prod
  x <- str_replace(x, "^Journal of Clean Prod$", "Journal of Cleaner Production")

  # Journal of Clim
  x <- str_replace(x, "^Journal of Clim$", "Journal of Climate")

  # Journal of Clinical Epidemihttp://Wwwnhtsadotgov/Portal/Site/Nhtsa/Menuitemdfedd570f698cabbbf Ology
  x <- str_replace(x, "^Journal of Clinical Epid.*", "Journal of Clinical Epidemiology")

  # Journal of Cold Regions Engineering  ASCE
  x <- str_replace(x, "^Journal of Cold Regions Engineering$", "Journal of Cold Regions Engineering  ASCE")

  # Journal of Combinatorial Theory Series B
  x <- str_replace(x, "^Journal of Combinatorial Theory B$", "Journal of Combinatorial Theory Series B")

  # Journal of Comp Electro
  x <- str_replace(x, "^Journal of Comp Electro$", "Journal of Computational Electronics")

  # Journal of Compos Construction
  x <- str_replace(x, "^Journal of Compos Construction$|^Journal of Composite for Construction$|^StateoftheArt Review Journal of Composites for Construction$", "Journal of Composites for Construction")

  # Journal of Computers and Structures
  x <- str_replace(x, "^Journal of Computers and Structures$", "Computers and Structures")

  # Journal of Computing Civil Engineering
  x <- str_replace(x, "^Journal of Computing Civil Engineering$", "Journal of Computing in Civil Engineering")

  # Journal of Computational and Graphical Statistics
  x <- str_replace(x, "^Journal of Computational and Graphical Statistics$", "Journal of Computational and Graphical Statistics")

  # Journal of Construction Engineering and Management  ASCE
  x <- str_replace(x, "^Journal of Construction Engi$|^Journal of Construction Engineering and Management$|^Journal of Construction Engineering Manage$|^Journalof Construction Engineering and Management$", "Journal of Construction Engineering and Management  ASCE")

  # Journal of Structural Engineering
  x <- str_replace(x, "^Journal of Constructional Steel$", "Journal of Structural Engineering")

  # Journal of Contaminant Hydrology
  x <- str_replace(x, "^Journal of Conta?m? Hydrology$", "Journal of Contaminant Hydrology")

  # Journal of Dynamic Systems Measurement and Control Transactions of the ASME
  x <- str_replace(x, "^Journal of Dynamics? Systems Measurement and Control$", "Journal of Dynamic Systems Measurement and Control Transactions of the ASME")

  # Journal of Earthquake Engineering
  x <- str_replace(x, "^Journal of Earthquake$|^Journal of Earthquake Engineering Taylor and Francis UK$", "Journal of Earthquake Engineering")

  # Econ
  x <- str_replace(x, "Econ\\b", "Economic")

  # Journal of Emerg Nurs
  x <- str_replace(x, "^Journal of Emerg Nurs$", "Journal of Emergency Nursing")

  # Journal of Engineering Gas Turbines Power
  x <- str_replace(x, "^Journal of Engineering Gas Turbines Power$", "Journal of Engineering for Gas Turbines and Power")

  # Journal of Engineering Mechanics  ASCE
  x <- str_replace(x, "^Journal of Engineering Mechanical$|^Journal of Engineering Mechanics$|^Journal of Engineering MechanicsAsce$", "Journal of Engineering Mechanics  ASCE")

  # Journal of Engineering and Technology Management  JETM
  x <- str_replace(x, "^Journal of Engineering Tech Manage$", "Journal of Engineering and Technology Management  JETM")

  # Journal of Environmental Engineering ASCE
  x <- str_replace(x, "^Journal of Environmental Engineering$", "Journal of Environmental Engineering ASCE")

  # Journal of Environmental and Engineering Geophysics
  x <- str_replace(x, "^Journal of Environmental Engineering Geophysics$", "Journal of Environmental and Engineering Geophysics")

  # Hort
  x <- str_replace(x, "Hort\\b", "Horticulture")

  #  Journal of Environmental Plann Manage
  x <- str_replace(x, "^Journal of Environmental Plann Manage$", "Journal of Environmental Planning and Management")

  # Journal of Epidemiology and Community Health
  x <- str_replace(x, "^Journal of Epidemiology Commun$|^Journal of Epidemiology Community$", "Journal of Epidemiology and Community Health")

  # Journal of Fluid Mechanical
  x <- str_replace(x, "^Journal of Fluid Mechanical$", "Journal of Fluid Mechanics")

  # Journal of Fluids Engineering Transactions of the ASME
  x <- str_replace(x, "^Journal of Fluids Engineering$", "Journal of Fluids Engineering Transactions of the ASME")

  # Journal of Forestry
  x <- str_replace(x, "^Journal of Forestry.*", "Journal of Forestry")

  # Journal of General Internal Medicine
  x <- str_replace(x, "^Journal of Gen Intern Med$", "Journal of General Internal Medicine")

  # Journal of Geophysical Resear
  x <- str_replace(x, "^Journal of Geophysical Resear$|^Journal of Geophysical Research\\: Earth Surface$|^Journal of Geophysics Res$|^Journal of Geophysics Res: Solid Earth$", "Journal of General Internal Medicine")

  # Journal of Geotechnical and Geoenvironmental Engineering  ASCE
  x <- str_replace(x, "^Journal of Geotechnical and Geo Environmental Engineering Division$|^Journal of Geotechnical and Geo[Ee]nvironmental Engineering$|^Journal of Geotechnical and Geoenvironmental Engrg$|^Journal of Geotechnical Geoen Engineering$|^Journal of Geotechnical Geoenvironmental$|^Journal of Geotechnical Geoenvironmental Engineering$|^Journal of Geot Engineering Div$|^Journal of Geotechnical Engineering Div$|^Journal of Geotechnical Engrg\\s?D?i?v?$|^Journal of Geotechnical Geoen Engineering$|^Journal of Geotechnical Geoenvironmental$|^Journal of Geotechnical Geoenvironmental Engineering$|^Journal of Geotechnical and Geoenvironmental Engineering JOUR American Society of Civil Engineers$|^Journal of Geotechnical Engg$|^Journal of Geotechnical Geoenvironmental Engineering$", "Journal of Geotechnical and Geoenvironmental Engineering  ASCE")

  # Journal of Geotechnical Engineering , I am calling the one abocve

  # Journal of Health and Social Behavior
  x <- str_replace(x, "^Journal of Health Society$", "Journal of Health and Social Behavior")

  # Journal of Housing and the Built Environment
  x <- str_replace(x, "^Journal of Housing Built Environment$", "Journal of Housing and the Built Environment")

  # Journal of Housing Economic
  x <- str_replace(x, "^Journal of Housing Economic$", "Journal of Housing Economics")

  # Journal of Housing Economic
  x <- str_replace(x, "^Journal of Hydraulic Research$", "Journal of Hydraulic Research\\/De Recherches Hydrauliques")

  # Hydro
  x <- str_replace(x, "Hydro\\b", "Hydrology")

  # Journal of Hydrologic Engineering  ASCE
  x <- str_replace(x, "^Journal of Hydrologic Engineering$", "Journal of Hydrologic Engineering  ASCE")

  # Hydrometeorology
  x <- str_replace(x, "Hydrometeor\\b", "Hydrometeorology")

  # Journal of Industrial and Engineering Chemistry
  x <- str_replace(x, "^Journal of Ind Engineering Chem$", "Journal of Industrial and Engineering Chemistry")

  # Ind
  x <- str_replace(x, "\\bInd\\b", "Industrial")

  # Journal of Insect
  x <- str_replace(x, "^Journal of Insect$", "Journal of Insect Conservation")

  # Journal of Intell Transp Syst: Technol Planning and Operations
  x <- str_replace(x, "^Journal of Intell Transp Syst\\: Technol Planning and Operations$|^Journal of Intelligent Transportation System$", "Journal of Intelligent Transportation Systems")

  # Journal of Intelligent and Robotic Systems: Theory and Applications
  x <- str_replace(x, "^Journal of Intell Transp Syst: Technol Planning and Operations$|^Journal of Intelligent and Robotic Systems$", "Journal of Intelligent and Robotic Systems\\: Theory and Applications")

  # Journal of Irrigation and Drainage Engineering  ASCE
  x <- str_replace(x, "^Journal of Irrig and Drain Engrg$|^Journal of Irrigation and Drainage Engineering$", "Journal of Irrigation and Drainage Engineering  ASCE")

  # Journal of Mach Learn Res
  x <- str_replace(x, "^Journal of Mach Learn Res$", "Journal of Machine Learning Research")

  # Journal of Management in Engineering  ASCE
  x <- str_replace(x, "^Journal of Management in Engineering$", "Journal of Management in Engineering  ASCE")

  # Journal of Materials in Civil Engineering
  x <- str_replace(x, "^Journal of Materials Civil Engineering$|^Journal of Materials Civil Engrg$|^Journal of Materials in Civil Engrg$", "Journal of Materials in Civil Engineering")

  # Mathematics and Computers in Simulation
  x <- str_replace(x, "^Journal of Mathematics and Computers in Simulation$", "Mathematics and Computers in Simulation")

  # Journal of Mechanical Design Transactions of the ASME
  x <- str_replace(x, "^Journal of Mechanical Design$", "Journal of Mechanical Design Transactions of the ASME")

  # Journal of Mechanical Engineering and Sciences
  x <- str_replace(x, "^Journal of Mechanical Engineering Science$", "Journal of Mechanical Engineering and Sciences")

  # Journal of Modern Transportation
  x <- str_replace(x, "^Journal of Mod Transp$", "Journal of Modern Transportation")

  # Journal of Modern Transportation
  x <- str_replace(x, "^Journal of of", "Journal of")

  # Journal of Multivariate Analysis
  x <- str_replace(x, "^Journal of Multivar Anal$", "Journal of Multivariate Analysis")

  # Journal of Petroleum Science and Engineering
  x <- str_replace(x, "^Journal of Pet Science Engineering$", "Journal of Petroleum Science and Engineering")

  # Journal of Petroleum Technology -- not a journal but worth standarizing
  x <- str_replace(x, "^Journal of Petr? Techn?o?l?$|^Journal of Pet Tech Transactions AIME$", "Journal of Petroleum Technology")

  # Journal of Petroleum Science and Engineering
  x <- str_replace(x, "^Journal of Petrol Science Engineering$", "Journal of Petroleum Science and Engineering")

  # Journal of Pharmaceutical Sciences
  x <- str_replace(x, "^Journal of Pharm Science$", "Journal of Pharmaceutical Sciences")

  # Phycol
  x <- str_replace(x, "Phycol\\b", "Phycology")

  # Journal of Phys Act Health
  x <- str_replace(x, "^Journal of Phys Act Health$|^Journal of Physical Activity and Health$|^Journal of Physical Activity$", "Journal of Physical Activity & Health")

  # Journal of Plan Educ
  x <- str_replace(x, "^Journal of Plan Educ$|^Journal of Plan Educ Res$", "Journal of Planning Education and Research")

  # Journal of Planning Literature
  x <- str_replace(x, "^Journal of Plan Lit$", "Journal of Planning Literature")

  # Journal of Policy Analysis and Managment
  x <- str_replace(x, "^Journal of Policy Analysis and Managment$", "Journal of Policy Analysis and Management")

  # Journal of Public Economic
  x <- str_replace(x, "^Journal of Public Economic$", "Journal of Public Economics")

  # Journal of Public Health Management and Practice : JPHMP
  x <- str_replace(x, "^Journal of Public Health Management and Practice$", "Journal of Public Health Management and Practice \\: JPHMP")

  # Journal of Public Transportation
  x <- str_replace(x, "^Journal of Public Transp$|^Journal of Public Transportation Research$", "Journal of Public Transportation")

  #Journal of the Royal Statistical Society Series a (Statistics in Society)
  x <- str_replace(x, "^Journal of R Statist Society a$", "Journal of the Royal Statistical Society Series a \\(Statistics in Society\\)")

  # Journal of R Stat Society Ser B
  x <- str_replace(x, "^Journal of R Stat Society Ser B$|^Journal of the Royal Statistical Society Series B.*|^Journal of the Royal Statistical Society$", "Journal of the Royal Statistical Society Series B\\: Statistical Methodology")

  # Journal of Real Estate Economics
  x <- str_replace(x, "^Journal of Real Estate Economics$|^Journal of Real Estate Financ Economic$|^The Journal of Real Estate Finance and Economics$|^The Journal of Real Estate Finance Economics$", "Journal of Real Estate Finance and Economics")

  # Journal of Real Estate Research
  x <- str_replace(x, "^Journal of Real Estate Res$|^The Journal of Real Estate Research$", "Journal of Real Estate Research")

  # Journal of Rock Mechanics and Geotechnical Engineering
  x <- str_replace(x, "^Journal of Rock Mechanical Geotechnical Engineering$", "Journal of Rock Mechanics and Geotechnical Engineering")

  # Res
  x <- str_replace(x, "Res\\b", "Research")

  # Journal of Soils and Water Conservation
  x <- str_replace(x, "^Journal of Soil and Water Conservation.*", "Journal of Soils and Water Conservation")

  # Journal of the Soil Mechanics and Foundations Division -- not a journal in scimago but really popular
  x <- str_replace(x, "^Journal of Soil$|^Journal of Soil Mechanical Fdns Div$|^Journal of Soil Mechanical Found$|^Journal of Soil Mechanical Found Div$|^Journal of Soil Mechanical Found Engin Div American Society Civil Engin$|^Journal of Soil Mechanical Nc Fdns Div$|^Journal of Soil Mechanics and Foundation.*|^Journal of the Soil Mechanics and Foundation$|^Journal of the Soil Mechanics and Foundations Division.*|^JSoil Mechanical and Foundations Div$", "Journal of the Soil Mechanics and Foundations Division")

  # Journal of Sound and Vibration
  x <- str_replace(x, "^Journal of Sound Vib$", "Journal of Sound and Vibration")

  # Journal of Southeast University (English Edition)
  x <- str_replace(x, "^Journal of Southeast University \\(English Edition$", "Journal of Southeast University \\(English Edition\\)")

  # Journal of Strain Analysis for Engineering Design
  x <- str_replace(x, "^Journal of Strain Analysis$", "Journal of Strain Analysis for Engineering Design")

  # Journal of Structural and Construction Engineering
  x <- str_replace(x, "^Journal of Structural Construction Engineering$", "Journal of Structural and Construction Engineering")

  # Journal of Structural Engineering
  x <- str_replace(x, "^Journal of Structural Div$|^Journal of Structural Engineering.*|^Journal of Structural Engrg$|^Journal of the Structural Engineering$|^Journal of the Structural Division Proceedings of the American Society of Civil Engineers$", "Journal of Structural Engineering")

  # Journal of Studies on Alcohol and Drugs
  x <- str_replace(x, "^Journal of Studies Alcohol$", "Journal of Studies on Alcohol and Drugs")

  # Journal of the American Concrete Institute
  x <- str_replace(x, "^Journal of American Concrete Institute$|^Journal of the American Concrete Institute$|^Materials Journal$", "ACI Materials Journal")

  # Journal of the American Water Resources Association
  x <- str_replace(x, "^Journal of the American Water Resources Association \\(JAWRA
$", "Journal of the American Water Resources Association")

  # Asphalt Paving Technology: Association of Asphalt Paving TechnologistsProceedings of the Technical Sessions
  x <- str_replace(x, "^Journal of the Association of Asphalt Pavement Technologists
$|^Journal of the Association of Asphalt Paving Technologists$", "Asphalt Paving Technology\\: Association of Asphalt Paving TechnologistsProceedings of the Technical Sessions")

  # Journal of the Chinese Institute of Engineers Transactions of the Chinese Institute of EngineersSeries a/Chungkuo Kung Ch'eng Hsuch K'an
  x <- str_replace(x, "^Journal of the Chinese Institute of Engineers$", "Journal of the Chinese Institute of Engineers Transactions of the Chinese Institute of EngineersSeries a\\/Chungkuo Kung Ch\\'eng Hsuch K\\'an")

  # Journal of the South African Institution of Civil Engineers
  x <- str_replace(x, "^Journal of the South African Institution of Civil Engineers$", "Journal of the South African Institution of Civil Engineering")

  # Transportation Research Record
  x <- str_replace(x, "^Journal of the Transportation Research Board$|^Journal of the Transportation Research Forum$|^Journal of Transportation Research$|^Journal of Transportation Research Record$", "Transportation Research Record")

  # Journal of Toxicology and Environmental Health
  x <- str_replace(x, "^Journal of Toxicology and Environmental HealthPart aCurrent Issues$|^Journal of Toxicology and Environmental Health$", "Journal of Toxicology and Environmental Health  Part a")

  # Trans
  x <- str_replace(x, "\\bTransp?\\b", "Transportation")
  # Geog
  x <- str_replace(x, "\\bGeog\\b", "Geography")

  # Transportation Engineering
  x <- str_replace(x, "^Journal of Transportation$|^Journal of Transportation Enginee1ing$|^Journal of Transportation Engineering American Society of Civil Engineers$|^Journal of Transportation EngineeringAsce$", "Journal of Transportation Engineering")

  # Journal of Transport and Land Use
  x <- str_replace(x, "^Journal of Transpotation L Use$|^Journal of Transport and Land Use \\(JTLU", "Journal of Transport and Land Use")

  # Journal of Transp Stat -- this is a government joirnal
  x <- str_replace(x, "^Journal of Transp Stat$", "Journal of Transportation and Statistics")

  # Journal of Transport Economics Policy
  x <- str_replace(x, "^Journal of Transport Economics Policy$", "Journal of Transport Economics and Policy")

  # Journal of Transportation Geography
  x <- str_replace(x, "^Journal of Transportation Geography$", "Journal of Transport Geography")

  # Journal of Trauma and Acute Care Surgery
  x <- str_replace(x, "^Journal of Trauma$", "Journal of Trauma and Acute Care Surgery")

  # Journal of Urban Technology
  x <- str_replace(x, "^Journal of Urban$", "Journal of Urban Technology")

  # Journal of Urban Economic
  x <- str_replace(x, "^Journal of Urban Economic.*", "Journal of Urban Economics")

  # Journal of Urban Planning and
  x <- str_replace(x, "^Journal of Urban Planning and$|^Journal of Urban Planning and DevelopmentAsce$", "Journal of Urban Planning and Development")

  # Journal of Vibration and Acoustics Transactions of the ASME
  x <- str_replace(x, "^Journal of Vibration and Acoustics.*", "Journal of Vibration and Acoustics Transactions of the ASME")

  # Journal of Water Resources Planning and Management  ASCE
  x <- str_replace(x, "^Journal of Water Resources Planning and ManagementAsce", "Journal of Water Resources Planning and Management  ASCE")

  # Journal of Waterway Port Coastal and Ocean Engineering
  x <- str_replace(x, "^Journal of Waterway Port Coastal and Ocean Engineering American Society of Civil Engineering$", "Journal of Waterway Port Coastal and Ocean Engineering")

  # Journal of Wil
  x <- str_replace(x, "^Journal of Wil.*|^JWidl Mgmt$", "Journal of Wildlife Management")

  # Journal of Wind Engineering and Industrial Aerodynamics
  x <- str_replace(x, "^Journal of Wind Engineering Industrial Aerodyn$", "Journal of Wind Engineering and Industrial Aerodynamics")

  # Journal of Zoology (London
  x <- str_replace(x, "^Journal of Zoology \\(London$", "Journal of Zoology")

  # Land Economics
  x <- str_replace(x, "^Land Economic$", "Land Economics")

  # Landscape and Urban Planning
  x <- str_replace(x, "^Landsc Urban Plan$|^Landscale and Urban Planning$|^Landscape and Planning$|^Landscaping and Urban Planning$", "Landscape and Urban Planning")

  # Lect Notes in Control and Inf
  x <- str_replace(x, "^Lect Notes in Control and Inf$", "Lecture Notes in Control and Information Sciences")

  # New England Journal of Medicine
  x <- str_replace(x, "^Life Expectancy the New England Journal of Medicine$|^NEJM
$", "New England Journal of Medicine")

  # Lighting Design and Application: LD and a
  x <- str_replace(x, "^Lighting Design and Application$", "Lighting Design and Application: LD and a")

  # unclear why Lighting Research and Technology is not a match

  # Limnol Oceanogr Methods
  x <- str_replace(x, "^Limnol Oceanogr Methods$", "Limnology and Oceanography\\: Methods")

  # Mag Concr Research
  x <- str_replace(x, "^Mag Concr Research$", "Magazine of Concrete Research")

  # Mar = Marine (I hope this doesn't mess up future March tags)
  x <- str_replace(x, "Mar\\b", "Marine")

  # Marine Ecology  Progress Series
  x <- str_replace(x, "^Marine Ecology Prog Ser$|^Marine Ecology Progress Series$", "Marine Ecology  Progress Series")

  # Marine and Petroleum Geology
  x <- str_replace(x, "^Marine Pet Geology", "Marine and Petroleum Geology")

  # Materials and Structures
  x <- str_replace(x, "^Materials and Structures$|^Materials Structural$|^Materiaux et Constructions$", "Materials and Structures\\/Materiaux et Constructions")

  # Materials Chemistry and Physics
  x <- str_replace(x, "^Materials Chem Physics$", "Materials Chemistry and Physics")

  # Materials and Corrosion  Werkstoffe Und Korrosion
  x <- str_replace(x, "^Materials Corrosion$", "Materials and Corrosion  Werkstoffe Und Korrosion")

  # Materials Evaluation
  x <- str_replace(x, "^Materials Evaluation Journal American Society for Nondestructive Testing$", "Materials Evaluation")

  # Materials Perform
  x <- str_replace(x, "^Materials Perform$", "Materials Performance")

  # Materials Phys Chem
  x <- str_replace(x, "^Materials Phys Chem$", "Materials Chemistry and Physics")

  # Materials Science and Engineering
  x <- str_replace(x, "^Materials Science and Engineering$", "Materials Science & Engineering A\\: Structural Materials\\: Properties Microstructure and Processing")

  # Mathematical Methods in the Applied Sciences
  x <- str_replace(x, "^Math Meth Applied Science$", "Mathematical Methods in the Applied Sciences")

  # Mathematical Programming Series B
  x <- str_replace(x, "^Mathematical Programming$|^Mathematical Programming SerB$", "Mathematical Programming Series B")

  # Measurement Science and Technology
  x <- str_replace(x, "^Meas Science Technol$", "Measurement Science and Technology")

  # Mechanical Systems and Signal Processing
  x <- str_replace(x, "^Mechanical Syst Signal Process$", "Mechanical Systems and Signal Processing")

  # Mechatronics IEEE/ASME Transactions on
  x <- str_replace(x, "^Mechatronics IEEE/ASME Transactions on$", "Mechatronics")

  # Medicine and Science in Sports and Exercise
  x <- str_replace(x, "^Medicine and Science in Sports Exercise$", "Medicine and Science in Sports and Exercise")

  # Metallurgical and Materials Transactions a
  x <- str_replace(x, "^Metallurgical and Materials Transactions a$|^Metallurgical Transactions a$", "Metallurgical and Materials Transactions A\\: Physical Metallurgy and Materials Science")

  # Mol
  x <- str_replace(x, "Mol\\b", "Molecular")

  # Molecular Biology Evol
  x <- str_replace(x, "^Molecular Biology Evol$", "Molecular Biology and Evolution")

  # Nat Clim Chang
  x <- str_replace(x, "^Nat Clim Chang$", "Nature Climate Change")

  #Nat Research
  x <- str_replace(x, "^Nat Research$", "Natural Resources Forum")

  # Natural Hazards and Earth System Sciences
  x <- str_replace(x, "^Natural Hazards and Earth System Science$", "Natural Hazards and Earth System Sciences")

  # Natural Resource Journal
  x <- str_replace(x, "^Natural Resource Journal$", "Natural Resources Journal")

  # NCHRP Synthesis
  x <- str_replace(x, "^NCHRP.*", "National Cooperative Highway Research Program")

  # Near Surf Geophysics
  x <- str_replace(x, "^Near Surf Geophysics$", "Near Surface Geophysics")

  # Networks and Heterogeneous Media
  x <- str_replace(x, "^Netw Heterog Media$", "Networks and Heterogeneous Media")

  # Netw Spat Economic
  x <- str_replace(x, "^Netw Spat Economic$|^Network and Spatial Economics$", "Networks and Spatial Economics")

  # Noise Health
  x <- str_replace(x, "^Noise Health$", "Noise and Health")

  # Nonlinear Analysis Theory Methods and Applications
  x <- str_replace(x, "^Nonlinear Analysis$", "Nonlinear Analysis Theory Methods and Applications")

  # Numerishe Mathematik
  x <- str_replace(x, "^Numerishe Mathematik$", "Numerische Mathematik")

  # Operation Research
  x <- str_replace(x, "^Operation Research$|^Operations Reseaich$", "Operation Researchs")

  # Not a journal but an org Pacific Coast Archaeological Society Quarterly
  x <- str_replace(x, "^Pacific Coast Archaeological Society Quarterly$", "Pacific Coast Archaeological Society")

  # Pacific Earthquake Engineering Research
  x <- str_replace(x, "^Pacific Earthquake Engineering Research.*|^PEER.*", "Pacific Earthquake Engineering Research")
  x <- ifelse(str_detect(x, "PEER"), "Pacific Earthquake Engineering Research", x)

  # Packaging Technology and Science
  x <- str_replace(x, "^Packaging and Technology Science$", "Packaging Technology and Science")

  # Pervasive and Mobile Computing
  x <- str_replace(x, "^Pervasive Computing$", "Pervasive and Mobile Computing")

  # Petroleum Engineer International
  x <- str_replace(x, "^Pet Engineering$", "Petroleum Engineer International")

  # Philosophical Transactions of the Royal Society A: Mathematical Physical and Engineering Sciences
  x <- str_replace(x, "^Philosophical Transactions of the Royal Society a$|^Phil Transportation R Society a$|^Philosophical Transactions of the Royal Society of London$|^Philosophical Transactions of the Royal Society of London Series [Aa]Mathematical Physical and Engineering Sciences$|^Philosophical Transactions: Mathematical Physical and Engineering Sciences$", "Philosophical Transactions of the Royal Society A\\: Mathematical Physical and Engineering Sciences")

  # Philosophical Transactions of the Royal Society B: Biological Sciences
  x <- str_replace(x, "^Philosophical Transactions of the Royal Society of London Series BBiological Sciences$", "Philosophical Transactions of the Royal Society B: Biological Sciences")

  # Phys D
  x <- str_replace(x, "^Phys D$", "Physica D: Nonlinear Phenomena")

  # Physics of the Earth and Planetary Interiors
  x <- str_replace(x, "^Phys Earth Planet International$", "Physics of the Earth and Planetary Interiors")

  # Physical Review Letters
  x <- str_replace(x, "^Phys Review of Lett$", "Physical Review Letters")

  # Physics and Chemistry of the Earth
  x <- str_replace(x, "^Physics and Chemistry of the Earth Parts.*$", "Physics and Chemistry of the Earth")

  # PLoS Medicine
  x <- str_replace(x, "^PLOS Med$", "PLoS Medicine")

  # PLoS ONE
  x <- str_replace(x, "^P[Ll][Oo][Ss] [Oo][Nn][Ee]$", "PLoS ONE")


  # Prepared for all look like they have authors, so I should just remove the prepared for
  x <- ifelse(str_detect(x, "^Prepared for t?h?e?"), str_remove(x, "^Prepared for t?h?e?\\s?"), x)

  # Preventative Medicine
  x <- str_replace(x, "^Preventative Medicine$", "Preventive Medicine")

  # Probabilistic Engineering Mechanics
  x <- str_replace(x, "^Probability Engineering Mechanics$", "Probabilistic Engineering Mechanics")

  # Proc
  x <- str_replace(x, "^Proc\\b", "Proceedings ")

  # Symp
  x <- str_replace(x, "Symp\\b", "Symposium")

  # Procedia  Social and Behavioral Sciences
  x <- str_replace(x, "^Procedia‐Social and Behavioral Sciences$|^ProcediaSocial and Behavioral Sciences$", "Procedia  Social and Behavioral Sciences")

  # Looking at the proceedings that are journals and matching those before throwing them into conference bucket later

  # Proceedings of the National Academy of Sciences of the United States of America
  x <- str_replace(x, "^PNAS.*|^Proceedings of the National Academy of Sciences$", "Proceedings of the National Academy of Sciences of the United States of America")

  # Proceedings of the IEEE Special Issue on Vehicular Communications
  x <- str_replace(x, "^Proceedings of the IEEE Special Issue on Vehicular Communications$|^Proceedings  IEEE$|^Proceedings  of the IEEE Journal", "Proceedings of the IEEE")

  # Proceedings of the Royal Society A: Mathematical Physical and Engineering Sciences
  # Proceedings  Royal Society
  x <- str_replace(x, "^Proceedings  Royal Society$|^Proceedings  Royal Society of London Series a$|^Proceedings of the Royal Society of London Series a Mathematical and Physical Sciences$|^Royal Society: Math Phys Engineering Science$|^Ser a Math Phys Engineering Science$|^Series a Mathematical and Physical Sciences$", "Proceedings of the Royal Society A\\: Mathematical Physical and Engineering Sciences")

  # Proceedings of the Royal Society B: Biological Sciences
  x <- str_replace(x, "^Proceedings of the Royal Society Biological Sciences$", "Proceedings of the Royal Society B\\: Biological Sciences")

  # Progress in Energy and Combustion Science
  x <- str_replace(x, "^Prog Energy Combust Science$", "Progress in Energy and Combustion Science")

  # Prog Structural Engng Materials
  x <- str_replace(x, "^Prog Energy Combust Science$", "Progress in Energy and Combustion Science")

  # Psychosomatic Medicine
  x <- str_replace(x, "^Psychosom Med$", "Psychosomatic Medicine")

  # Pure and Applied Geophysics
  x <- str_replace(x, "^Pure Applied Geophysics$", "Pure and Applied Geophysics")

  # Pure and Applied Mathematics Quarterly
  x <- str_replace(x, "^Pure Applied Math$", "Pure and Applied Mathematics Quarterly")

  # Q Journal of Engineering Geology
  x <- str_replace(x, "^Q Journal of Engineering Geology$|^Quart Journal of Engineering Geology Hydrogeology$", "Quarterly Journal of Engineering Geology and Hydrogeology")

  # RAND Journal of Economics
  x <- str_replace(x, "^Rand Journal of Economics$|^The RAND Journal of Economics$", "RAND Journal of Economics")

  # Real Estate Economic
  x <- str_replace(x, "^Real Estate Economic$", "Real Estate Economics")

  # Regional Science and Urban Economics
  x <- str_replace(x, "^Reg Science Urban Economic$", "Regional Science and Urban Economics")

  # Reg Studies
  x <- str_replace(x, "^Reg Studies$", "Regional Studies")

  # Renewable and Sustainable Energy Reviews
  x <- str_replace(x, "^Renew Sustain Energy Review$|^Renewable and Sustainable Energy.*", "Renewable and Sustainable Energy Reviews")


  # Road Materials and Pavement Design
  x <- str_replace(x, "^Road Materials and Pavement Design \\(Online$", "Road Materials and Pavement Design")

  # Rock Mechanics and Rock Engineering
  x <- str_replace(x, "^Rock Mechanical Engineering Geology$|^Rock Mechanical Rock Engineering$", "Rock Mechanics and Rock Engineering")

  # Sun: STUVW to Within the Transportation

  # SAE Technical Paper
  x <- str_replace(x, "^SAE.*", "SAE Technical Papers")

  # Science Magazine
  x <- str_replace(x, "^Science Magazine$", "Science")

  # Environmental Science & Technology
  x <- str_replace(x, "^Science Technol$", "Environmental Science & Technology")

  # Science of the Total Environment
  x <- str_replace(x, "^Science Total Environment$", "Science of the Total Environment")

  # Seismological Research Letters
  x <- str_replace(x, "^Seismol Research Lett$|^Seismological Research Let.*", "Seismological Research Letters")

  # Shock and Vibration
  x <- str_replace(x, "^Shock Vib$", "Shock and Vibration")

  # SIAM Journal of Applied Math
  x <- str_replace(x, "^SIAM Journal of Applied Math$", "SIAM Journal on Applied Mathematics")

  # SIAM Journal of Control and Optimization
  x <- str_replace(x, "^SIAM Journal of Control and Optimization$|^SIAM Journal of Control Optim$", "SIAM Journal on Control and Optimization")

  # SIAM Journal of Matrix Anal Applied
  x <- str_replace(x, "^SIAM Journal of Matrix Anal Applied$", "SIAM Journal on Matrix Analysis and Applications")

  # SIAM Journal of Scientific Computing
  x <- str_replace(x, "^SIAM Journal on Scientific Computing$", "SIAM Journal of Scientific Computing")

  # Signal Processing IEEE Transactions on
  x <- str_replace(x, "^Signal Processing IEEE Transactions on$|^Signal Processing Magazine IEEE$", "Signal Processing")

  # Smart Materials and Structures
  x <- str_replace(x, "^Smart Materials Structural$|^Smart Structures and Materials$", "Smart Materials and Structures")

  # Society Science
  x <- str_replace(x, "^Society Science$|^Society Science Med$", "Social Science and Medicine")

  # Social Studies of Science
  x <- str_replace(x, "^Society Studies Science$", "Social Studies of Science")

  # Soils and Foundations
  x <- str_replace(x, "^Soil and Foundations$", "Soils and Foundations")

  # Soil Dynamics and Earthquake Engineering
  x <- str_replace(x, "^Soil Dyn Earthquake Engineering$", "Soil Dynamics and Earthquake Engineering")

  # Communications in Soil Science and Plant Analysis
  x <- str_replace(x, "^Soil Science Plant Anal$", "Communications in Soil Science and Plant Analysis")

  # Soil Science Society of America Journal
  x <- str_replace(x, "^Soil Science Society American Journal.*$|^Soil Science Society of American Journal of$", "Soil Science Society of America Journal")

  # Soil and Tillage Research
  x <- str_replace(x, "^Soil Till Research$", "Soil and Tillage Research")

  # Soils and Foundations
  x <- str_replace(x, "^Soils and Foundations.*$|^Soils Found$", "Soils and Foundations")

  # Spectra
  x <- str_replace(x, "^Spectra$", "Earthquake Spectra")

  # Standard and Poors PPP Credit Survey
  x <- str_replace(x, "^Standard and Poors PPP Credit Survey$", "SandP")

  # Stanford Law Review
  x <- str_replace(x, "^Stanford Law Policy Review$", "Stanford Law Review")

  # Statistics and Computing
  x <- str_replace(x, "^Stat Computing$", "Statistics and Computing")

  # Statistical Methods in Medical Research
  x <- str_replace(x, "^Stat Methods Med Research$|^Statistical Methods in Medical$", "Statistical Methods in Medical Research")

  # Structural Engineering/Earthquake Engineering
  x <- str_replace(x, "^Structural and Earthquake Engineering Proc JSCE$", "Structural Engineering\\/Earthquake Engineering")

  # Structural Concrete
  x <- str_replace(x, "^Structural Concr Journal of FIB$", "Structural Concrete")

  # Structural Control and Health Monitoring
  x <- str_replace(x, "^Structural Control and Health Monitoring Inpress$|^Structural Control Heal Monit$|^Structural Heal Monit$", "Structural Control and Health Monitoring")

  # Structural Design of Tall and Special Buildings
  x <- str_replace(x, "^Structural Design of Tall Buildings$", "Structural Design of Tall and Special Buildings")

  # Structural Dynamics @ 2000: Current Status and Future Directions Research Studies Press Ltd
  x <- str_replace(x, "^Structural Dynamics @ 2000: Current Status and Future Directions Research Studies Press Ltd$", "Structural Dynamics")

  # Structural Engineering
  x <- str_replace(x, "^Structural Engineering$|^Structural Engineering International$", "Structural Engineering International: Journal of the International Association for Bridge and Structural Engineering (IABSE)")

  # Structure and Infrastructure Engineering
  x <- str_replace(x, "^Structural Infrastruct Engineering$", "Structure and Infrastructure Engineering")

  # Studies Avian Biology -- I don't know why this is not being detected, I think it ended in 2008
  x <- str_replace(x, "^Studies Avian Biology$", "Studies in Avian Biology")


  # Studies in Symbolic Interaction
  x <- str_replace(x, "^Studies Symb Interact$", "Studies in Symbolic Interaction")

  # Supply Chain Management
  x <- str_replace(x, "^Supply Chain Management\\: An International Journal$", "Supply Chain Management")

  # Technometrics
  x <- str_replace(x, "^Technometric$", "Technometrics")

  # American Economic Review
  x <- str_replace(x, "^The American Economic Review$", "American Economic Review")

  # American Journal of Clinical Nutrition
  x <- str_replace(x, "^The American Journal of Clinical Nutrition$", "American Journal of Clinical Nutrition")

  # American Naturalist
  x <- str_replace(x, "^The American Naturalist$", "American Naturalist")

  # the
  x <- ifelse(str_detect(x, "^the"), str_remove(x, "^the "), x)

  # Annals of Porb
  x <- str_replace(x, "^The Annals of Probability$", "Annals of Probability")

  # Annals of Regional Science
  x <- str_replace(x, "^The Annals of Regional Science$", "Annals of Regional Science")

  # Annals of Regional Statistics
  x <- str_replace(x, "^The Annals of Statistics$", "Annals of Statistics")

  # Auk
  x <- str_replace(x, "^The Auk$", "Auk")

  # International Journal of Life Cycle Assessment
  x <- str_replace(x, "^The International Journal of Life Cycle Assessment$", "International Journal of Life Cycle Assessment")

  # International Journal of Logistics Management
  x <- str_replace(x, "^The International Journal of Logistics Management$", "International Journal of Logistics Management")

  # Robotics Research
  x <- str_replace(x, "^The International Journal of Robotics Research$", "International Journal of Robotics Research")

  # Journal of Economic Inequality
  x <- str_replace(x, "^The Journal of Economic Inequality$", "Journal of Economic Inequality")

  # The Journal of Economic Perspectives
  x <- str_replace(x, "^The Journal of Economic Perspectives$", "Journal of Economic Perspectives")

  # The Journal of Industrial Economics
  x <- str_replace(x, "^The Journal of Industrial Economics$", "Journal of Industrial Economics")

  # The Journal of Machine Learning Research
  x <- str_replace(x, "^The Journal of Machine Learning Research$", "Journal of Machine Learning Research")

  # The Journal of Supercomputing
  x <- str_replace(x, "^The Journal of Supercomputing$", "Journal of Supercomputing")

  # The Quarterly Journal of Economics
  x <- str_replace(x, "^The Quarterly Journal of Economics$", "Quarterly Journal of Economics")

  # Quarterly Review of Economics and Finance
  x <- str_replace(x, "^The Quarterly Review of Economics and Finance$", "Quarterly Review of Economics and Finance")

  # The Senses and Society
  x <- str_replace(x, "^The Senses and Society$", "Senses and Society")

  # The Social Science Journal
  x <- str_replace(x, "^ The Social Science Journal$", "Social Science Journal")

  # Traffic Engineering and Control
  x <- str_replace(x, "^Traffic Engineering Control$|^Traffic Engineeiing and Control$", "Traffic Engineering and Control")


  # SKIPPED MORE

  # Transportation Research Part A: Policy and Practice
  x <- str_replace(x, "^Transport Research APol$|^Transport Research\\:? Part a$|^Transportation Research a$|^Transportation Research A: Policy and Practice$|^Transportation Research Part [Aa].*|^Transportation Research\\: Part a", "Transportation Research Part A\\: Policy and Practice")

  # Transportation Research Part B: Methodological
  pattern <- c("^Transp01tation Research Part B$", "^Transpmtation Research Pait B: Methodological$", "^Transpn Research$", "^Transporation Research B$", "Transportation Re Search Part B\\: Methodological$", "^Transportation Reseaich Part B.*$", "^Transportation Research B$", "^Transportation Research Pait B.*|^Transportation Research\\:? Part\\s?B.*")
  pattern <- paste(pattern, collapse = "|")
  x <- str_replace(x, pattern, "Transportation Research Part B\\: Methodological")

  # Transportation Research Part C: Emerging Technologies
  pattern <- c("^Transpo1tation Research Part C\\: Emerging Technologies$", "^Transport Research Part C: Emerging Technol$", "Transportation Reseaich Pait C\\: Emerging Technologies$", "^Transportation Research C$|^Transportation Research C Emerging Technol$|^Transportation Research C: Emerging Technologies$|^Transportation Research\\:? Part C.*|^Transportation ResearchPart C")
  pattern <- paste(pattern, collapse = "|")
  x <- str_replace(x, pattern, "Transportation Research Part C\\: Emerging Technologies")

  # Transportation Research Part D: Transport and Environment -- I am including Transportation Research Part in here, even though not all are D, most seem to be
  x <- str_replace(x, "^Transportation Research D.*$|^Transportation Research Part$|^Transportation Research\\:? Part D.*", "Transportation Research Part D\\: Transport and Environment")

  # Transportation Research Part E: Logistics and Transportation Review
  x <- str_replace(x, "^Transport Research ELog$|^Transportation Research  Part E$|^Transportation Resear\\:? Part E.*|^Transportation Research Part E$|^Transportation Research Part ELogistics and Transportation Review$", "Transportation Research Part E\\: Logistics and Transportation Review")

  # Transportation Research Part F: Traffic Psychology and Behaviour
  x <- str_replace(x, "^Transportat Research F\\: Traff Psychology Behavior$|^Transportation Research Part F.*|^Transportation Research\\: Part F$", "Transportation Research Part F\\: Traffic Psychology and Behaviour")

  # Transportation Research Record
  x <- str_replace(x, "^Transp01tation Research Record: Journal of the Transportation Research Board$|^Transpn Research Rec$|^Transport Research Rec$|^Transport Research Rec: Journal of Transportation Res Board$|^Transportation Reasearch Record: Journal of the Transportation Research Board$|^Transportation Research Rec$|^Transportation Research Rec\\:? Journal of Transp Res Board$|^TRANSPORTATION RESEARCH RECORD|^Transportation Research Repord.*|^Transptn Research Rec$|^Transrortation Research Record64and$|^TRR\\b|^Transportation Research Record.*", "Transportation Research Record")

  # Transport Pol
  x <- str_replace(x, "^Transport Pol$|^Transport Policy17\\(2\\):7284$|^Transportation Policy$|^Transportation Transport Policy$", "Transport Policy")

  # Transport Science
  x <- str_replace(x, "^Transport Science$", "Transportation Science")

  # Transport Reviews
  x <- str_replace(x, "^Transport Reviews\\: A Transnational Transdisciplinary Journal$", "Transport Reviews")

  # Transportation (Amst
  x <- str_replace(x, "^Transportation \\(Amst$", "Transportation")

  # Transportmetrica A: Transport Science
  x <- str_replace(x, "^Transportation A\\: Transp Science$", "Transportmetrica A\\: Transport Science")

  #ASAE is the ASABE in Scimago!!!
  # Transactions of the ASABE
  x <- str_replace(x, "^Transportation ASAE$|^^Transportation of the ASAE$", "Transactions of the ASABE")

  # Transportation Letters: The International Journal of Transportation Research
  x <- str_replace(x, "^Transportation Letters\\: The International Journal of Transportation Research$", "Transportation Letters")

  # Transportation LJournal
  x <- str_replace(x, "^Transportation LJournal$", "Transportation Journal")

  # Transport in Porous Media
  x <- str_replace(x, "^Transportation Porous Media$", "Transport in Porous Media")

  # On its own "Transportation Research" cannot be connected to one journal. The titles link to multiple of the A-F above and Research Record

  # It looks like this Board runs into the research program, so I can assign this so it can be detected as an org later
  x <- str_replace(x, "^Transportation Research Board.*", "National Cooperative Highway Research Program")

  # Tunnelling and Underground Space Technology
  x <- str_replace(x, "^Tunnel Undergr Space Tech$|^Tunneling and Underground Space Technology$", "Tunnelling and Underground Space Technology")

  # URBAN STUDIES
  x <- str_replace(x, "^URBAN STUDIES", "Urban Studies")

  # World Applied Sciences Journal
  x <- str_replace(x, "^World Applied Science Journal$", "World Applied Sciences Journal")

  # World Dredging Mining and Constructions
  x <- str_replace(x, "^World Dredging Mining and Construction$", "World Dredging Mining and Constructions")

  # ZAMM Zeitschrift Für Angewandte Mathematik Und Mechanik
  x <- str_replace(x, "^ZAMM‐Journal of Applied Mathematics and Mechanics/Zeitschrift Fuf Grund Der Plastizitätsbedingung Fü$", "ZAMM Zeitschrift Für Angewandte Mathematik Und Mechanik")

  if(remove_periods = T){
    #remove all periods
    x <- str_remove_all(x, '\\.')
  }

  return(x)
}
