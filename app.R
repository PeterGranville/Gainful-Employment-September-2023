
#### Load packages #### 
library(DT)
library(shiny)
library(dplyr)
library(scales)
library(plotly)
library(stringr)
library(ggplot2)
library(reshape2)
library(data.table)
#### End #### 

ui <- fluidPage(

    titlePanel("Outcomes of the Gainful Employment Rule"),

    #### Preamble ####
    paste("Preamble text goes here."),
    #### End #### 
    
    sidebarLayout(
        sidebarPanel(
          
          #### U1: Dropdown menu for stateSelection ####
          selectInput(inputId = "stateSelection", 
                      label = "Filter for a U.S. state here:", 
                      choices = c(
                        "---",
                        "Alabama",
                        "Alaska",
                        "Arizona",
                        "Arkansas",
                        "California",
                        "Colorado",
                        "Connecticut",
                        "Delaware",
                        "District of Columbia",
                        "Florida",
                        "Georgia",
                        "Hawaii",
                        "Idaho",
                        "Illinois",
                        "Indiana",
                        "Iowa",
                        "Kansas",
                        "Kentucky",
                        "Louisiana",
                        "Maine",
                        "Maryland",
                        "Massachusetts",
                        "Michigan",
                        "Minnesota",
                        "Mississippi",
                        "Missouri",
                        "Montana",
                        "Nebraska",
                        "Nevada",
                        "New Hampshire",
                        "New Jersey",
                        "New Mexico",
                        "New York",
                        "North Carolina",
                        "North Dakota",
                        "Ohio",
                        "Oklahoma",
                        "Oregon",
                        "Pennsylvania",
                        "Rhode Island",
                        "South Carolina",
                        "South Dakota",
                        "Tennessee",
                        "Texas",
                        "Utah",
                        "Vermont",
                        "Virginia",
                        "Washington",
                        "West Virginia",
                        "Wisconsin",
                        "Wyoming"), 
                      multiple=FALSE), 
          #### End #### 
          
          #### U2: Dropdown menu for controlSelection ####
          selectInput(inputId = "controlSelection", 
                      label = "Filter for an institutional control here:",
                      choices=c(
                        "---",
                        "Public", 
                        "Private, nonprofit", 
                        "Private, for-profit"
                      ), 
                      multiple=FALSE),
          #### End #### 
          
          #### U3: Dropdown menu for collegeSelection ####
          selectInput(inputId = "collegesSelection",
                      label="Filter for an MSI category here:",
                      choices=c("---", 
                                "Historically Black Colleges and Universities (HBCUs)", 
                                "Predominantly Black Institutions (PBIs)", 
                                "Hispanic Serving Institutions (HSIs)", 
                                "Asian American Native American Pacific Islander-serving Institution (AANAPIIs)", 
                                "Tribal Colleges and Universities (TCUs)", 
                                "Alaska Native Native Hawaiian-serving Institutions (ANNHIs)",
                                "Native American Non-Tribal Institutions (NANTIs)",
                                "All Minority Serving Institutions (MSIs)", 
                                "All non-MSIs"),
                      multiple=FALSE),
          #### End ####
          
          #### U4: Dropdown menu for fieldSelection ####
          selectInput(inputId = "fieldSelection", 
                      label = "Filter for a field of study here:", 
                      choices = c(
                        "---",
                        "(2-digit CIP 01) Agriculture, Agriculture Operations, And Related Sciences",
                        "(2-digit CIP 03) Natural Resources And Conservation",
                        "(2-digit CIP 04) Architecture And Related Services",
                        "(2-digit CIP 09) Communication, Journalism, And Related Programs",
                        "(2-digit CIP 10) Communications Technologies/Technicians And Support Services",
                        "(2-digit CIP 11) Computer And Information Sciences And Support Services",
                        "(2-digit CIP 12) Personal And Culinary Services",
                        "(2-digit CIP 13) Education",
                        "(2-digit CIP 14) Engineering",
                        "(2-digit CIP 15) Engineering Technologies And Engineering-Related Fields",
                        "(2-digit CIP 16) Foreign Languages, Literatures, And Linguistics",
                        "(2-digit CIP 19) Family And Consumer Sciences/Human Sciences",
                        "(2-digit CIP 22) Legal Professions And Studies",
                        "(2-digit CIP 23) English Language And Literature/Letters",
                        "(2-digit CIP 24) Liberal Arts And Sciences, General Studies And Humanities",
                        "(2-digit CIP 25) Library Science",
                        "(2-digit CIP 26) Biological And Biomedical Sciences",
                        "(2-digit CIP 30) Multi/Interdisciplinary Studies",
                        "(2-digit CIP 31) Parks, Recreation, Leisure, And Fitness Studies",
                        "(2-digit CIP 32) Basic Skills And Developmental/Remedial Education",
                        "(2-digit CIP 34) Health-Related Knowledge And Skills",
                        "(2-digit CIP 39) Theology And Religious Vocations",
                        "(2-digit CIP 40) Physical Sciences",
                        "(2-digit CIP 41) Science Technologies/Technicians",
                        "(2-digit CIP 42) Psychology",
                        "(2-digit CIP 43) Homeland Security, Law Enforcement, Firefighting And Related Protective Services",
                        "(2-digit CIP 44) Public Administration And Social Service Professions",
                        "(2-digit CIP 45) Social Sciences",
                        "(2-digit CIP 46) Construction Trades",
                        "(2-digit CIP 47) Mechanic And Repair Technologies/Technicians",
                        "(2-digit CIP 48) Precision Production",
                        "(2-digit CIP 49) Transportation And Materials Moving",
                        "(2-digit CIP 50) Visual And Performing Arts",
                        "(2-digit CIP 51) Health Professions And Related Programs",
                        "(2-digit CIP 52) Business, Management, Marketing, And Related Support Services",
                        "(2-digit CIP 54) History",
                        "(2-digit CIP 60) Residency Programs",
                        "(4-digit CIP 0105) Agricultural And Domestic Animal Services",
                        "(4-digit CIP 0106) Applied Horticulture And Horticultural Business Services",
                        "(4-digit CIP 0110) Food Science And Technology",
                        "(4-digit CIP 0301) Natural Resources Conservation And Research",
                        "(4-digit CIP 0402) Architecture",
                        "(4-digit CIP 0901) Communication And Media Studies",
                        "(4-digit CIP 0904) Journalism",
                        "(4-digit CIP 0907) Radio, Television, And Digital Communication",
                        "(4-digit CIP 0909) Public Relations, Advertising, And Applied Communication",
                        "(4-digit CIP 1002) Audiovisual Communications Technologies/Technicians",
                        "(4-digit CIP 1003) Graphic Communications",
                        "(4-digit CIP 1099) Communications Technologies/Technicians And Support Services, Other",
                        "(4-digit CIP 1101) Computer And Information Sciences, General",
                        "(4-digit CIP 1102) Computer Programming",
                        "(4-digit CIP 1103) Data Processing",
                        "(4-digit CIP 1104) Information Science/Studies",
                        "(4-digit CIP 1105) Computer Systems Analysis",
                        "(4-digit CIP 1107) Computer Science",
                        "(4-digit CIP 1108) Computer Software And Media Applications",
                        "(4-digit CIP 1109) Computer Systems Networking And Telecommunications",
                        "(4-digit CIP 1110) Computer/Information Technology Administration And Management",
                        "(4-digit CIP 1199) Computer And Information Sciences And Support Services, Other",
                        "(4-digit CIP 1203) Funeral Service And Mortuary Science",
                        "(4-digit CIP 1204) Cosmetology And Related Personal Grooming Services",
                        "(4-digit CIP 1205) Culinary Arts And Related Services",
                        "(4-digit CIP 1299) Personal And Culinary Services, Other",
                        "(4-digit CIP 1301) Education, General",
                        "(4-digit CIP 1302) Bilingual, Multilingual, And Multicultural Education",
                        "(4-digit CIP 1303) Curriculum And Instruction",
                        "(4-digit CIP 1304) Educational Administration And Supervision",
                        "(4-digit CIP 1305) Educational/Instructional Media Design",
                        "(4-digit CIP 1306) Educational Assessment, Evaluation, And Research",
                        "(4-digit CIP 1310) Special Education And Teaching",
                        "(4-digit CIP 1311) Student Counseling And Personnel Services",
                        "(4-digit CIP 1312) Teacher Education And Professional Development, Specific Levels And Methods",
                        "(4-digit CIP 1313) Teacher Education And Professional Development, Specific Subject Areas",
                        "(4-digit CIP 1314) Teaching English Or French As A Second Or Foreign Language",
                        "(4-digit CIP 1315) Teaching Assistants/Aides",
                        "(4-digit CIP 1399) Education, Other",
                        "(4-digit CIP 1409) Computer Engineering",
                        "(4-digit CIP 1410) Electrical, Electronics And Communications Engineering",
                        "(4-digit CIP 1439) Geological/Geophysical Engineering",
                        "(4-digit CIP 1499) Engineering, Other",
                        "(4-digit CIP 1500) Engineering Technology, General",
                        "(4-digit CIP 1503) Electrical Engineering Technologies/Technicians",
                        "(4-digit CIP 1504) Electromechanical Instrumentation And Maintenance Technologies/Technicians",
                        "(4-digit CIP 1505) Environmental Control Technologies/Technicians",
                        "(4-digit CIP 1506) Industrial Production Technologies/Technicians",
                        "(4-digit CIP 1507) Quality Control And Safety Technologies/Technicians",
                        "(4-digit CIP 1508) Mechanical Engineering Related Technologies/Technicians",
                        "(4-digit CIP 1512) Computer Engineering Technologies/Technicians",
                        "(4-digit CIP 1513) Drafting/Design Engineering Technologies/Technicians",
                        "(4-digit CIP 1601) Linguistic, Comparative, And Related Language Studies And Services",
                        "(4-digit CIP 1907) Human Development, Family Studies, And Related Services",
                        "(4-digit CIP 1909) Apparel And Textiles",
                        "(4-digit CIP 2200) Non-Professional General Legal Studies (Undergraduate)",
                        "(4-digit CIP 2201) Law",
                        "(4-digit CIP 2202) Legal Research And Advanced Professional Studies",
                        "(4-digit CIP 2203) Legal Support Services",
                        "(4-digit CIP 2299) Legal Professions And Studies, Other",
                        "(4-digit CIP 2301) English Language And Literature, General",
                        "(4-digit CIP 2313) Rhetoric And Composition/Writing Studies",
                        "(4-digit CIP 2314) Literature",
                        "(4-digit CIP 2399) English Language And Literature/Letters, Other",
                        "(4-digit CIP 2401) Liberal Arts And Sciences, General Studies And Humanities",
                        "(4-digit CIP 2501) Library Science And Administration",
                        "(4-digit CIP 2601) Biology, General",
                        "(4-digit CIP 2613) Ecology, Evolution, Systematics, And Population Biology",
                        "(4-digit CIP 3000) Multi-/Interdisciplinary Studies, General",
                        "(4-digit CIP 3005) Peace Studies And Conflict Resolution",
                        "(4-digit CIP 3011) Gerontology",
                        "(4-digit CIP 3017) Behavioral Sciences",
                        "(4-digit CIP 3020) International/Global Studies",
                        "(4-digit CIP 3031) Human Computer Interaction",
                        "(4-digit CIP 3099) Multi/Interdisciplinary Studies, Other",
                        "(4-digit CIP 3103) Parks, Recreation And Leisure Facilities Management",
                        "(4-digit CIP 3105) Health And Physical Education/Fitness",
                        "(4-digit CIP 3201) Basic Skills And Developmental/Remedial Education",
                        "(4-digit CIP 3401) Health-Related Knowledge And Skills",
                        "(4-digit CIP 3903) Missions/Missionary Studies And Missiology",
                        "(4-digit CIP 3904) Religious Education",
                        "(4-digit CIP 3906) Theological And Ministerial Studies",
                        "(4-digit CIP 4008) Physics",
                        "(4-digit CIP 4101) Biology Technician/Biotechnology Laboratory Technician",
                        "(4-digit CIP 4201) Psychology, General",
                        "(4-digit CIP 4227) Research And Experimental Psychology",
                        "(4-digit CIP 4228) Clinical, Counseling And Applied Psychology",
                        "(4-digit CIP 4299) Psychology, Other",
                        "(4-digit CIP 4301) Criminal Justice And Corrections",
                        "(4-digit CIP 4302) Fire Protection",
                        "(4-digit CIP 4303) Homeland Security",
                        "(4-digit CIP 4400) Human Services, General",
                        "(4-digit CIP 4402) Community Organization And Advocacy",
                        "(4-digit CIP 4404) Public Administration",
                        "(4-digit CIP 4405) Public Policy Analysis",
                        "(4-digit CIP 4407) Social Work",
                        "(4-digit CIP 4499) Public Administration And Social Service Professions, Other",
                        "(4-digit CIP 4501) Social Sciences, General",
                        "(4-digit CIP 4504) Criminology",
                        "(4-digit CIP 4506) Economics",
                        "(4-digit CIP 4507) Geography And Cartography",
                        "(4-digit CIP 4509) International Relations And National Security Studies",
                        "(4-digit CIP 4510) Political Science And Government",
                        "(4-digit CIP 4511) Sociology",
                        "(4-digit CIP 4599) Social Sciences, Other",
                        "(4-digit CIP 4600) Construction Trades, General",
                        "(4-digit CIP 4602) Carpenters",
                        "(4-digit CIP 4603) Electrical And Power Transmission Installers",
                        "(4-digit CIP 4604) Building/Construction Finishing, Management, And Inspection",
                        "(4-digit CIP 4605) Plumbing And Related Water Supply Services",
                        "(4-digit CIP 4699) Construction Trades, Other",
                        "(4-digit CIP 4700) Mechanics And Repairers, General",
                        "(4-digit CIP 4701) Electrical/Electronics Maintenance And Repair Technology",
                        "(4-digit CIP 4702) Heating, Air Conditioning, Ventilation And Refrigeration Maintenance Technology/Technician",
                        "(4-digit CIP 4703) Heavy/Industrial Equipment Maintenance Technologies",
                        "(4-digit CIP 4704) Precision Systems Maintenance And Repair Technologies",
                        "(4-digit CIP 4706) Vehicle Maintenance And Repair Technologies",
                        "(4-digit CIP 4799) Mechanic And Repair Technologies/Technicians, Other",
                        "(4-digit CIP 4805) Precision Metal Working",
                        "(4-digit CIP 4901) Air Transportation",
                        "(4-digit CIP 4902) Ground Transportation",
                        "(4-digit CIP 4903) Marine Transportation",
                        "(4-digit CIP 4999) Transportation And Materials Moving, Other",
                        "(4-digit CIP 5001) Visual And Performing Arts, General",
                        "(4-digit CIP 5004) Design And Applied Arts",
                        "(4-digit CIP 5005) Drama/Theatre Arts And Stagecraft",
                        "(4-digit CIP 5006) Film/Video And Photographic Arts",
                        "(4-digit CIP 5007) Fine And Studio Arts",
                        "(4-digit CIP 5009) Music",
                        "(4-digit CIP 5010) Arts, Entertainment,And Media Management",
                        "(4-digit CIP 5100) Health Services/Allied Health/Health Sciences, General",
                        "(4-digit CIP 5105) Advanced/Graduate Dentistry And Oral Sciences",
                        "(4-digit CIP 5106) Dental Support Services And Allied Professions",
                        "(4-digit CIP 5107) Health And Medical Administrative Services",
                        "(4-digit CIP 5108) Allied Health And Medical Assisting Services",
                        "(4-digit CIP 5109) Allied Health Diagnostic, Intervention, And Treatment Professions",
                        "(4-digit CIP 5110) Clinical/Medical Laboratory Science/Research And Allied Professions",
                        "(4-digit CIP 5111) Health/Medical Preparatory Programs",
                        "(4-digit CIP 5112) Medicine",
                        "(4-digit CIP 5115) Mental And Social Health Services And Allied Professions",
                        "(4-digit CIP 5116) Nursing",
                        "(4-digit CIP 5118) Ophthalmic And Optometric Support Services And Allied Professions",
                        "(4-digit CIP 5119) Osteopathic Medicine/Osteopathy",
                        "(4-digit CIP 5120) Pharmacy, Pharmaceutical Sciences, And Administration",
                        "(4-digit CIP 5121) Podiatric Medicine/Podiatry",
                        "(4-digit CIP 5122) Public Health",
                        "(4-digit CIP 5123) Rehabilitation And Therapeutic Professions",
                        "(4-digit CIP 5124) Veterinary Medicine",
                        "(4-digit CIP 5126) Health Aides/Attendants/Orderlies",
                        "(4-digit CIP 5127) Medical Illustration And Informatics",
                        "(4-digit CIP 5131) Dietetics And Clinical Nutrition Services",
                        "(4-digit CIP 5133) Alternative And Complementary Medicine And Medical Systems",
                        "(4-digit CIP 5135) Somatic Bodywork And Related Therapeutic Services",
                        "(4-digit CIP 5136) Movement And Mind-Body Therapies And Education",
                        "(4-digit CIP 5138) Registered Nursing, Nursing Administration, Nursing Research And Clinical Nursing",
                        "(4-digit CIP 5139) Practical Nursing, Vocational Nursing And Nursing Assistants",
                        "(4-digit CIP 5199) Health Professions And Related Clinical Sciences, Other",
                        "(4-digit CIP 5201) Business/Commerce, General",
                        "(4-digit CIP 5202) Business Administration, Management And Operations",
                        "(4-digit CIP 5203) Accounting And Related Services",
                        "(4-digit CIP 5204) Business Operations Support And Assistant Services",
                        "(4-digit CIP 5206) Business/Managerial Economics",
                        "(4-digit CIP 5207) Entrepreneurial And Small Business Operations",
                        "(4-digit CIP 5208) Finance And Financial Management Services",
                        "(4-digit CIP 5209) Hospitality Administration/Management",
                        "(4-digit CIP 5210) Human Resources Management And Services",
                        "(4-digit CIP 5211) International Business",
                        "(4-digit CIP 5212) Management Information Systems And Services",
                        "(4-digit CIP 5213) Management Sciences And Quantitative Methods",
                        "(4-digit CIP 5214) Marketing",
                        "(4-digit CIP 5215) Real Estate",
                        "(4-digit CIP 5218) General Sales, Merchandising And Related Marketing Operations",
                        "(4-digit CIP 5219) Specialized Sales, Merchandising And  Marketing Operations",
                        "(4-digit CIP 5220) Construction Management",
                        "(4-digit CIP 5299) Business, Management, Marketing, And Related Support Services, Other",
                        "(4-digit CIP 5401) History",
                        "(4-digit CIP 6004) Medical Residency Programs - General Certificates"), 
                      multiple=FALSE),
          #### End ####
          
          #### U5: Dropdown menu for levelSelection ####
          selectInput(inputId = "levelSelection", 
                      label = "Filter for a credential level here:",
                      choices=c(
                        "---",
                        "Undergraduate certificate", 
                        "Associate's degree", 
                        "Bachelor's degree",
                        "Post baccalaureate certificate", 
                        "Master's degree", 
                        "Doctoral degree", 
                        "First professional degree", 
                        "Graduate certificate"
                      ), 
                      multiple=FALSE), 
          #### End ####
          
          #### U6: Dropdown menu for plotSelection ####
          selectInput(inputId = "plotSelection", 
                      label = "Select a plot here:",
                      choices=c(
                        "Map of colleges by GE status",
                        "Debt-to-earnings chart",
                        "Earnings premium chart"
                      ), 
                      multiple=FALSE), 
          #### End ####
          
          #### U7: Dropdown menu for tableSelection ####
          selectInput(inputId = "tableSelection", 
                      label = "Select a table here:",
                      choices=c(
                        "Pass/fail outcomes",
                        "Simulating changes to debt and earnings", 
                        "Table of programs", 
                        "Table of colleges"
                      ), 
                      multiple=FALSE), 
          #### End ####
          
          #### U8: Submit button ####
          submitButton(text = "Enter my selections", icon = NULL, width = NULL)
          #### End #### 
        ) 
      ),

        mainPanel(
          
          #### O1: Print initialCount ####
          textOutput("initialCount"),
          #### End ####
          
          tabsetPanel(
            tabPanel("Plot View", fluid=TRUE,
          
          #### O2: Print plotTitle ####
          textOutput("plotTitle"),
          #### End ####
          
          #### O3: Print plotPlot ####
          plotlyOutput("plotPlot"),
          #### End ####
          
          #### O4: Print plotNotes ####
          textOutput("plotNotes")
          #### End #### 
          
            ), tabPanel("Table View", fluid=TRUE,
                        
          #### O5: Print tableTitle ####
          textOutput("tableTitle"),
          #### End ####
          
          #### O6: Print tableTable ####
          dataTableOutput("tableTable"),
          #### End ####
          
          #### O7: Print tableNotes ####
          textOutput("tableNotes")
          #### End #### 
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  observe({
    
    ######################## Formatting Data ########################
    
    #### S1: Create statesDF, the state crosswalk #### 
    
    statesDF <- data.frame("StateName"=c(
      "Alabama",
      "Alaska",
      "Arizona",
      "Arkansas",
      "California",
      "Colorado",
      "Connecticut",
      "Delaware",
      "District of Columbia",
      "Florida",
      "Georgia",
      "Hawaii",
      "Idaho",
      "Illinois",
      "Indiana",
      "Iowa",
      "Kansas",
      "Kentucky",
      "Louisiana",
      "Maine",
      "Maryland",
      "Massachusetts",
      "Michigan",
      "Minnesota",
      "Mississippi",
      "Missouri",
      "Montana",
      "Nebraska",
      "Nevada",
      "New Hampshire",
      "New Jersey",
      "New Mexico",
      "New York",
      "North Carolina",
      "North Dakota",
      "Ohio",
      "Oklahoma",
      "Oregon",
      "Pennsylvania",
      "Rhode Island",
      "South Carolina",
      "South Dakota",
      "Tennessee",
      "Texas",
      "Utah",
      "Vermont",
      "Virginia",
      "Washington",
      "West Virginia",
      "Wisconsin",
      "Wyoming"
    ), "state"=c(
      "AL", #	Alabama
      "AK", #	Alaska
      "AZ", #	Arizona
      "AR", #	Arkansas
      "CA", #	California
      "CO", #	Colorado
      "CT", #	Connecticut
      "DE", #	Delaware
      "DC", #	District of Columbia
      "FL", #	Florida
      "GA", #	Georgia
      "HI", #	Hawaii
      "ID", #	Idaho
      "IL", #	Illinois
      "IN", #	Indiana
      "IA", #	Iowa
      "KS", #	Kansas
      "KY", #	Kentucky
      "LA", #	Louisiana
      "ME", #	Maine
      "MD", #	Maryland
      "MA", #	Massachusetts
      "MI", #	Michigan
      "MN", #	Minnesota
      "MS", #	Mississippi
      "MO", #	Missouri
      "MT", #	Montana
      "NE", #	Nebraska
      "NV", #	Nevada
      "NH", #	New Hampshire
      "NJ", #	New Jersey
      "NM", #	New Mexico
      "NY", #	New York
      "NC", #	North Carolina
      "ND", #	North Dakota
      "OH", #	Ohio
      "OK", #	Oklahoma
      "OR", #	Oregon
      "PA", #	Pennsylvania
      "RI", #	Rhode Island
      "SC", #	South Carolina
      "SD", #	South Dakota
      "TN", #	Tennessee
      "TX", #	Texas
      "UT", #	Utah
      "VT", #	Vermont
      "VA", #	Virginia
      "WA", #	Washington
      "WV", #	West Virginia
      "WI", #	Wisconsin
      "WY"  # Wyoming
    ))
    statesDF$`st_fips` <- statesDF$`StateName`
    
    #### End #### 
    
    #### S2: Create levelsDF, the level crosswalk ####
    levelsDF <- data.frame("LevelName"=c("Undergraduate certificate", 
                                         "Associate's degree", 
                                         "Bachelor's degree",
                                         "Post baccalaureate certificate", 
                                         "Master's degree", 
                                         "Doctoral degree", 
                                         "First professional degree", 
                                         "Graduate certificate"),
                           "cred_lvl" = c("UG Certificates", 
                                          "Associate's", 
                                          "Bachelor's", 
                                          "Post-BA Certs",
                                          "Master's",
                                          "Doctoral", 
                                          "Professional", 
                                          "Grad Certs"))
    #### End #### 
    
    #### S3: Load in cips, the field crosswalk ####
    cips <- read.csv("Data/ge-cips.csv", header=TRUE)
    #### End #### 
    
    #### S4: Create controlsDF, the control crosswalk #### 
    controlsDF <- data.frame("ControlName"=c("Public", 
                                             "Private, nonprofit", 
                                             "Private, for-profit"), 
                             "control_peps"=c("Public", 
                                              "Private, Nonprofit", 
                                              "Proprietary"))
    #### End ####
    
    #### S5A: Load in gei, the data on institutions ####
    gei <- fread("Data/ge-colleges.csv", header=TRUE)
    #### End ####
    
    #### S5B: Merge gei with statesDF and controlsDF ####
    gei <- left_join(x=gei, y=statesDF, by="st_fips")
    gei <- left_join(x=gei, y=controlsDF, by="control_peps")
    #### End #### 
    
    #### S6: Filter gei by stateSelection, controlSelection, and collegesSelection ####
    if(input$stateSelection != "---"){
      gei <- gei %>% filter(`st_fips`==input$stateSelection)
    }
    
    if(input$controlSelection != "---"){
      gei <- gei %>% filter(`ControlName`==input$controlSelection)
    }
    
    if(input$collegesSelection=="Historically Black Colleges and Universities (HBCUs)"){gei <- gei %>% filter(`hbcu`==1)}
    if(input$collegesSelection=="Predominantly Black Institutions (PBIs)"){gei <- gei %>% filter(`pbi`==1)}
    if(input$collegesSelection=="Hispanic Serving Institutions (HSIs)"){gei <- gei %>% filter(`hsi`==1)}
    if(input$collegesSelection=="Asian American Native American Pacific Islander-serving Institution (AANAPIIs)"){gei <- gei %>% filter(`aanapii`==1)}
    if(input$collegesSelection=="Tribal Colleges and Universities (TCUs)"){gei <- gei %>% filter(tribal==1)}
    if(input$collegesSelection=="Alaska Native Native Hawaiian-serving Institutions (ANNHIs)"){gei <- gei %>% filter(`annhi`==1)}
    if(input$collegesSelection=="Native American Non-Tribal Institutions (NANTIs)"){gei <- gei %>% filter(`nanti`==1)}
    if(input$collegesSelection=="All Minority Serving Institutions (MSIs)"){gei <- gei %>% filter(`msi`==1)}
    if(input$collegesSelection=="All non-MSIs"){gei <- gei %>% filter(`msi` != 1)}
    #### End #### 
    
    #### C1: If gei has no colleges, add a placeholder and a trigger warning ####
    if(nrow(gei)==0){
      warning1 <- "Your selected filters have reduced the dataset of programs too far. Please remove one of your filters and try again."
    }else{
      warning1 <- ""
    }
    #### End #### 
    
    #### S7A: Load in gep, the data on programs ####
    gep <- fread("Data/ge-programs.csv", header=TRUE)
    gep$Count <- rep(1, nrow(gep))
    #### End ####
    
    #### S7B: Merge gep with statesDF, controlsDF, and levelsDF ####
    gep <- left_join(x=gep, y=statesDF, by="st_fips")
    gep <- left_join(x=gep, y=controlsDF, by="control_peps")
    gep <- left_join(x=gep, y=levelsDF, by="cred_lvl")
    #### End ####
    
    #### S8: Filter gep by stateSelection, controlSelection, and collegesSelection ####
    if(input$stateSelection != "---"){
      gep <- gep %>% filter(`StateName`==input$stateSelection)
    }
    
    if(input$controlSelection != "---"){
      gep <- gep %>% filter(`ControlName`==input$controlSelection)
    }
    
    if(input$collegesSelection=="Historically Black Colleges and Universities (HBCUs)"){gep <- gep %>% filter(`hbcu`==1)}
    if(input$collegesSelection=="Predominantly Black Institutions (PBIs)"){gep <- gep %>% filter(`pbi`==1)}
    if(input$collegesSelection=="Hispanic Serving Institutions (HSIs)"){gep <- gep %>% filter(`hsi`==1)}
    if(input$collegesSelection=="Asian American Native American Pacific Islander-serving Institution (AANAPIIs)"){gep <- gep %>% filter(`aanapii`==1)}
    if(input$collegesSelection=="Tribal Colleges and Universities (TCUs)"){gep <- gep %>% filter(tribal==1)}
    if(input$collegesSelection=="Alaska Native Native Hawaiian-serving Institutions (ANNHIs)"){gep <- gep %>% filter(`annhi`==1)}
    if(input$collegesSelection=="Native American Non-Tribal Institutions (NANTIs)"){gep <- gep %>% filter(`nanti`==1)}
    if(input$collegesSelection=="All Minority Serving Institutions (MSIs)"){gep <- gep %>% filter(`msi`==1)}
    if(input$collegesSelection=="All non-MSIs"){gep <- gep %>% filter(`msi` != 1)}
    #### End #### 
    
    #### C2: If gep has no GE programs, add a placeholder and a trigger warning ####

    if(sum(gep.ge$inGE, na.rm=TRUE)==0){
      warning2 <- "Your selected filters have reduced the dataset of programs too far. Please remove one of your filters and try again."
    }else{
      warning2 <- ""
    }
    #### End #### 
    
    #### S9A: Tell gei whether each college has a failing program using opeid6 ####
    gep <- gep %>% mutate(`Has any failing program` = ifelse(`passfail_2019` %in% c("Fail both DTE and EP", "Fail DTE only", "Fail EP only"), 1, 0))
    gep.anyfail <- gep %>% filter(`Has any failing program` == 1) 
    failing.opeids <- unique(gep.anyfail$opeid6)
    gei$`Has any failing program` <- ifelse(gei$`opeid6` %in% failing.opeids, "Yes", "No")
    rm("gep.anyfail")
    #### End #### 
    
    #### S9B: Create variables pfDTE and pfEP in gep ####
    gep$`pfDTE` <- rep(NA, nrow(gep))
    gep$`pfDTE`[gep$`fail_DTE_2019`==1] <- "Fail DTE"
    gep$`pfDTE`[gep$`fail_DTE_2019`==0] <- "Pass DTE"
    
    gep$`pfEP` <- rep(NA, nrow(gep))
    gep$`pfEP`[gep$`fail_EP_2019`==1] <- "Fail EP"
    gep$`pfEP`[gep$`fail_EP_2019`==0] <- "Pass EP"
    
    gep$`pfDTE`[gep$`passfail_2019`=="No DTE/EP data"] <- "Insufficient data for DTE"
    gep$`pfEP`[gep$`passfail_2019`=="No DTE/EP data"] <- "Insufficient data for EP"
    #### End ####
    
    #### S10: Filter gep by fieldSelection and levelSelection ####
    if(input$fieldSelection != "---"){
      if(grepl("2-digit", input$fieldSelection)==TRUE){
        cips <- cips %>% filter(`Digits` == "2-digit CIP") %>% select(!(Digits)) %>% select(!(CIPnum))
        names(cips) <- c("cip2_title_2010", "FieldName")
        gep <- left_join(x=gep, y=cips, by="cip2_title_2010")
        gep <- gep %>% filter(`FieldName` == input$fieldSelection)
      }else{
        cips <- cips %>% filter(`Digits` == "4-digit CIP") %>% select(!(Digits)) %>% select(!(CIPnum))
        names(cips) <- c("cipdesc", "FieldName")
        gep <- left_join(x=gep, y=cips, by="cipdesc")
        gep <- gep %>% filter(`FieldName` == input$fieldSelection)
      }
    }
    
    if(input$levelSelection != "---"){
      gep <- gep %>% filter(`LevelName`==input$levelSelection)
    }
    #### End #### 
    
    #### C3: If gep has no programs, add a placeholder and a trigger warning ####
    if(sum(gep.ge$inGE, na.rm=TRUE)==0){
      warning3 <- "Your selected filters have reduced the dataset of programs too far. Please remove one of your filters and try again."
    }else{
      warning3 <- ""
    }
    #### End #### 
    
    ######################## Producing Outputs ######################## 
    
    #### S11: Produce 'initialCount' ####
  
    all.programs <- nrow(gep)
    gep <- gep %>% filter(`inGE`==1)
    
    if(nrow(gep)==0){
      output$initialCount <- renderText({
        if(warning1 != ""){warning1}else{if(warning2 != ""){warning2}else{warning3}}
      })
    }else{
      output$initialCount <- renderText({
        paste(
          "A total of ",
          all.programs, 
          " programs across ", 
          comma(nrow(gei)),
          " colleges match your selections. Of these, ", 
          comma(nrow(gei)),
          " programs are subject to the GE rules.",
          sep=""
          )
      })
    }
    #### End ####
    
    #### S12: Produce 'collegesMap' ####
    
    gei <- gei %>% mutate(`Share of all programs that are GE` = `inGE` / `count`)
    geo <- gei %>% select(`schname`, 
                          `longitud`, 
                          `latitude`, 
                          `StateName`, 
                          `Has a GE program`,
                          `ControlName`, 
                          `inGE`, 
                          `count`,
                          `Share of all programs that are GE`, 
                          `Number of students in failing program`, 
                          `Total number of students in all programs`, 
                          `Share of students in failing programs`) 
    names(geo) <- c("Name", 
                    "Longitude", 
                    "Latitude", 
                    "State", 
                    "Has a GE program",
                    "Control", 
                    "GE programs", 
                    "Total programs",
                    "Share of all programs that are GE programs", 
                    "Number of students in failing program", 
                    "Total number of students in all programs", 
                    "Share of students in failing programs")
    geo <- geo %>% filter(is.na(`State`)==FALSE) %>% filter(is.na(`Has a GE program`)==FALSE)
    
    geo$hover <- paste(
      "Name: ", geo$Name, '\n',
      "State: ", geo$State, '\n',
      "Control: ", geo$`Control`, '\n',
      "Number of GE programs: ", geo$`GE programs`, '\n',
      "Number of total programs: ", geo$`Total programs`, '\n',
      "Share of all students who are in failing GE programs: ", percent(geo$`Share of students in failing programs`, accuracy=0.1), 
      sep=""
    )
    
    geo <- geo %>% arrange(`Share of students in failing programs`)
    
    output$collegesMap <- renderPlotly({
      if(plotSelection=="Map of colleges by GE status"){
        if(nrow(geo) > 0){
          plot_geo(geo, locationmode='USA-states') %>% add_trace(type="scatter", mode="markers", lat=~Latitude, lon=~Longitude, text=~hover, color=~`Share of students in failing programs`, colors = c("#1B98E0","black")) %>% layout(geo = list(scope = 'usa'))
        }else{
          plot_geo(geo, locationmode='USA-states')
        }
      }
    })
    
    #### End #### 
    
    #### S13: Produce 'DebtPlot' #### 
    
    if(plotSelection=="Debt-to-earnings chart"){
      if(nrow(gep)==0){
        
      }else{
        
      }
    }
    
    #### End #### 
    
    #### S14: Produce 'EarningsPlot' #### 
    
    if(plotSelection=="Earnings premium chart"){
      if(nrow(gep)==0){
        
      }else{
        
      }
    }
    
    #### End #### 
    
    #### S15: Produce 'geMatrix' ####
    
    if(tableSelection=="Pass/fail outcomes"){
      if(nrow(gep)==0){
        
      }else{
        
      }
    }

    #### End #### 
    
    #### S16: Produce 'simulationTable' ####
    
    if(tableSelection=="Simulating changes to debt and earnings"){
      if(nrow(gep)==0){
        
      }else{
        
      }
    }
    
    #### End ####
    
    #### S17: Produce 'programsTable' ####
    
    if(tableSelection=="Table of programs"){
      if(nrow(gep)==0){
        
      }else{
        
      }
    }
    
    #### End ####
    
    #### S18: Produce 'collegesTable' ####
    
    if(tableSelection=="Table of colleges"){
      if(nrow(gei)==0){
        
      }else{
        
      }
    }
    
    #### End ####
      
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
