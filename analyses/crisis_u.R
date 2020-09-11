library(tidyverse); library(readxl)

majors <-
  c(
    "Family and Consumer Sciences/Human Sciences, General.",
    "Psychology, General.",
    "Social Work.",
    "Sociology.",
    "Neurobiology and Neurosciences.",
    "Social Sciences, Other.",
    "Health Services/Allied Health/Health Sciences, General.",
    "Medicine.",
    "Mental and Social Health Services and Allied Professions.",
    "Registered Nursing, Nursing Administration, Nursing Research and Clinical Nursing.",
    "Human Development, Family Studies, and Related Services.",
    "Social Sciences, General.",
    "Family and Consumer Sciences/Human Sciences, Other.",
    "Biopsychology.",
    "Behavioral Sciences.",
    "Psychology, Other.",
    "Research and Experimental Psychology.",
    "Family and Consumer Sciences/Human Sciences Business Services.",
    "Cognitive Science.",
    "Sociology and Anthropology.",
    "Developmental and Child Psychology.",
    "Clinical Psychology.",
    "Physiological Psychology/Psychobiology.",
    "Counseling Psychology.",
    "Cognitive Psychology and Psycholinguistics.",
    "Social Psychology."
  )


by_field <- 
  read_csv(
    "data/Most-Recent-Field-Data-Elements.csv",
    na = c("NULL","","NA")
  )

colleges <- 
  read_csv(
    "data/CollegeScorecard_Raw_Data/MERGED2017_18_PP.csv",
    na = c("NULL","","NA")
  )

dictionary <- read_excel("data/CollegeScorecardDataDictionary.xlsx",sheet = "data_dictionary")

by_field <- 
  read_csv(
    "../data/Most-Recent-Field-Data-Elements.csv",
    na = c("NULL","","NA")
  )

colleges <- 
  read_csv(
    "../data/CollegeScorecard_Raw_Data/MERGED2017_18_PP.csv",
    na = c("NULL","","NA")
  )

dictionary <- read_excel("../data/CollegeScorecardDataDictionary.xlsx",sheet = "data_dictionary")

fields <-
  by_field %>%
  filter(CREDLEV == 3) %>%
  select(CIPCODE,CIPDESC) %>%
  distinct() %>%
  filter(
    str_detect(
      CIPDESC,
      paste(majors,collapse = "|")
    )
  )

by_field <-
  by_field %>%
  filter(CREDLEV %in% c(3,5,6)) %>%
  filter(CIPCODE %in% fields$CIPCODE)

field_pivot <-
  by_field %>%
  mutate(
    CIPDESC = str_replace_all(str_to_lower(CIPDESC), "[:punct:]| ","_"),
    CIPDESC = str_replace_all(CIPDESC, "_$",""),
    COUNT = as.numeric(COUNT),
    COUNT = replace_na(COUNT,0),
    has = 1
  ) %>%
  select(UNITID,CIPDESC,has) %>%
  distinct() %>%
  pivot_wider(
    id_cols = UNITID,
    names_from = CIPDESC,
    values_from = has,
    values_fill = list(has = 0)
  ) 

df <-
  colleges %>%
  filter(UNITID %in% unique(by_field$UNITID)) %>%
  select(
    UNITID,OPEID,INSTNM,
    CONTROL,RELAFFIL,UGDS,UG,STABBR,
    REGION,LOCALE,LOCALE2,
    ST_FIPS,CITY,LATITUDE,LONGITUDE,
    ends_with("DEG"),starts_with("PCIP"),
    starts_with("CC"),
    starts_with("SAT"),
    starts_with("NPT4_"),starts_with("NPT45_"),
    starts_with("TUITIONFEE"),INEXPFTE,
    starts_with("CIPTFB"),
    ends_with("RPY_1YR_RT"),ends_with("RPY_3YR_RT"),
    ends_with("RPY_7YR_RT"),
    ends_with("DEBT_MDN"),
    MD_EARN_WNE_P8,ends_with("EARN_WNE_P10"),
    starts_with("MTHCMP")
  ) %>%
  mutate(
    CONTROL = recode(
      as.character(CONTROL),
      `Public` = '1',
      `Private non-profit` = '2',
      `Private for-profit` = '3'
    )
  )


write_csv(df,"analyses/crisis_univ.csv")
