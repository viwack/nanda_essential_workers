# Individual 14 Types Breakdown

#health practitioners
health_practitioners <- df %>%
  select(TRACT_FIPS20, State, WRKR, 
         EWRKR_TOT, EWRKR_PROP, health_pract_p, health_pract, 
         PHISPANIC16_20, PNHWHITE16_20, PNHBLACK16_20, PFBORN16_20, PLIMENG16_20,
         PED1_16_20, PED2_16_20, PED3_16_20,
         PFAMINCGE125K16_20,PFAMINCGE40LT75K16_20, PFAMINCGE75LT125K16_20, PFAMINCGE125K16_20,
         AFFLUENCE16_20, DISADVANTAGE16_20, MEDFAMINC16_20)

health_practitioners %>%
  group_by(State) %>%
  filter(!is.na(health_pract_p)) %>%
  mutate(cv_HP = sd(health_pract_p, na.rm = TRUE) / mean(health_pract_p, na.rm = TRUE)) %>%
  mutate(mean_HP = mean(health_pract_p)) %>%
  select(State, mean_HP, cv_HP) %>%
  arrange(desc(mean_HP)) %>%
  distinct(State, .keep_all = TRUE) %>%
  print(n = 11)
