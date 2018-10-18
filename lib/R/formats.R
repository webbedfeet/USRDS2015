agegrps <- c('18-29','30-39','40-49','50-59','60-69','70+')

term <- c('(Intercept)', 'INC_AGE','SEX','DIABETES','ZSCORE','Cancer','Cardia','Cva','Hyper',
          'Ihd', 'Pulmon','Pvasc','Smoke','HISPANIC','BMI','DIALTYPE',
          'REGION','RACE2','HispGrps')
reflevels <- c('','','Male','','','','','','','',
               '','','','','18.5-24.9','Hemodialysis','Northeast','White','Nonhispanic White')
names(reflevels) <- term
termLabels <- c('(Intercept)'='Intercept',
                'INC_AGE'='Age',
                'SEX' = 'Sex',
                'DIABETES' = 'Diabetes mellitus',
                'ZSCORE' = 'Socio-economic score',
                'Cancer' = 'Cancer',
                'Cardia' = 'Heart failure',
                'Cva' = 'Cerebrovascular accident',
                'Hyper' = 'Hypertension',
                'Ihd' = 'Ischemic heart disease',
                'Pulmon' = 'Chronic obstructive lung disease',
                'Pvasc' = 'Peripheral vascular disease',
                'Smoke' = 'Current smoker',
                'RACE2' = 'Race',
                'HISPANIC' = 'Hispanic',
                'BMI' = 'Body Mass Index (kg/m^2^)',
                'DIALTYPE' = 'Dialysis',
                'REGION' = 'Region',
                'HispGrps' = 'Hispanic groups')

agegrp_labels <- c('[18,29]' = 'Age 18-29',
                   '(29,39]' = 'Age 30-39',
                   '(39,49]' = 'Age 40-49',
                   '(49,59]' = 'Age 50-59',
                   '(59,69]' = 'Age 60-69',
                   '(69,100]' = 'Age 70 and older')
race_labels = c('Nonhispanic White'='White',
                'Nonhispanic Black' = 'Black',
                'Nonhispanic Asian' = 'Asian/Pacific Islander',
                'Nonhispanic Native American' = 'AI/AN',
                'Hispanic'='Hispanic')
bmi_labels = c('< 18.5' = '< 18.5',
               '18.5-25' = '18.5-24.9',
               '25-30' = '25-29.9',
               'greater than 30' = '> 30')

hisp_term <- term; hisp_term <- hisp_term[-14]
hisp_reflevels <- reflevels[-14];
names(hisp_reflevels) <- hisp_term
