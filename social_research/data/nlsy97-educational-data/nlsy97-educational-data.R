
# Set working directory
# setwd()


new_data <- read.table('data/nlsy97-educational-data/nlsy97-educational-data.dat', sep=' ')
names(new_data) <- c('R0000100',
'R0536300',
'R0536401',
'R0536402',
'R1235800',
'R1236101',
'R1302600',
'R1302700',
'R1482600',
'U1846000',
'U1855400')


# Handle missing values

new_data[new_data == -1] = NA  # Refused
new_data[new_data == -2] = NA  # Dont know
new_data[new_data == -3] = NA  # Invalid missing
new_data[new_data == -4] = NA  # Valid missing
new_data[new_data == -5] = NA  # Non-interview


# If there are values not categorized they will be represented as NA

vallabels = function(data) {
  data$R0536300 <- factor(data$R0536300,
levels=c(0.0,1.0,2.0),
labels=c("No Information",
"Male",
"Female"))
  data$R0536401 <- factor(data$R0536401,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0),
labels=c("1: January",
"2: February",
"3: March",
"4: April",
"5: May",
"6: June",
"7: July",
"8: August",
"9: September",
"10: October",
"11: November",
"12: December"))
  data$R1235800 <- factor(data$R1235800,
levels=c(0.0,1.0),
labels=c("Oversample",
"Cross-sectional"))
  data$R1302600 <- factor(data$R1302600,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
labels=c("NONE",
"1ST GRADE",
"2ND GRADE",
"3RD GRADE",
"4TH GRADE",
"5TH GRADE",
"6TH GRADE",
"7TH GRADE",
"8TH GRADE",
"9TH GRADE",
"10TH GRADE",
"11TH GRADE",
"12TH GRADE",
"1ST YEAR COLLEGE",
"2ND YEAR COLLEGE",
"3RD YEAR COLLEGE",
"4TH YEAR COLLEGE",
"5TH YEAR COLLEGE",
"6TH YEAR COLLEGE",
"7TH YEAR COLLEGE",
"8TH YEAR COLLEGE OR MORE",
"UNGRADED"))
  data$R1302700 <- factor(data$R1302700,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
labels=c("NONE",
"1ST GRADE",
"2ND GRADE",
"3RD GRADE",
"4TH GRADE",
"5TH GRADE",
"6TH GRADE",
"7TH GRADE",
"8TH GRADE",
"9TH GRADE",
"10TH GRADE",
"11TH GRADE",
"12TH GRADE",
"1ST YEAR COLLEGE",
"2ND YEAR COLLEGE",
"3RD YEAR COLLEGE",
"4TH YEAR COLLEGE",
"5TH YEAR COLLEGE",
"6TH YEAR COLLEGE",
"7TH YEAR COLLEGE",
"8TH YEAR COLLEGE OR MORE",
"UNGRADED"))
  data$R1482600 <- factor(data$R1482600,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Black",
"Hispanic",
"Mixed Race (Non-Hispanic)",
"Non-Black / Non-Hispanic"))
  data$U1846000 <- factor(data$U1846000,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0),
labels=c("None",
"GED",
"High school diploma (Regular 12 year program)",
"Associate/Junior college (AA)",
"Bachelor's degree (BA, BS)",
"Master's degree (MA, MS)",
"PhD",
"Professional degree (DDS, JD, MD)"))
return(data)
}


# If there are values not categorized they will be represented as NA

vallabels_continuous = function(data) {
data$R0000100[1.0 <= data$R0000100 & data$R0000100 <= 999.0] <- 1.0
data$R0000100[1000.0 <= data$R0000100 & data$R0000100 <= 1999.0] <- 1000.0
data$R0000100[2000.0 <= data$R0000100 & data$R0000100 <= 2999.0] <- 2000.0
data$R0000100[3000.0 <= data$R0000100 & data$R0000100 <= 3999.0] <- 3000.0
data$R0000100[4000.0 <= data$R0000100 & data$R0000100 <= 4999.0] <- 4000.0
data$R0000100[5000.0 <= data$R0000100 & data$R0000100 <= 5999.0] <- 5000.0
data$R0000100[6000.0 <= data$R0000100 & data$R0000100 <= 6999.0] <- 6000.0
data$R0000100[7000.0 <= data$R0000100 & data$R0000100 <= 7999.0] <- 7000.0
data$R0000100[8000.0 <= data$R0000100 & data$R0000100 <= 8999.0] <- 8000.0
data$R0000100[9000.0 <= data$R0000100 & data$R0000100 <= 9999.0] <- 9000.0
data$R0000100 <- factor(data$R0000100,
levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0),
labels=c("0",
"1 TO 999",
"1000 TO 1999",
"2000 TO 2999",
"3000 TO 3999",
"4000 TO 4999",
"5000 TO 5999",
"6000 TO 6999",
"7000 TO 7999",
"8000 TO 8999",
"9000 TO 9999"))
data$R1236101[30000.0 <= data$R1236101 & data$R1236101 <= 59999.0] <- 30000.0
data$R1236101[60000.0 <= data$R1236101 & data$R1236101 <= 99999.0] <- 60000.0
data$R1236101[100000.0 <= data$R1236101 & data$R1236101 <= 149999.0] <- 100000.0
data$R1236101[150000.0 <= data$R1236101 & data$R1236101 <= 199999.0] <- 150000.0
data$R1236101[200000.0 <= data$R1236101 & data$R1236101 <= 249999.0] <- 200000.0
data$R1236101[250000.0 <= data$R1236101 & data$R1236101 <= 299999.0] <- 250000.0
data$R1236101[300000.0 <= data$R1236101 & data$R1236101 <= 349999.0] <- 300000.0
data$R1236101[350000.0 <= data$R1236101 & data$R1236101 <= 399999.0] <- 350000.0
data$R1236101[400000.0 <= data$R1236101 & data$R1236101 <= 449999.0] <- 400000.0
data$R1236101[450000.0 <= data$R1236101 & data$R1236101 <= 499999.0] <- 450000.0
data$R1236101[500000.0 <= data$R1236101 & data$R1236101 <= 549999.0] <- 500000.0
data$R1236101[550000.0 <= data$R1236101 & data$R1236101 <= 599999.0] <- 550000.0
data$R1236101[600000.0 <= data$R1236101 & data$R1236101 <= 649999.0] <- 600000.0
data$R1236101[650000.0 <= data$R1236101 & data$R1236101 <= 699999.0] <- 650000.0
data$R1236101[700000.0 <= data$R1236101 & data$R1236101 <= 749999.0] <- 700000.0
data$R1236101[750000.0 <= data$R1236101 & data$R1236101 <= 799999.0] <- 750000.0
data$R1236101[800000.0 <= data$R1236101 & data$R1236101 <= 849999.0] <- 800000.0
data$R1236101[850000.0 <= data$R1236101 & data$R1236101 <= 9999999.0] <- 850000.0
data$R1236101 <- factor(data$R1236101,
levels=c(0.0,30000.0,60000.0,100000.0,150000.0,200000.0,250000.0,300000.0,350000.0,400000.0,450000.0,500000.0,550000.0,600000.0,650000.0,700000.0,750000.0,800000.0,850000.0),
labels=c("0",
"30000 TO 59999: 300.00-599.99",
"60000 TO 99999: 600.00-999.99",
"100000 TO 149999: 1000.00-1499.99",
"150000 TO 199999: 1500.00-1999.99",
"200000 TO 249999: 2000.00-2499.99",
"250000 TO 299999: 2500.00-2999.99",
"300000 TO 349999: 3000.00-3499.99",
"350000 TO 399999: 3500.00-3999.99",
"400000 TO 449999: 4000.00-4499.99",
"450000 TO 499999: 4500.00-4999.99",
"500000 TO 549999: 5000.00-5499.99",
"550000 TO 599999: 5500.00-5999.99",
"600000 TO 649999: 6000.00-6499.99",
"650000 TO 699999: 6500.00-6999.99",
"700000 TO 749999: 7000.00-7499.99",
"750000 TO 799999: 7500.00-7999.99",
"800000 TO 849999: 8000.00-8499.99",
"850000 TO 9999999: 8500.00+"))
data$U1855400[30000.0 <= data$U1855400 & data$U1855400 <= 59999.0] <- 30000.0
data$U1855400[60000.0 <= data$U1855400 & data$U1855400 <= 99999.0] <- 60000.0
data$U1855400[100000.0 <= data$U1855400 & data$U1855400 <= 149999.0] <- 100000.0
data$U1855400[150000.0 <= data$U1855400 & data$U1855400 <= 199999.0] <- 150000.0
data$U1855400[200000.0 <= data$U1855400 & data$U1855400 <= 249999.0] <- 200000.0
data$U1855400[250000.0 <= data$U1855400 & data$U1855400 <= 299999.0] <- 250000.0
data$U1855400[300000.0 <= data$U1855400 & data$U1855400 <= 349999.0] <- 300000.0
data$U1855400[350000.0 <= data$U1855400 & data$U1855400 <= 399999.0] <- 350000.0
data$U1855400[400000.0 <= data$U1855400 & data$U1855400 <= 449999.0] <- 400000.0
data$U1855400[450000.0 <= data$U1855400 & data$U1855400 <= 499999.0] <- 450000.0
data$U1855400[500000.0 <= data$U1855400 & data$U1855400 <= 549999.0] <- 500000.0
data$U1855400[550000.0 <= data$U1855400 & data$U1855400 <= 599999.0] <- 550000.0
data$U1855400[600000.0 <= data$U1855400 & data$U1855400 <= 649999.0] <- 600000.0
data$U1855400[650000.0 <= data$U1855400 & data$U1855400 <= 699999.0] <- 650000.0
data$U1855400[700000.0 <= data$U1855400 & data$U1855400 <= 749999.0] <- 700000.0
data$U1855400[750000.0 <= data$U1855400 & data$U1855400 <= 799999.0] <- 750000.0
data$U1855400[800000.0 <= data$U1855400 & data$U1855400 <= 849999.0] <- 800000.0
data$U1855400[850000.0 <= data$U1855400 & data$U1855400 <= 9999999.0] <- 850000.0
data$U1855400 <- factor(data$U1855400,
levels=c(0.0,30000.0,60000.0,100000.0,150000.0,200000.0,250000.0,300000.0,350000.0,400000.0,450000.0,500000.0,550000.0,600000.0,650000.0,700000.0,750000.0,800000.0,850000.0),
labels=c("0",
"30000 TO 59999: 300.00-599.99",
"60000 TO 99999: 600.00-999.99",
"100000 TO 149999: 1000.00-1499.99",
"150000 TO 199999: 1500.00-1999.99",
"200000 TO 249999: 2000.00-2499.99",
"250000 TO 299999: 2500.00-2999.99",
"300000 TO 349999: 3000.00-3499.99",
"350000 TO 399999: 3500.00-3999.99",
"400000 TO 449999: 4000.00-4499.99",
"450000 TO 499999: 4500.00-4999.99",
"500000 TO 549999: 5000.00-5499.99",
"550000 TO 599999: 5500.00-5999.99",
"600000 TO 649999: 6000.00-6499.99",
"650000 TO 699999: 6500.00-6999.99",
"700000 TO 749999: 7000.00-7499.99",
"750000 TO 799999: 7500.00-7999.99",
"800000 TO 849999: 8000.00-8499.99",
"850000 TO 9999999: 8500.00+"))
return(data)
}

varlabels <- c("PUBID - YTH ID CODE 1997",
"KEY!SEX (SYMBOL) 1997",
"KEY!BDATE M/Y (SYMBOL) 1997",
"KEY!BDATE M/Y (SYMBOL) 1997",
"CV_SAMPLE_TYPE 1997",
"R1 SAMPLE WEIGHT CC 1997",
"CV_HGC_RES_DAD 1997",
"CV_HGC_RES_MOM 1997",
"KEY!RACE_ETHNICITY (SYMBOL) 1997",
"CV_HIGHEST_DEGREE_EVER 2017",
"R18 SAMPLE WEIGHT CC 2017"
)


# Use qnames rather than rnums

qnames = function(data) {
names(data) <- c("PUBID_1997",
"KEY_SEX_1997",
"KEY_BDATE_M_1997",
"KEY_BDATE_Y_1997",
"CV_SAMPLE_TYPE_1997",
"SAMPLING_WEIGHT_CC_1997",
"CV_HGC_RES_DAD_1997",
"CV_HGC_RES_MOM_1997",
"KEY_RACE_ETHNICITY_1997",
"CV_HIGHEST_DEGREE_EVER_EDT_2017",
"SAMPLING_WEIGHT_CC_2017")
return(data)
}


#********************************************************************************************************

# Remove the '#' before the following line to create a data file called "categories" with value labels.
# categories <- vallabels(new_data)

# Remove the '#' before the following lines to rename variables using Qnames instead of Reference Numbers
new_data <- qnames(new_data)
# categories <- qnames(categories)

# Produce summaries for the raw (uncategorized) data file
summary(new_data)

# Remove the '#' before the following lines to produce summaries for the "categories" data file.
#categories <- vallabels(new_data)
#categories <- vallabels_continuous(new_data)
#summary(categories)

#************************************************************************************************************


