#Module : Team Research and Development Project
#A1:Descriptive Statistics
#Star dataset : https://www.kaggle.com/datasets/deepu1109/star-dataset
#Dataset Format :6_class_csv.csv (csv_format)


#Part 1- Dataset Fundamentals (1a,1b)

library(readr)                           # Load the readr package
Stars <- read_csv("6 class csv.csv")     # Import the dataset
View(Stars)                              # Display the dataset
print(dim(Stars))                        # Display dataset dimensions
print(colnames(Stars))                   # Display column names
print(nrow(Stars))                       # Display number of rows
print(ncol(Stars))                       # Display number of columns
summary(Stars)                           # Display dataset information


colnames(Stars)[4] <- "Abs Magnitude (Mv)"
colnames(Stars)[5] <- "Type"
colnames(Stars)[6] <- "Color"     # Rename columns for easier understanding


#"Abs Magnitude(Mv)"renamed from “Absolute Magnitude”
#Based on Kaggle data description "StarName" Column is created. 
#To represent column the "Star type" in characters instead of Numerical Values.

Stars$StarName <- ifelse(Stars$Type==0,"Brown Dwarf",
                         ifelse(Stars$Type==1,"Red Dwarf",
                                ifelse(Stars$Type==2,"White Dwarf",
                                       ifelse(Stars$Type==3,"Main Sequence",
                                              ifelse(Stars$Type==4,"Supergiant"
                                                     ,"Hypergiant")))))
print(head(Stars[, c("Temperature (K)",
                     "Luminosity(L/Lo)",
                     "Radius(R/Ro)",
                     "Abs Magnitude (Mv)",
                     "StarName")], 5))



#Part 2 – Descriptive Statistics (2a,2b)

#Calculate means values of each Column
print(mean(Stars$`Temperature (K)`))      
print(mean(Stars$`Luminosity(L/Lo)`))       
print(mean(Stars$`Radius(R/Ro)`))         
print(mean(Stars$`Abs Magnitude (Mv)`))   

# Calculate median values of each Column
print(median(Stars$`Temperature (K)`))    
print(median(Stars$`Luminosity(L/Lo)`))   
print(median(Stars$`Radius(R/Ro)`))       
print(median(Stars$`Abs Magnitude (Mv)`)) 

# Calculate standard deviation of each Column
print(sd(Stars$`Temperature (K)`))
print(sd(Stars$`Luminosity(L/Lo)`))
print(sd(Stars$`Radius(R/Ro)`))
print(sd(Stars$`Abs Magnitude (Mv)`))

# Display five-number summary of each Column
summary(Stars$`Temperature (K)`)
summary(Stars$`Luminosity(L/Lo)`)
summary(Stars$`Radius(R/Ro)`)
summary(Stars$`Abs Magnitude (Mv)`)

print(table(Stars$Color))                      # Frequency of each star colour         

print(table(Stars$Type))         #Each of the six star types contains 40 stars.


#Part 3 – A Simple Graph (3a,3b)
# Count each spectral class
star_counts <- table(Stars$`Spectral Class`)

# Create barplot
bp <- barplot(star_counts,
              main = "Distribution of Stars by Spectral Class",
              xlab = "Spectral Class",
              ylab = "Number of Stars",
              col = "lightblue",
              ylim = c(0, 125))

# Add count labels
text(bp, star_counts, labels = star_counts,
     pos = 3, cex = 0.8)

cor(Stars$`Radius(R/Ro)`,Stars$`Luminosity(L/Lo)`) # Radius and Luminosity
cor(Stars$`Temperature (K)`,Stars$`Luminosity(L/Lo)`)#Temperature and Luminosity

# Luminosity and Absolute Magnitude
cor(Stars$`Luminosity(L/Lo)`, Stars$`Abs Magnitude (Mv)`)

# Radius and Absolute Magnitude
cor(Stars$`Radius(R/Ro)`,Stars$`Abs Magnitude (Mv)`)

# Temperature and Absolute Magnitude
cor(Stars$`Temperature (K)`, Stars$`Abs Magnitude (Mv)`)


# Investigation Plan (4b)

Stars$Color <- trimws(Stars$Color)     # Remove extra spaces from colour names

# Replace different spellings of Blue-white with one standard name
Stars$Color[Stars$Color %in% c("Blue-White",
                               "Blue white", "Blue White")] <- "Blue-white"

Stars$Color[Stars$Color == "white"] <- "White" # Change lowercase white to White
print(table(Stars$Color))   # Display the updated colour names and their counts



