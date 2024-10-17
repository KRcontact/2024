#########################################################
################                         ################
################ GENERATE CONTACT MATRIX ################
################                         ################
#########################################################

# Required libraries
library(data.table)
library(readxl)
library(socialmixr)

# Function to generate contact matrices
generate_contact_matrices <- function(participant_data, contact_data, population_file, population_sheet) {
  # Read population data
  population <- read_xlsx(population_file, sheet = population_sheet)
  population$frequency <- as.numeric(as.character(population$freq))
  population_proportions <- population$frequency / sum(population$frequency, na.rm = TRUE)
  
  # Calculate sample sizes for each age group
  sample_sizes <- round(nrow(participant_data) * population_proportions)
  age_groups <- levels(participant_data$age_group5)
  
  # Initialize list to store matrices
  matrices <- list()
  
  # Generate 10,000 bootstrapped matrices
  for (iteration in 1:10000) {
    sampled_participants <- data.table()
    sampled_contacts <- data.table()
    
    # Sample participants and contacts for each age group
    for (age_group_index in seq_along(age_groups)) {
      for (sample_index in 1:sample_sizes[age_group_index]) {
        # Sample a participant from the current age group
        sampled_id <- sample(participant_data[age_group5 == age_groups[age_group_index]]$subid, 1)
        
        # Get participant and contact data for the sampled ID
        participant <- participant_data[subid == sampled_id]
        contact <- contact_data[subid == sampled_id]
        
        # Assign unique participant IDs
        participant$part_id <- paste0(participant$subid, sample_index)
        contact$part_id <- paste0(contact$subid, sample_index)
        
        # Append to sampled data
        sampled_participants <- rbind(sampled_participants, participant)
        sampled_contacts <- rbind(sampled_contacts, contact)
      }
    }
    
    # Convert participant IDs to numeric
    sampled_participants$part_id <- as.numeric(sampled_participants$part_id)
    sampled_contacts$part_id <- as.numeric(sampled_contacts$part_id)
    
    # Convert contact age columns to numeric
    sampled_contacts[, c("cnt_age_exact", "cnt_age_est_min", "cnt_age_est_max") := 
                       lapply(.SD, function(x) as.numeric(as.character(x))), 
                     .SDcols = c("cnt_age_exact", "cnt_age_est_min", "cnt_age_est_max")]
    
    # Impute missing exact ages with a random value between min and max estimates
    sampled_contacts[is.na(cnt_age_exact) & !is.na(cnt_age_est_min) & !is.na(cnt_age_est_max), 
                     cnt_age_exact := sample(round(cnt_age_est_min):round(cnt_age_est_max), 1), 
                     by = .I]
    
    # Create survey object
    survey_data <- survey(sampled_participants, sampled_contacts)
    
    # Define age limits for the contact matrix
    age_limits <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75)
    
    # Generate contact matrix
    contact_mat <- contact_matrix(survey_data, 
                                  age.limits = age_limits, 
                                  symmetric = TRUE, 
                                  missing.contact.age = "sample", 
                                  n = 1)
    
    # Replace NA values with 0
    contact_mat$matrix[is.na(contact_mat$matrix)] <- 0
    
    # Add matrix to the list
    matrices[[iteration]] <- contact_mat$matrix
  }
  
  return(matrices)
}

# File paths
participant_file <- "D:/KR/contact/SouthKorea_participant_common.xlsx"
contact_file <- "D:/KR/contact/SouthKorea_contact_common.xlsx"
population_file <- "D:/KR/contact/KR_population_2023.xlsx"

# Read participant data
participants <- read_xlsx(participant_file)
participants <- participants[, c("part_id", "part_age")]
setnames(participants, "part_id", "subid")
participants$country <- "Korea"
participants$age_group5 <- cut(participants$part_age, c(seq(0, 75, 5), 100), right = FALSE)
participants <- as.data.table(participants)

# Read contact data
contacts <- read_xlsx(contact_file)
contacts <- contacts[, c("part_id", "cnt_age_exact", "cnt_age_est_min", "cnt_age_est_max")]
setnames(contacts, "part_id", "subid")
contacts <- as.data.table(contacts)

# Generate contact matrices
matrices <- generate_contact_matrices(participants, contacts, population_file, 1)

# Calculate mean bootstrapped matrix
mean_matrix <- Reduce("+", matrices) / length(matrices)



##############################################################
################                              ################
################ VISUALIZATION CONTACT MATRIX ################
################                              ################
##############################################################

# Required libraries
library(ggplot2)
library(reshape2)

# Convert the mean_matrix to a data frame for ggplot
matrix_df <- melt(mean_matrix)
colnames(matrix_df) <- c("Age_contactor", "Age_contactee", "value")

# Define custom color palette
custom_colors <- c("#0D5257", "#00BF6F", "#07e88a", "#8af5c8", "#ffe09a", "#FFB81C", "#FE5000", "red")

# Create the matrix plot
matrix_plot <- ggplot(matrix_df, aes(x = Age_contactor, y = Age_contactee, fill = value)) +
  geom_tile() +
  # Set custom color scale with defined limits
  scale_fill_gradientn(colors = custom_colors, 
                       limits = c(0, 15),  # Set min and max values for color scale
                       na.value = "#A7A8AA") +
  # Set labels for axes and legend
  labs(x = "Age of contactor", y = "Age of contactee", fill = "Contact rate", 
       title = "Mean contacts per day") +
  # Apply minimal theme for clean look
  theme_minimal() +
  # Customize theme elements
  theme(
    legend.position = "right",  # Move legend to the right side
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),  # Rotate x-axis labels
    legend.title = element_text(angle = 90, vjust = 0.5),  # Rotate legend title
    panel.background = element_rect(fill = "#FFFFFF"),  # Set white background
    legend.key.height = unit(1, "cm"),  # Increase legend key height
    legend.key.width = unit(0.5, "cm")  # Set legend key width
  ) +
  # Remove expansion of axes
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  # Ensure tiles are square
  coord_fixed() +
  # Customize the color scale guide
  guides(fill = guide_colorbar(
    title = "Contact rate",
    title.position = "right",
    title.theme = element_text(angle = 90, hjust = 0.5),
    barwidth = 1,
    barheight = 15,
    frame.colour = "#FFFFFF",
    ticks.colour = "#FFFFFF"
  ))

# Display the plot
print(matrix_plot)
