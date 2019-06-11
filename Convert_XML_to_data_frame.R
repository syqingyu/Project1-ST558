library(xml2)

# Read xml file 
xml_file <- read_xml("https://data.cdc.gov/api/views/w9j2-ggv5/rows.xml?accessType=DOWNLOAD")
# Show the xml structure
# xml_structure(xml_file)
# Check the xml structure size
xml_length(xml_file)
xml_length(xml_children(xml_file))
xml_length(xml_child(xml_children(xml_file), 1))
xml_length(xml_child(xml_child(xml_children(xml_file), 1)))

# Check if each child has same amount of variables
max(xml_length(xml_children(xml_children(xml_file))))
min(xml_length(xml_children(xml_children(xml_file))))

# Set number of columns for final dataframe
ncols <- max(xml_length(xml_children(xml_children(xml_file))))

# Find the index of first element whose length is ncols
ind_name <- which(xml_length(xml_children(xml_children(xml_file)))==5)[1]

# Change it to a large list
xml_list <- as_list(xml_file)

# Check the list size
length(xml_list)
length(xml_list[[1]])
length(xml_list[[1]][[1]])
length(xml_list[[1]][[1]][[2]])

# For-loop to combine list to a dataframe
df <- data.frame(matrix(ncol = ncols, nrow = 0))
colnames(df) <- names(xml_list[[1]][[1]][[ind_name]])
for (i in 1:length(xml_list[[1]][[1]])){
  tmp <- as.data.frame(xml_list[[1]][[1]][[i]])
  colnames(tmp) <- names(xml_list[[1]][[1]][[i]])
  df <- merge(df, tmp, all = TRUE)
}
View(df)
