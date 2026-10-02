# Rules shared by the scripts of this task.
vacant_classes <- c("100", "190", "241", "390", "590", "580", "990")  # vacant land and minor improvements
lineage_share <- 0.1  # an old parcel covering at least this share of a new parcel is under it

# Apartment classes: 3xx and 9xx, except 399 (a rented condominium unit) and the minor-improvement classes 390 and 990.
is_large <- function(class) substr(class, 1, 1) %in% c("3", "9") & !class %in% c("399", "390", "990")
