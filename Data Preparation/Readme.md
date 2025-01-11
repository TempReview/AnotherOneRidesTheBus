# Data Preparation

This folder contains scripts and data used in the secure data room of GESIS for our calculations. Where it is not possible to share the data, we provide information on how to calculate the data and scripts to automate these processes.


# Documentation for Running Scripts in the Secure Data Room at GESIS

To execute the scripts in the secure data room at GESIS, the following five files are required:

---

## 1. `ZA6294_plz_georef_v1-0-0.dta`
- **Source:** Provided by GESIS.
- **Description:** Contains the georeferences version of MESARAS.

---

## 2. `ZA6294_v1-0-0.dta`
- **Source:** Provided by GESIS.
- **Description:** Contains data from MESARAS.

---

## 3. `zip_statistics.csv`
- **Description:** 
  This file maps statistics from the INKAR database (stored as `Mesars_regional_indicators.csv`) to ZIP codes. 
  - **Note:** The geometries of ZIP codes are copyright-protected by Deutsche Post AG and cannot be shared. However, they are used in our dataset as helper data to match districts and grid files to ZIP codes. Therefore, we provide here the final result of this transformation, which includes, for each ZIP code regional statistics. A script for documentation is available in the folder `Scripts for Documentation`.


---

## 4. `cultural_amenities_MESARAS.csv`
- **Description:** 
  Contains additional information from INKAR. 
  - This data was added later in the research process and is not pre-mapped to ZIP codes like `zip_statistics.csv`. 
  - The mapping will occur during script execution in the secure data room.

---

## 5. `import_gesis.csv`
- **Description:** 
  Contains commuting times between different ZIP codes and specifies which connections are covered by student tickets.
  To create this dataset, please follow the guide below.

---

# Creating Data Source 5

### 1. **Executing `Download_from_Table.R`**
- **Requirements:**
  - An API key for the Google Maps API (enter in **line 6** of the script).
  - An appropriate query date in UNIX time (enter in **line 8** manually).
  
- **Input File:** 
  `prepared_distances.Rdata` 
  - This file contains tuples of ZIP codes and their calculated population-weighted centroids, derived from grid data. 
  - **Note:** The geometries of ZIP codes are copyright-protected by Deutsche Post AG and cannot be shared. However, they are used in our dataset as helper data to match districts and grid files to ZIP codes. Therefore, we provide here the final result of this transformation, which includes, for each ZIP code, the geopoints used for the Google query.

- **Output File:** 
  Running the script will generate `distances_around_universities_with_times.Rdata`.

---

### 2. **Executing `update_tickets.Rmd`**
- Open and execute this file to add wether each connection is covered by a student ticket.

---

# References

- **INKAR:**  
  Laufende Raumbeobachtung des BBSR - INKAR, Ausgabe 03/2024.  
  Hrsg.: Bundesinstitut für Bau-, Stadt- und Raumforschung (BBSR), Bonn.  
  [https://www.inkar.de/](https://www.inkar.de/)

- **Google Distance Matrix API:**  
  [Google Maps API Documentation](https://developers.google.com/maps/documentation/distance-matrix)

---



