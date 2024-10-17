# phytossociology_tools - A Suite of tools for vegetation structure analysis
Here I provide a suite of functions designed to streamline the analysis of phytosociological data. 
It includes tools for calculating key ecological indices, generating species abundance tables, and producing detailed visualizations of vegetation structure.

# Installation:
The easiest way is to download all the files and execute the script in the "R" folder. 
However, you can also download only the functions from the "func" folder and source or execute them in R

# Usage: 
## Function dap.eq
The dap.eq function calculates equivalent diameter based on multiple DBH (Diameter at Breast Height) measurements of a tree and also returns their respective volume (in m³). 

The function can be used as follows:
dap.eq (x = input_data, ff = form_factor)

- x: a data frame where the columns represent plots (parc), species (spp), height (H), diameters (DBH), or circumferences (CBH).
- ff: form factor; if absent, 0.5 will be used by default.
  
At the end of the process, the function returns a new data frame with the equivalent diameters and the volumes of the plants, calculated based on the provided form factor.

Forwarder details are provided inside the archive "FitoR.R".

## Function FitoR_camp
Function fitoR_camp calculates phytosociological parameters for **herbaceous layer or grassland**.

The function can be used as follows:
fitoR(x = input_data, area = plot_area_in_m2, filename = 'output_filename')

- x: a data frame where the columns represent plots (parc), species (spp), and absolute cover (cob).
- area: the area of the sampling units in square meters.
- filename: the name of the output file for the results, enclosed in single or double quotes.


At the end of the process, the function returns a list containing a summary of the data, including: the number of sampling units evaluated,
the total sampled area, species richness, Shannon-Wiener diversity (nats - calculated based on species cover) and Pielou's evenness.

Forwarder details are provided inside the archive "FitoR_camp.R".

## Function FitoR
Function fitoR calculates phytosociological parameters for **forest vegetation**.
The function can be used as follows:
fitoR(x=input_data, area = plot_area_in_m2, VI = VI_type, filename = 'output_filename')

- x: a data frame where the columns represent plots (parc), species (spp), height (H), diameters (DBH), or circumferences (CBH).
- area: the area of the sampling units in square meters.
- VI: type of importance value. Choose between "cottam" for the sum of the relativized parameters or "percent" (default) to return the average of the relativized parameters.
- filename: the name of the output file for the results, enclosed in single or double quotes.

At the end of the process, the function returns a list containing a summary of the data, including: basal area (m²/ha), density (ind./ha), 
species richness (-1, i.e., excluding dead specimens from the raw data), Shannon-Wiener diversity (nats), Pielou’s evenness. Also a 
data frame with the phytossociological parameters, based on the input data.

Forwarder details are provided inside the archive "FitoR.R".

### Fuction parc.resume
Function to summarize the data observed per sampling unit.

The function can be used as follows:
parc.resume(data = variable_input)

- data: a data frame containing in its columns the plots (parc), species (spp), height (H), diameters (DBH), or circumferences (CBH).
Values

At the end of the process, the function returns a list consisting of a summary of the data per sampling unit: abundance, richness (it's advisable to remove dead specimens to avoid counting them), Shannon-Wiener diversity (nats),
mean and standard deviation of height (m), mean and standard deviation of DBH or CBH (cm), Pielou's evenness, and basal area (m²).



