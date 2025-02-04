## Code from manuscript: 'Climate change threatens crop diversity at low latitudes'
#### Sara Heikonen, Matias Heino, Mika Jalava, Stefan Siebert, Daniel Viviroli, Matti Kummu

The ananlysis is composed of five MATLAB scripts and five R scripts:

#### 1) step1_crop_calendar_filters.R
- Create monthly filter rasters for filtering climate parameter only within the cropping season (1), for limited number of crops. Used for supplementary, seasonal SCS analysis.

#### 2) step2_create_ref_data.R
- Create Shapefiles and rasters of World Bank regions (2), latitude zones and elevation (3) zones
- Create total cropland mask rasters for main analysis with SPAM 2020 data (4), for supplementary analysis with SPAM 2005 (5) and SPAM 2010 data (6), and seasonal Safe Climatic Space (SCS) analysis
- Create crop production rasters for maize and soybean with matching coverage for seasonal SCS analysis

#### 3) step3_holdridge_download_data_missing_removed.m
- Modified from code from Kummu et al. (7). Downloads present and future climate parameter data datafrom Worldclim (8) and preprocess for next analysis steps. Data can be preprosessed either for the main annual SCS analysis or for the supplementry analaysis with seasonal SCS.

#### 4) step4_holdridge_define_classes.m
-  Modified from code from Kummu et al. (7). Classifies global land area into Holdridge life zones.

#### 5) step5_holdridge_calculate_change.m
-  Modified from code from Kummu et al. (7). Calculates future shifts in Holdridge life zones.

#### 6) step6_holdridge_analyses_main.m
-  Modified from code from Kummu et al. (7). Combines Holdridge data with crop production data. Delineates crop specific SCSs and share of production and cropland area within and outside the SCS. The script performs the analysis for one crop at a time with the following input parameters: env = 'local', spamcrop_name = (crop raster filename here without the '.tif'), scenarios = 'warming', spam_year = '2020' or '2010' or '2005'.
	   
#### 7) step7_reorganize_step6_results.m
- Reorganizes results collected from the parfor loop in step 6 for further analyses

#### 8) step8_crop_diversity.R
- Performs all main analyses in the manuscript.

#### 9) step9_figures_tables.R
- Produces all figures and reformats some output tables from previous steps

#### 10) step10_seasonal_vs_annual_SCS.R
- Comparison of results from seasonal and annual SCS approaches. Before running this script, the steps 3-8 should be run with annual SCS approach and seasonal SCS approach. The selction between approaches is made in step 3, and the steps after that are the same with both approaches.
  
### Data requirements
The data used for downloading the data and creating the Holdridge life zones are in the 'input' folder. In the 'ref_data' folder, there is a the country code file and raster and the crop group codes file that were used in these analyses.

The scripts have coherent file structure and external data needed to perform the analyses are listed in References (1-6 and 8).

### Software requirements

Used MATLAB version: 9.14.0.2239454 (R2023a)

Used external MATLAB packages and their versions:
cbarrow 1.1.0.0, crameri 1.08, export_fig 3.27, wprctile 1.0.0.0

Used R version: 4.4.1

Used R base packages: stats, graphics, grDevices, utils, datasets, methods, base

Used other R packages and their versions:
<pre>
lubridate_1.9.3     stringr_1.5.1       purrr_1.0.2        
readr_2.1.5         tibble_3.2.1        tidyverse_2.0.0    
ggalt_0.4.0         reshape2_1.4.4      ggtern_3.5.0       
RColorBrewer_1.1-3  tmap_3.3-4          scico_1.5.0        
colorspace_2.1-1    forcats_1.0.0       ggplot2_3.5.1      
matrixStats_1.4.1   rstudioapi_0.16.0   rhdf5_2.48.0       
rnaturalearth_1.0.1 tidyr_1.3.1         sf_1.0-16          
readxl_1.4.3        openxlsx_4.2.7.1    raster_3.6-30      
sp_2.1-4            dplyr_1.1.4         terra_1.7-83       
ncdf4_1.23
</pre>

### Refrerences

1. Jägermeyr, J., Müller, C., Minoli, S., Ray, D. & Siebert, S. GGCMI Phase 3 crop calendar. (2021) doi:10.5281/zenodo.5062513.

2. The World Bank Group. World Bank Country and Lending Groups – World Bank Data Help Desk. https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups.

3. Lehner, B., Verdin, K. & Jarvis, A. New Global Hydrography Derived From Spaceborne Elevation Data. Eos, Transactions American Geophysical Union 89, 93–94 (2008).

4. IFPRI. Global Spatially-Disaggregated Crop Production Statistics Data for 2020 Version 1.0. Harvard Dataverse https://doi.org/10.7910/DVN/SWPENT (2024).

5. IFPRI & IIASA. Global Spatially-Disaggregated Crop Production Statistics Data for 2005 Version 3.2. Harvard Dataverse https://doi.org/10.7910/DVN/DHXBJX (2016).
   
6. IFPRI. Global Spatially-Disaggregated Crop Production Statistics Data for 2010 Version 2.0. Harvard Dataverse https://doi.org/10.7910/DVN/PRFF8V (2019).

7. Kummu, M., Heino, M., Taka, M., Varis, O. & Viviroli, D. Climate change risks pushing one-third of global food production outside the safe climatic space. One Earth 4, 720–729 (2021).

8. Fick, S. E. & Hijmans, R. J. WorldClim 2: new 1-km spatial resolution climate surfaces for global land areas.
International Journal of Climatology 37, 4302–4315 (2017).


