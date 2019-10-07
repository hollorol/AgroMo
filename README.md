# AgroMo project

## Introduction

AgroMo is an Integrated Assessment and Modelling software that integrates 4M (Fodor et al. 2014) a CERES based crop model, the BiomeBGC-MuSo (Hidy et al. 2016) biogeochemical and a simple agro-economical model in order to support decision makers at multiple scales.

## AgroMo User Interface

A graphical user interface (GUI) has been developed for providing easy and user friendly access to the functions of the AgroMo system. When lunching the AgroMo App the user meets the AgroMo Base form first.

### AgroMo Base

![alt text](base_gui.png "AgroMo Base")

- [x] You may choose the main directory where your data files are located (in a fixed subdirectory system) by clicking the [CHOOSE] button.
- [x] You may go to the site specific component by clicking the [SITE] button.
- [x] You may create plots of the simulation results after clicking the [PLOT] button.
- [x] You may create, open, edit and save your input files with the [INPUT FILE MANAGER] after clicking the corresponding button.


### AgroMo Site

![alt text](site_gui.png "AgroMo Site")
- [x] The dropdown menus display the content, files only with the prescribed extension, of the corresponding directories within the input folder:
  - INI file (extension: .ini): .\input\initialization\
  - WEATHER file (extension: .wth): .\input\weather\site\
  - SOIL file (extension: .soi): .\input\soil\site\
  - MANAGEMENT file (extension: .mgm): .\input\management\
     - planting management option file (extension: .plt): .\input\management\planting\
     - harvest management option file (extension: .hrv): .\input\management\harvest\
     - fertilization management option file (extension: .frt): .\input\management\fertilization\
     - irrigation management option file (extension: .irr): .\input\management\irrigation\
     - cultivation management option file (extension: .cul): .\input\management\cultivation\
     - grazing management option file (extension: .grz): .\input\management\grazing\
     - mowing management option file (extension: .mow): .\input\management\mowing\
     - thinning management option file (extension: .thn): .\input\management\thinning\
  - PLANT specific input files (extension: .pla; folder: .\input\plant\) are referred to in the planting management option files
- [x] When selecting an INI file the WEATHER, SOIL and MANAGEMENT files that are referred to in the INI file are automatically selected from the corresponding dropdown menus, but those could be changed freely.
- [x] In case new INI, WEATHER, SOIL or MANAGEMENT files are placed to the corresponding folders of the file system, click the green [REFRESH] button to see the files in the corresponding dropdown menus.
- [x] When selecting a MANAGEMENT file the management option files that are referred to in the MANAGEMENT file are automatically selected from the corresponding dropdown menus, but those could be changed freely.
- [x] The most important parameters of the management options could be changed without accessing the management option file by adjusting the parameter values using the corresponding textboxes within the 'shift in...' section. The values can be set by typing or using the mouse scroll.
- [x] OUTPUT id could be entered in the corresponding textbox. Output data are stored in a SQLite datatable named after the OUTPUT id.
- [x] After selecting the desired input files and set up the changes in the 'shift in...' section the simulation can be started by clicking the [RUN] button.
- [x] An activity indicator is on while the simulation is running. 
- [x] The simulation results can be presented on different type of graphs after clicking the [PLOT] button.

### AgroMo Plot

![alt text](plotgui.png "AgroMo Plot")
- [ ] Select a maximum of 5 simulation results from the left side list by clicking the items. Clicking again deselects the item.
- [ ] Observed data could be added to the plots by selecting the EXPERIMENT and TREATMENT IDs from the corrresponding dropdown lists. NOTE, that data in the observed data tables should be structured according to the prescribed rules. Observed (experimental) data should be stored in the '.\observation\' folder in SQLite data tables: one table for one experiment. Observed data available in multiple repetitions could be presented on the plot in two ways: 1) each repetition as separate data point, or 2) one single data point representing the mean of the repetitions. Leaving the EXPERIMENT ID dropdown list empty results in presenting simulation results only on the plot(s).
- [ ] Select variable(s) from the right side list for presenting them on the plot(s).
- [ ] For each selected variable select:
  - a |time step| for which the daily data are to be aggregated into one single value (e.g. for presenting annual values the 'year' option should be selected).
  - a |function| that defines the aggregation (e.g. for presenting the final yield for each year the 'max' option should be selected)
  - a |plot type| 
- [ ] Options in the |time step|, |function| and |plot type| columns can be selected via circular menus operated by clicking/scrolling in the corresponding cells.
- [ ] Having all the desired options selected plots can be created by clicking the [PLOT] button.
- [ ] Data defining the plots can be exported into various file formats (e.g. xlsx, csv) by clicking the [EXPORT] button.

### AgroMo Grid

#### Queries:
NOTES: 1) in case no depth info is provided in a soil related query, by default it referes to the top soil (0-30 cm depth); 2) time slice dependent queries with no indication of the time period, by default refere to the baseline period (1981-2010); 
- sandy soils | homok talajok
- soils with SOM greater than 2% | >2% szervesanyag-tartalmű talajok
- places 200+ m above sea level | a tengerszint felett 200 m-nél magasabban fekvő területek
- arable lands | szántó területek
- places with average temperature greater then 10 °C | 10 °C-nél magasabb átlaghőmérsékletű területek
- average/minimum/maximum/10th/90th percentile yield | a termés átlaga/minimuma/maximuma/10-ik/90-ik percentilise
- 0-3 cm soil layer mean temperature in May in the 2071-2100 period | 0-3 cm talajréteg májusi átlaghőmérséklete a 2071-2100 időszakban


  
