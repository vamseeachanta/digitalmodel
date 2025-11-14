Utilizing the project input file, projects\modules\calm\baltic_039m\TEST_OPERABILITY\project_config.yml to update the following: 
a/ Water depth should be updated to 10 m
b/  the moorings should be modified to reflect a 10 m water depth scenario.
c/ add 5 mT of clump weight to each mooring line.

Following this, the orcaflex files should be updated accordingly.


The files in below location are working and loading into orcaflex as expected.
projects\modules\calm\nse_100m\TEST_OPERABILITY\orcaflex\base_files

review the files in below location and let us review the changes made to the orcaflex files.
projects\modules\calm\baltic_039m\TEST_OPERABILITY\orcaflex\base_files

## Changes  to Orcaflex files


Update env files using following:
projects\modules\calm\baltic_039m\TEST_OPERABILITY\orcaflex\base_files\env
Operating Conditions:
 Seas (from bow)	2.5 m
 Winds	30 knots
 Current	1.5 knots


Define the below vessel, use ocimf wind and current coefficients. 

    Handy Size Tanker (based on M/V Byzantion)
        Length (LOA) (m)	182.60	
        Breadth (m)	 	  27.30
        Depth (m)		  16.70
        Max Draft (m)		  11.70	
        DWT (tonnes)		35,589
        Capacity (bbl)	   260,440		
