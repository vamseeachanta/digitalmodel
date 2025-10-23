Prepare YML input file for the following. Ask for any clarifications if needed before proceeding.

This input file will be used for: preparing orcaflex run files for a specific analysis.

Background:
- Orcaflex files have yaml keys which follow patterns.
- Changing a few key-value pairs changes the configuration and thus the runs.
- To easily manage the files, files are to be divided into various modules and are referenced in main files via a orcaflex system. 
- The general rules or processes is in: docs\modules\orcaflex\notes\pre_yml_file_management


- Also, a comprehensive example of output files is given in : tests\modules\orcaflex\analysis\moorings\pretension\source
- output files are named appropriately to reflect their content and module type. Easy to reference and refer to for maximum practical use by humans and AI.

- This work output files should also adhere to general rules of orcaflex files (format, etc.)

Input files: The entire .yml file with all keys in 1 file. 2 such files are given in : 
specs\modules\orcaflex\modular-input-file\input
output folder: specs\modules\orcaflex\modular-input-file\output
Output file: split modular files with references in main file.