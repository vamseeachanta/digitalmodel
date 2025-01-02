## Introduction

The Analysis good practices are given in this section

### Summary

- File management
- Running Files
- Visualization
- Adding installation steps

These topics are discussed in detail in the following subsections

### File Management

- Commmon files (if referred more than once) are to be stored in the `common` folder
- This common folder is referred to by one of following methods:
  - BaseFile reference
  - IncludeFile reference

### Running Files

The following guidelines will help file management and searching for appropriate data easy.

- The last file to get the results should always contain only 1 BaseFile line (and no other data or configuration tweaks)

- Installation steps: The vessel movement and addiing structure length etc should be done in a unique file(s)
  - Generate this data using a spreadsheet.
- Any additional settings to make it converge (Line type, removal of Bend stiffners) should be done in a unique file(s) so it is easy to edit or update the file with clear data

#### Visualizations

**Single Model Views**
Multiple views can be added to the visualiation settings to create additional view files.
These can help for reporting or QA of the files.

**Combined Model Views**
Currently the .dat or .sim file is created by a proprietary program (DigitalModel does not have the capability). This should be run manually. Views from the combined model can be created using the above.

### Results

QA

- Use static table
- Use views

OrcaFlex Experimentation is best done in Python
OrcaFlex Batchfiles are best run on UI right now (Parallel processing need to be understood best).
