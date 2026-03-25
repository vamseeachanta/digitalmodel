# Introduction

To help automate Orcaflex for a typical analysis

# Summary




## Understanding YAML

- YAML can be used to support data as "Single source of Truth principle" for typical analysis
    - YAML is a human readable data serialization language
    - YAML is a superset of popular formats such as JSON, XML
        - provides capability for interchangability or interoperability

## Some working points

- If we open a .yml file downstream. Any changes to BaseFile (i.e. previous/upstream ) .yml file(s) will be highlighted in red by OrcaFlex UI for easy reference on understanding changes
- Saving a .dat file as .yml 
    - if .dat file is generated as binary data file from the start, all data comes into .yml file
    - If .dat file is generated using .yml structure, only partial data will be saved to .yml file with a reference to the basefile.