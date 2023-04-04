# Introduction

To perform general tasks for any software/tools related to digital models

# Summary


# Orcaflex

- Test if valid license available to use? If possible, find how many licenses are available to use. 
    - Perform this task when license bottle necks are observed
- BaseFile understandings: 
    - Can be either .dat or .yml
    - Chained BaseFiles will work (i.e. file1.yml -> file2.yml -> file3.yml)
- Compare 2 yml files?
    - Utilize DeepDiff package for basic quantities?
    - For large array inputs, have to utilize other modules such as below
        - RAOs comparisons
        - etc.?
- Good scripts to run during multithread simulation/postprocessing analysis:
    - No.of processors = N-n 
    - At end, send an email to Teams channel with folder path, failure report etc., Potentially email by folder name?
