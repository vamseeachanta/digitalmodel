Is more work than needed. But it is the repeatability and reuse that will make engineering work easier (and enjoyable).

Here is a stab at it:

- Existing document to ASCII format conversion. (This is the machine readable version of the original document)
  - Convert FAT_DOC.docx to ascii format to suit the AI agents. (Word, excel, PDF are proprietary rigid documents with unnecessary stuff for machines)

  .temp\wlng_fatigue_methodology\fatigue_methodology.docx
      -> .temp\wlng_fatigue_methodology\fatigue_methodology.md

  - Use AI agents to convert the ascii format to a workflow document. (This is the human readable version of the ascii format)
  - Used an MCP for it (curated this yesterday as I am using it for first time)

- Create spec from the existing document.

```markdown
  /create-spec for a custom analysis using the file:                         ?
?   .temp\wlng_fatigue_methodology\fatigue_methodology.md 
```

  - Output:
  - used "agent-os" framework which will orchestrate tasks etc. for the program.
  - created a new spec file at specs\modules\fatigue\wlng_fatigue_methodology.md

- Course correct the spec using below prompt

```markdown
revise  the spec specs\modules\fatigue-analysis\strut-foundation-rainflow\spec.md, the raw data is timetrace data for reference seastates                   ?
   (specs\modules\fatigue-analysis\strut-foundation-rainflow\reference_seastate_timetraces.csv) and each row in this csv represents a metadata for a           ?
   timetrace for that reference seatate. These timetraces will be provided in .csv format in next steps.                                                       ?
   THese timetraces will be directly load scaled factor and then the rainflow counting followed by fatigue calculation.
```

  - Output:
  - updated the spec file at specs\modules\fatigue-analysis\strut-foundation-rainflow\spec.md
  
- Create a prompt for user to provide the input files and directories.
  ```markdown
  Get sample timetraces for reference seastates and place them in spec directory for use.
  The entire timetrace files are located in:
  D:\1522\ctr9\fatigue_wsp_method\07c_fatigue

  ```
  - Output: specs\modules\fatigue-analysis\strut-foundation-rainflow\sample_timetraces

- Scale factors
  -  are the scaling factors calculation available
  - the calculation looks good. create supplementary csv for easy verification 
  - rerun the DirectLoadScaler and then always dynamically update the scaling factors csv file

-  use the scale factors to sum up the effective tension of the strut timetrace from wind and wave. The output is the new timetrace in output csv file