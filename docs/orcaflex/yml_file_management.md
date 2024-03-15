## OrcaFlex | YML File Management

OrcaFlex software is a 3D software used for the analysis of offshore structures. The software can use yml files for data definitions and other inputs.

he usage and management of yml file(s) as input files for Orcaflex analysis an help in the following ways:

- The content can be defined in a separate file(s) and imported into the main file
- The content can be reused in multiple files
- The content can be easily modified and updated
- The content can be easily shared and reused with other users and across project(s)

## Summary

A summary of usage and management of is given below

- BaseFile and includefile options have their uses
- includefile option is comparitively more versatile than BaseFile option
- Being aware of various uses of these options helps in better management of yml files and easier and more enjoyable OrcaFlex modelling experience

| Method <br> Feature | includefile | BaseFile |
| --- | --- | --- |
| Importing yml file | Yes | Yes |
| Importing multiple yml files into single yml | Yes | No |
| Importing multiple yml files in project | Yes | Yes but chaining |
| Section imports | Multiple child files into 1 master file | Single child file into single master file |
| Child yml format knowledge | advanced | basic |

## YML file usage

There are 2 main ways to use yml files in OrcaFlex:

- includefile method
- BaseFile method

The definition and usage of these methods is given in following subsections.

## includefile

The different ways of using includefile method is given in this section.

- YML Level usage
- YML array usage

The includefile method is used to import a yml file into another yml file. The usage of this method summarized in schematic below and is given in the following sections.

### YML Level Usage, Single YML Files

The YML Level usage is summarized below:

- For a given key, a yml file can be prepared and imported into the main file
- An example usage is given below:

**MasterFile to run in OrcaFlex:** 01.yml

```yml
General:
  - includefile: __01a_dynamic_analysis_parameters.yml
```

**BaseFile 1 to be imported:** _dynamic_analysis_parameters.yml

```yml
# Dynamics
DynamicsSolutionMethod: Implicit time domain
ImplicitUseVariableTimeStep: Yes
ImplicitVariableMaxTimeStep: 0.25
LogPrecision: Double
TargetLogSampleInterval: 0.5
LogStartTime: ~
```

### YML Level Usage, Multiple YML Files

**MasterFile to run in OrcaFlex:** 01.yml

```yml
General:
  - includefile: _static_analysis_parameters.yml
  - includefile: _dynamic_analysis_parameters.yml
```

**BaseFile 1 to be imported:** _static_analysis_parameters.yml

```yml
StaticsMinDamping: 2
StaticsMaxDamping: 10
StaticsMaxIterations: 500
StaticsTolerance: 0.025
ImplicitConstantTimeStep: 0.05
```

**BaseFile 2 to be imported:** _dynamic_analysis_parameters.yml

```yml
# Dynamics
DynamicsSolutionMethod: Implicit time domain
ImplicitUseVariableTimeStep: Yes
ImplicitVariableMaxTimeStep: 0.25
LogPrecision: Double
TargetLogSampleInterval: 0.5
LogStartTime: ~
```

## BaseFile

#### BaseFile, Single Chaining

The typical BaseFile import to yml definition is given in this section. The below example demonstrates the import of a yml file into another yml file with below features:

- A yml level 1 key, "General" is defined in the BaseFile with dynamic analysis parameters
- yml level 1 key, "General" is again defined in MasterFile with  damping definition
- Running 01.yml below will contain both these definitions
- Single chain is explained in this section. For multiple chaining, refer to [multiple-chaining](#basefile-multiple-chaining)

**MasterFile to run in OrcaFlex:** 01.yml

```yml
BaseFile: 02a_global_direction.yml

General:
  StaticsMinDamping: 2
  StaticsMaxDamping: 10
```

**BaseFile to be imported into MasterFile:** _dynamic_analysis_parameters.yml

```yml
BaseFile: _static_analysis_parameters.yml
# Dynamics
DynamicsSolutionMethod: Implicit time domain
ImplicitUseVariableTimeStep: Yes
ImplicitVariableMaxTimeStep: 0.25
LogPrecision: Double
TargetLogSampleInterval: 0.5
LogStartTime: ~
```

#### BaseFile, Multiple Chaining

Multiple chaining is the process of importing multiple BaseFiles into a MasterFile. An example is given in this section and detailed below:

- BaseFile 1 (containing static analysis parameters) is imported to Basefile 2
- BaseFile 2 (containing dynamic analysis parameters and BaseFile1 content) is imported to MasterFile

**MasterFile to run in OrcaFlex:** 01.yml

```yml
BaseFile: 02a_global_direction.yml

General:
  StaticsMinDamping: 2
  StaticsMaxDamping: 10
```

**BaseFile 2 to be imported into MasterFile:** _dynamic_analysis_parameters.yml

```yml
BaseFile: _static_analysis_parameters.yml
# Dynamics
DynamicsSolutionMethod: Implicit time domain
ImplicitUseVariableTimeStep: Yes
ImplicitVariableMaxTimeStep: 0.25
LogPrecision: Double
TargetLogSampleInterval: 0.5
LogStartTime: ~
```

**BaseFile 1 to be imported into BaseFile 2:** _static_analysis_parameters.yml

```yml
StaticsMinDamping: 2
StaticsMaxDamping: 10
StaticsMaxIterations: 500
StaticsTolerance: 0.025
ImplicitConstantTimeStep: 0.05
```

### References

<https://www.orcina.com/webhelp/OrcaFlex/Content/html/Textdatafiles.htm>

<https://www.orcina.com/webhelp/OrcaFlex/Content/html/Textdatafiles,Examplesofsettingdata.htm>
