# OrcaFlex Text Data File Format Specification

> Source: https://www.orcina.com/webhelp/OrcaFlex/Content/html/Textdatafiles.htm
> Extracted: 2026-04-10

## Overview

OrcaFlex models are defined using YAML-formatted text data files with a `.yml` extension. These files provide a human-readable, easily editable representation of model configurations.

## File Format Requirements

### Character Encoding
YAML files must be saved with UTF-8 character encoding.

### YAML Version
Files may include a YAML directive (`%YAML 1.1`) at the beginning (optional).

### Document Structure
Main content between `---` and `...` delimiters, defining a single OrcaFlex document.

## Basic Elements

### Name/Value Pairs
```yaml
UnitsSystem: SI
```
Names match those used in batch script files for data identification.

### Indentation
- Outline indentation delimits blocks
- **MUST use SPACE characters, NOT TAB characters**
- Two-space indentation is OrcaFlex's standard

```yaml
General:
  UnitsSystem: SI
  DynamicsSolutionMethod: Implicit
Environment:
  WaterDepth: 80
```

### Comments
```yaml
# This is a comment (not preserved by OrcaFlex on save)
```

## Lists and Tables

### List Syntax
```yaml
Lines:
  - Name: Line1
    TopEnd: End B
  - Name: Umbilical
    TopEnd: End A
```

### Table Representation (Comma-Separated Property Names)
```yaml
Lines:
  - Name: Line1
    LineType, Length, TargetSegmentLength:
      - [Line Type1, 60, 5]
      - [Line Type1, 40, 2]
      - [Line Type2, 120, 10]
```
Column order is interchangeable. Columns may be omitted for defaults.

## Grouped Data
```yaml
3DBuoys:
  - Name: 3D Buoy1
    InitialPosition: [0, 0, 10]
    DragArea: [100, 100, 30]
    Pen: [4, Solid, Yellow]
```
X,Y,Z components appear in that order when grouped.

## Include Files
```yaml
VesselTypes:
  - Name: FPSO
    IncludeFile: FPSO.yml
```
- Relative paths supported
- Targets can be text files or ZIP archives containing a single YAML file

## Ordering Requirements

### Reference Objects
Referenced objects must appear before references to them:
- VesselTypes before Vessels
- LineTypes before Lines
- Types before instances

### Active/Inactive Data
Settings controlling which data remain active must be specified first:
```yaml
General:
  DynamicsSolutionMethod: Explicit  # Set first
  InnerTimeStep: 0.01              # Then use explicit-specific data
```

## Valid Top-Level Sections

```
General, Environment, VesselTypes, Vessels, LineTypes, Lines,
6DBuoys, 3DBuoys, Shapes, Constraints, Links, Winches,
ClumpTypes, WingTypes, BrowserGroups, MorisonElementTypes,
SupportTypes, DragChainTypes, FlexJointTypes, AttachedBuoys,
LineContents, Variables, VariableData, FrictionCoefficients, Groups
```

## Object Declaration Patterns

### Flat Format (simple models)
```yaml
VesselTypes:
  - Name: FPSO
    Length: 300
```

### New: Declaration (include-based models)
```yaml
New: FPSO
FPSO:
  Name: FPSO
  Length: 300
```

## Expression Evaluator

Mathematical expressions prefixed with `=`:
```yaml
General:
  StageDuration:
    - =4+3
    - =5*(4+3)
```

### Variables
```yaml
Variables:
  BuildUp: 7
  Stage1: =5*BuildUp
```

### Supported Functions
`abs`, `sgn`, `rand`, `if`, `round`, `floor`, `ceil`, `pow`, `sqrt`,
`exp`, `ln`, `log10`, `deg`, `rad`, `sin`, `cos`, `tan`, `asin`,
`acos`, `atan`, `atan2`, `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, `atanh`

## File Header (Optional)
```yaml
%YAML 1.1
# Program: OrcaFlex 11.5e
# File: model.yml
# Created: 12:35 on 21/07/2025
---
```
