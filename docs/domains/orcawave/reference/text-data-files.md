# User interface: Text data files

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

## YAML file format

Text data files use a standard file format called YAML and should be saved with the .yml file extension. The YAML file format was chosen because it is extremely easy to read and write.

YAML files are plain text files and so can be edited in any text editor. We have found Notepad++ to be a very effective editor for YAML files. Notepad++ has a tabbed interface for easy editing of multiple files and has code folding and syntax highlighting facilities that work well with YAML files.

|  |  |
| --- | --- |
| Notes: | YAML files must be saved with the UTF-8 character encoding. |
|  | The formatting (colour, bold, italic etc.) in the examples here has been added to aid readability, and is not a feature or requirement of text data files themselves. |

More details on the YAML format and Notepad++ can be obtained from the following web sites:

* <https://en.wikipedia.org/wiki/YAML> – YAML page on Wikipedia.
* <http://www.yaml.org/> – Official YAML homepage.
* <http://www.yaml.org/spec/> – Complete technical specification of YAML.
* <https://notepad-plus-plus.org/> – Notepad++.

## Elements of a text data file

The most basic element of a text data file is the **name/value** pair:

UnitsSystem: SI

The name (UnitsSystem) is written first, followed by a colon (:), then a SPACE, and then the value (SI). The [names](Automation,Obtainingdatanames.htm) used in text data files are the same as used to identify data items in automation processes.

Names and values in YAML files can contain spaces and other punctuation.

This example also contains a **list**. New items in a list are introduced by a dash (-) followed by a SPACE (items in a list can span more than a single line):

WaveHeading:
  - 0
  - 10
  - 20

Outline indentation is used to delimit blocks in a YAML file. This concept, known as **significant indentation**, is perhaps a little unusual as most data formats and programming languages use symbols to indicate the beginnings and ends of blocks. Indentation must be made with SPACE characters rather than TAB characters. It does not matter how many spaces are used so long as the indentation is consistent within each block. However, it is good practice to use the same indentation throughout a file. OrcaWave uses indentation of two spaces when it writes YAML files.

Lists are commonly used to represent **tables** of data:

FieldPointX, FieldPointY, FieldPointZ:
  - [45, 0, 0]
  - [15, 0, -5]
  - [-27.9, -21, 0]

The name FieldPointX, FieldPointY, FieldPointZ indicates three columns of data which are interpreted in that order. The comma (,) character is used as a separator. Note that you do not have to present the data in the same order as it appears in OrcaWave. The following example is equivalent to the previous example:

FieldPointY, FieldPointZ, FieldPointX:
  - [0, 0, 45]
  - [0, -5, 15]
  - [-21, 0, -27.9]

YAML files may contain **comments** which are introduced by a hash (#) character followed by a SPACE. All subsequent text on the same line is comment and is ignored when OrcaWave reads a text data file. Comments are not preserved by OrcaWave and any user comments in a manually edited YAML file opened with OrcaWave will be lost if the file is saved.

The IncludeFile identifier allows you to move data into a separate file which is then **included** in the main file. As well as making the main file shorter and more readable, using this approach can offer significant QA benefits. The included file can be a text file or a ZIP file containing a single text YAML file.

A text data file saved by OrcaWave contains some extra information:

﻿%YAML 1.1
# Type: Diffraction
# Program: OrcaWave 11.0a
# File: C:\Desktop\untitled.yml
# Created: 12:35 on 21/07/2019
# User: Stuart
# Machine: StuartsDesktop
---
# Units
UnitsSystem: SI
# Calculation & output
SolveType: Potential formulation only
LoadRAOCalculationMethod: Both
PreferredLoadRAOCalculationMethod: Haskind
...

The section between the `---` and `...` lines is the main body of the file and is known in YAML terminology as a *document*. Everything else is in fact optional and can be omitted. A YAML file can contain multiple documents, separated by `---` lines but OrcaWave has no special treatment for such multi-document files and all data are read into a single OrcaWave model.

The first line (`%YAML 1.1`) is known as the YAML directive and specifies which version of YAML the file adheres to. The YAML directive can be omitted. The rest of the header contains a number of comments detailing the version of OrcaWave which created the file, the file name etc. Again, these comments can be omitted.

## Ordering issues

The order in which the data appear in a text data file is very important. OrcaWave processes the file line by line in the order in which it appears in the file. Any **references** must be ordered so that the referenced object appears before any references to it.

The other ordering issue relates to **inactive data**. Data which are not currently available are known as inactive data. For example, data relating to [QTF calculation method](Data,QTFs.htm) are inactive when the solve type is [potential formulation only](Data,Calculationandoutput.htm#SolveType). Inactive data cannot be specified in a text data file. For example, you must set the solve type to [full QTF calculation](Data,Calculationandoutput.htm#SolveType)  **before** setting the QTF calculation method.

This principle applies in general – you should set as soon as possible all data which influences whether other data are active.

## Automation

Text data files can easily be modified and/or generated by computer programs/scripts. This means that the text data file format, combined with a text processing script language (e.g. [Python](http://www.python.org/), [Perl](http://www.perl.org/), [Ruby](http://www.ruby-lang.org/) etc.), can form a very effective automation tool. The [OrcaFlex spreadsheet](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Textdatafiles,Automatinggeneration.htm) provides a simple, yet effective, facility for automating the production of text data files. A common automation task is to make systematic variations to a base case; this can readily be achieved via the use of [variation models](Data,Model.htm#VariationModels). Another specialist automation feature is the [expression evaluator](#ExpressionEvaluator). Saving a text data file from OrcaWave and then editing it by hand in a text editor is a good way to create a base file for automation, or to discover data names and data structure for an object.

## Expression evaluator

OrcaWave can understand mathematical expressions in text data files. An expression is indicated by the presence of a prefixed equals sign.

WaveHeading:
  - 0
  - =2\*5
  - 20

In addition, you can define named variables. So the previous example could also be written as follows:

Variables:
  Angle: 5
WaveHeading:
  - 0
  - =2\*Angle
  - 20

The expression evaluator supports the following operators:

* The basic arithmetic operators: `+`  `-`  `*`  `/`
* The exponentiation operator: `^`
* The comparison operators: `<`  `<=`  `>`  `>=`  `<>`  `=`

The comparison operators evaluate to 1 if the condition is satisfied, otherwise to 0. These operators will invariably be used with the conditional function `if(X, Y, Z)`.

The pre-defined constant `pi` can be used to express the value $\pi$. In addition, a number of pre-defined functions are supported:

| Function | Description |
| `abs(X)` | The magnitude or absolute value of X |
| `sgn(X)` | The sign of X, evaluates to -1 if X<0, +1 if X>0, 0 if X=0 |
| `rand` | A random number between 0 and 1 |
| `if(X, Y, Z)` | The conditional operator, evaluates to Y if X≠0, Z if X=0 |
| `round(X)` | The nearest integer to X |
| `floor(X)` | The largest integer not greater than X |
| `ceil(X)` | The smallest integer not less than X |
| `pow(X, Y)` | XY, equivalent to the exponentiation operator X^Y |
| `sqrt(X)` | The square root of X |
| `exp(X)` | The exponential function, eX |
| `ln(X)` | The natural logarithm, loge |
| `log10(X)` | Logarithm to base 10, log10 |
| `deg(X)` | Converts from radians to degrees, evaluates to $\frac{180}{\pi}$ X |
| `rad(X)` | Converts from degrees to radians, evaluates to $\frac{\pi}{180}$ X |
| `sin(X)` | The sine of X |
| `cos(X)` | The cosine of X |
| `tan(X)` | The tangent of X |
| `asin(X)` | The inverse sine of X |
| `acos(X)` | The inverse cosine of X |
| `atan(X)` | The inverse tangent of X |
| `atan2(Y, X)` | The [two argument version of the inverse tangent](http://en.wikipedia.org/wiki/Atan2). |
| `sinh(X)` | The hyperbolic sine of X |
| `cosh(X)` | The hyperbolic cosine of X |
| `tanh(X)` | The hyperbolic tangent of X |
| `asinh(X)` | The inverse hyperbolic sine of X |
| `acosh(X)` | The inverse hyperbolic cosine of X |
| `atanh(X)` | The inverse hyperbolic tangent of X |
