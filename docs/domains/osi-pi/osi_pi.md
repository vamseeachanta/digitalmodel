# Introduction

For OSISoft PI fundamentals, see https://github.com/vamseeachanta/energy/blob/fd79c12d790a7466745175a835b2b904cfad6ce0/tools/osi_pi.md 

## Summary

Using AF SDK libraries, the code is in the form of any typical python library with objects, attributes etc. See Example Codes below.

The typical way of understanding the output from the written code is to verify against the values in PI System Explorer, a graphical interface.

## Example Codes

- Importing necessary custom libraries
- The osisoft_client_files contain the entire libraries (XML and other formats) required for programming development for both .NET and Python
- These osisoft_client_files are version dependent and need to sync with the PI AF network supported across the domain

Code
<pre>
import sys
import clr

parameters = {'osisoft_client_files': r'lib\PI_AF_Client_2016_SP2_v2.8.2.7626', 'osisoft_afsdk': 'OSIsoft.AFSDK'}

sys.path.append(parameters['osisoft_client_files'])
clr.AddReference(parameters['osisoft_afsdk'])

from OSIsoft.AF import *
from OSIsoft.AF.PI import *
from OSIsoft.AF.Asset import *
from OSIsoft.AF.Data import *
from OSIsoft.AF.Time import *
from OSIsoft.AF.UnitsOfMeasure import *
</pre>

Output:
<pre>
None
</pre>

- Documenting and understand servers with access

Code
<pre>
print("Welcome to PIthon!!")

# PI Data Archive
piServers = PIServers()
piServer = piServers.DefaultPIServer

# Print Server with access permission. 
for server in piServers:
    print(server)

print("The default server is {}".format(piServer))
</pre>

Output:
<pre>
Welcome to PIthon!!
osipiserver1
osipistaging
osipiproduction.company.com

The default server is osipiserver1
</pre>

- Reading current value

Code
<pre>
pt = PIPoint.FindPIPoint(piServer, "sinusoid")
name = pt.Name.lower()

# CurrentValue
print('\nShowing PI Tag CurrentValue from {0}'.format(name))
current_value = pt.CurrentValue()
print('{0}\'s Current Value: {1}'.format(name, current_value.Value))
</pre>

Output:
<pre>
Showing PI Tag CurrentValue from sinusoid

sinusoid's Current Value: 22.92718
</pre>

- Reading recorded values (Historical time series values)

Code
<pre>
#recordedvalues
pt = PIPoint.FindPIPoint(piServer, "sinusoid")
timerange = AFTimeRange("*-3h", "*")
recorded = pt.RecordedValues(timerange, AFBoundaryType.Inside, "", False)
print('\nShowing PI Tag RecordedValues from {0}'.format(name))
for event in recorded:
    print('{0} value: {1}'.format(event.Timestamp.LocalTime, event.Value))
</pre>

Output:
<pre>
Showing PI Tag RecordedValues from sinusoid

3/8/2021 4:41:04 PM value: 81.78175
3/8/2021 7:05:34 PM value: 22.92718
</pre>

- Writing values (historical or future)

Code
<pre>
#writeValue
writept = PIPoint.FindPIPoint(piServer, "CDT158")
writeptname = writept.Name.lower()
val = AFValue()
val.Value = 20000
#val.Timestamp = AFTime("t+9h")

print('\nWrite value to {0} value: {1}'.format(writeptname, val.Value))
writept.UpdateValue(val, AFUpdateOption.Replace, AFBufferOption.BufferIfPossible)
</pre>

Output: No write permissions
<pre>
Write value to cdt158 value: 20000
Traceback (most recent call last):
  File "test_PIServer.py", line 50, in <module>
    writept.UpdateValue(val, AFUpdateOption.Replace, AFBufferOption.BufferIfPossible)
OSIsoft.AF.PI.PIException: [-10401] No Write Access - Secure Object
   at OSIsoft.AF.PI.PIException.ConvertAndThrowException(PIServer piServer, Exception ex, String message)
  at OSIsoft.AF.PI.PIPoint.UpdateValueInternalBufferIfPossible(Events mdaEvents, AFUpdateOption option)
   at OSIsoft.AF.PI.PIPoint.UpdateValue(AFValue value, AFUpdateOption option, AFBufferOption bufferOption)
</pre>
