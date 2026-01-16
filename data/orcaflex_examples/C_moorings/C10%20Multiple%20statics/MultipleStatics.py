# -*- coding: utf-8 -*-

from __future__ import print_function
import argparse
import math
import os.path
import sys
import copy
import yaml
import numpy
import numpy.linalg
import xlsxwriter
import OrcFxAPI

class DataError(Exception):
    pass

def dataError(msg):
    raise DataError(msg)

def getSpec(specFileName):
    with open(specFileName, 'r') as specFile:
        spec = yaml.safe_load(specFile)
    return OrcFxAPI.objectFromDict(spec)

def getArgs():
    parser = argparse.ArgumentParser(description='OrcaFlex multiple static analysis.')
    parser.add_argument('spec', nargs='?', help='name of the specification file')
    parser.add_argument('-e', '--echo', dest='echo', action='store_true', help='echo specification')
    parser.set_defaults(echo=False)
    args = parser.parse_args()
    if args.spec is None:
        args.spec = os.path.splitext(os.path.basename(__file__))[0] + '.yml'
    return args

def getCase(azimuth, offset):
    return OrcFxAPI.objectFromDict({
        'azimuth': azimuth,
        'offset': offset
    })

def getCases(spec):
    if spec.AzimuthStep < 0.0:
        dataError('Invalid AzimuthStep {0}, must be positive'.format(spec.AzimuthStep))
    if spec.AzimuthStep > 0.0 and spec.AzimuthFrom >= spec.AzimuthTo:
        dataError('AzimuthFrom {0}, must be less than AzimuthTo'.format(spec.AzimuthFrom, AzimuthTo))
    if spec.OffsetStep < 0.0:
        dataError('Invalid OffsetStep {0}, must be positive'.format(spec.OffsetStep))
    if spec.OffsetStep > 0.0 and spec.OffsetFrom >= spec.OffsetTo:
        dataError('OffsetFrom {0}, must be less than OffsetTo'.format(spec.OffsetFrom, OffsetTo))

    result = []
    azimuth = spec.AzimuthFrom
    while True:
        offset = spec.OffsetFrom
        while True:
            result.append(getCase(azimuth, offset))
            if spec.OffsetStep == 0.0 or (offset > spec.OffsetTo - 1.0e-3*spec.OffsetStep):
                break
            offset += spec.OffsetStep
        if spec.AzimuthStep == 0.0 or (azimuth > spec.AzimuthTo - 1.0e-3*spec.AzimuthStep):
            break
        azimuth += spec.AzimuthStep
    return result

def setInitialPosition(vessel, case, initialX, initialY):
    X, Y = case.offset*math.cos(math.radians(case.azimuth)), case.offset*math.sin(math.radians(case.azimuth))
    vessel.InitialX, vessel.InitialY = initialX + X, initialY + Y

def isConnectedToVessel(line, vesselName):
    return line.EndAConnection == vesselName \
        or line.EndBConnection == vesselName \
        or vesselName in line.MidLineConnectionMaster

def getWorstTension(lines, vesselName):
    worstTension = -numpy.inf
    lineName = None
    for line in lines:
        thisTension = line.RangeGraph('Effective Tension').Mean.max()
        if thisTension > worstTension:
            lineName = line.Name
            worstTension = thisTension
    return worstTension, lineName

def getCaseResults(model, case, vessel, lines):
    if model.state != OrcFxAPI.ModelState.InStaticState:
        return OrcFxAPI.objectFromDict({
            'azimuth': case.azimuth,
            'offset': case.offset,
            'succeeded': False
        })

    connectionForce = vessel.StaticResult(('Connections GX-Force', 'Connections GY-Force', 'Connections GZ-Force'))
    connectionMomentZ = vessel.StaticResult('Connections GZ-Moment')
    worstTension, worstTensionLineName = getWorstTension(lines, vessel.Name)

    def azimuth(X, Y):
        result = numpy.arctan2(Y, X)
        if result < 0.0:
            result += 2.0*numpy.pi
        return result

    def restoringDirection(X, Y):
        if numpy.linalg.norm(numpy.array([X, Y])) < 1.0e-6:
            return 0.0
        else:
            return azimuth(X, Y)

    return OrcFxAPI.objectFromDict({
        'azimuth': case.azimuth,
        'offset': case.offset,
        'succeeded': True,
        'restoringForce': numpy.linalg.norm(connectionForce[0:2]),
        'restoringDirection': numpy.rad2deg(restoringDirection(connectionForce[0], connectionForce[1])),
        'verticalForce': -connectionForce[2],
        'yawMoment': connectionMomentZ,
        'worstTension': worstTension,
        'worstTensionLineName': worstTensionLineName
    })

def doEcho(spec):
    print('Model file name: {0}'.format(spec.Model))
    if hasattr(spec, 'VesselName'):
        print('Vessel name: {0}'.format(spec.VesselName))
    print('Azimuths (from, to, step): {0}, {1}, {2}'.format(spec.AzimuthFrom, spec.AzimuthTo, spec.AzimuthStep))
    print('Offsets (from, to, step): {0}, {1}, {2}'.format(spec.OffsetFrom, spec.OffsetTo, spec.OffsetStep))
    print()

def doCalculation(spec):
    cases = getCases(spec)
    model = OrcFxAPI.Model(spec.Model, threadCount=1)
    vessels = [obj for obj in model.objects if obj.type == OrcFxAPI.otVessel]
    if len(vessels) == 0:
        dataError('No vessels in model')
    if hasattr(spec, 'Vessel'):
        vessel = model[spec.Vessel]
    else:
        if len(vessels) > 2:
            dataError('No vessel name specified, but there is more than one vessel in the model')
        vessel = vessels[0]
    if vessel.DataNameValid('Connection') and vessel.Connection != 'Free':
        dataError('Specified vessel, {0}, is connected to {1}. The vessel connection data must be Free.'.format(vessel.name, vessel.Connection))
    if vessel.IncludedInStatics != 'None':
        dataError('Specified vessel, {0}, has {1} statics selected. The vessel must not be included in statics.'.format(vessel.name, vessel.IncludedInStatics))
    lines = [obj for obj in model.objects if obj.type == OrcFxAPI.otLine and isConnectedToVessel(obj, vessel.Name)]

    units = OrcFxAPI.objectFromDict({
        'length': model.general.LengthUnits,
        'force': model.general.ForceUnits,
        'moment': model.general.ForceUnits + '.' + model.general.LengthUnits
    })

    initialX, initialY = vessel.InitialX, vessel.InitialY
    zeroOffsetCaseResults = None
    azimuths = []
    results = {}
    for index, case in enumerate(cases):
        print('Case {0}/{1}: azimuth {2} offset {3}'.format(index + 1, len(cases), case.azimuth, case.offset), end='')
        isZeroOffset = case.offset == 0.0
        if isZeroOffset and zeroOffsetCaseResults is not None:
            caseResults = copy.copy(zeroOffsetCaseResults)
            caseResults.azimuth = case.azimuth
            print()
        else:
            setInitialPosition(vessel, case, initialX, initialY)
            try:
                model.CalculateStatics()
                print()
            except OrcFxAPI.DLLError as err:
                if err.status == OrcFxAPI.stStaticsFailed:
                    print(', statics failed')
                else:
                    raise
            caseResults = getCaseResults(model, case, vessel, lines)
            if isZeroOffset:
                zeroOffsetCaseResults = caseResults

        if case.azimuth in results:
            results[case.azimuth].append(caseResults)
        else:
            azimuths.append(case.azimuth)
            results[case.azimuth] = [caseResults]
    return units, vessel.Name, azimuths, results

def doOutput(spec, units, vesselName, azimuths, results):
    offsetCount = len(results[azimuths[0]])

    outputFileName = os.path.splitext(spec.Model)[0] + '.MultStat.xlsx'
    book = xlsxwriter.Workbook(outputFileName)
    titleFormat = book.add_format()
    titleFormat.set_bold()
    titleFormat.set_font_size(20)

    sheet = book.add_worksheet('Input')
    sheet.set_column(0, 0, 26)
    sheet.write(0, 0, 'Multiple statics input data', titleFormat)
    sheet.write_row(2, 0, ('Model file name', spec.Model))
    sheet.write_row(3, 0, ('Vessel name', vesselName))
    sheet.write_row(4, 0, ('Azimuths (from, to, step)', u'{0}°, {1}°, {2}°'.format(spec.AzimuthFrom, spec.AzimuthTo, spec.AzimuthStep)))
    sheet.write_row(5, 0, ('Offsets (from, to, step)', '{0}{3}, {1}{3}, {2}{3}'.format(spec.OffsetFrom, spec.OffsetTo, spec.OffsetStep, units.length)))

    header1Format = book.add_format()
    header1Format.set_bold()
    header1Format.set_align('right')
    header2Format = book.add_format()
    header2Format.set_bold()
    header2Format.set_align('right')
    header2Format.set_bottom(1)

    def addChart(title, col, location):
        chart = book.add_chart({
            'type': 'scatter',
            'subtype': 'straight'
        })
        chart.add_series({
            'name': title,
            'categories': u'={0}!$A$5:$A${1}'.format(azimuthText, 5 + offsetCount - 1),
            'values': u'={0}!${2}$5:${2}${1}'.format(azimuthText, 5 + offsetCount - 1, col),
            'line': {'width': 2}
        })
        chart.set_title ({
            'name': title
        })
        chart.set_x_axis({
            'name': 'Offset ({0})'.format(units.length),
            'min': 0,
            'max': spec.OffsetTo
        })
        chart.set_legend({
            'position': 'none'
        })
        chart.set_style(10)
        sheet.insert_chart(location, chart, {'x_offset': 5, 'y_offset': 5})

    for azimuth in azimuths:
        azimuthText = u'{0:.1f}°'.format(azimuth)
        sheet = book.add_worksheet(azimuthText)
        sheet.set_column(0, 6, 14)
        sheet.write(0, 0, 'Azimuth ' + azimuthText, titleFormat)
        sheet.write_row(2, 0, ('', 'Restoring', 'Restoring', 'Vertical', 'Yaw', 'Worst', 'Worst tension'), header1Format)
        sheet.write_row(3, 0, ('Offset ({0})'.format(units.length), 'force ({0})'.format(units.force), u'direction (°)',
            'force ({0})'.format(units.force), 'moment ({0})'.format(units.moment), 'tension ({0})'.format(units.force), 'line name'),
            header2Format)
        for index, case in enumerate(results[azimuth]):
            sheet.write(4 + index, 0, case.offset)
            if case.succeeded:
                sheet.write_row(4 + index, 1, (case.restoringForce, case.restoringDirection, case.verticalForce, case.yawMoment,
                    case.worstTension, case.worstTensionLineName))
            else:
                sheet.write(4 + index, 6, 'statics failed')
        addChart('Restoring force ({0})'.format(units.force), 'B', 'H3')
        addChart('Vertical force ({0})'.format(units.force), 'D', 'P3')
        addChart('Yaw moment ({0})'.format(units.moment), 'E', 'H18')
        addChart('Worst tension ({0})'.format(units.force), 'F', 'P18')

    book.close()
    print('\nResults saved: {0}'.format(outputFileName))

def main():
    args = getArgs()
    spec = getSpec(args.spec)
    if args.echo:
        doEcho(spec)
    units, vesselName, azimuths, results = doCalculation(spec)
    doOutput(spec, units, vesselName, azimuths, results)

if __name__ == '__main__':
    try:
        main()
    except DataError as err:
        print('ERROR')
        print(err)
        sys.exit(1)