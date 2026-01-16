import OrcFxAPI
import glob #to work through files in a folder
import xlsxwriter #to write to Excel
import inspect #to allow identification of Python file in Excel footer
import os #to allow Python to open MS Excel once the results are complete
import yaml #to allow access to config file for user inputs

# retrieves input data from a separate config file
with open('PipelayConfig.yml', 'r') as f:
    config = yaml.safe_load(f)
basecaseName = config['basecaseName']
maxTension = config['maxTension']
minTension = config['minTension']
minConstraintClearance = config['minConstraintClearance']
maxBollardPull = config['maxBollardPull']
increment = config['increment']
noOfCasesToAnalyse = config['noOfCasesToAnalyse']
codeCheckReference = config['codeCheckReference']

# identifies the basecase file and key objects within it
model = OrcFxAPI.Model(basecaseName)
line = model['Pipe']
vessel = model['Vessel1']
constraintBuoy = model['Constraint Buoy']
pipeLineType = model[line.LineType[0]] # assume that the first section line type is the pipe line type

# code check yield values
overbendLimit = pipeLineType.APIRP1111S*0.96/1000 #in MPa, derived automatically from the OrcaFlex model
sagbendLimit = pipeLineType.APIRP1111S*0.72/1000 #in MPa, derived automatically from the OrcaFlex model

# advises the user via the command line that the process has started
print ('\nPYTHON PIPELAY TABLE BUILDER\n')
print ('\n============================\n')
print ('\nAnalysing basecase and searching for limits...\n')

# function 'CaseIsAcceptable' defines pass/fail and is used in the subsequent code
def CaseIsAcceptable(): # checks results against the allowable limits
    model.CalculateStatics()
    codeCheck = (line.RangeGraph('API RP 1111 Max Combined', OrcFxAPI.pnStaticState)).Mean
    topTension = line.StaticResult('Effective Tension', OrcFxAPI.oeEndA)
    bottomTension = line.StaticResult('Effective Tension', OrcFxAPI.oeTouchdown)
    lineTension = (line.RangeGraph('Effective Tension', OrcFxAPI.pnStaticState)).Mean
    minLineTension = min(lineTension)
    bollardPull = line.StaticResult('End Lx-Force', OrcFxAPI.oeEndA)
    constraintClearance = constraintBuoy.StaticResult('Support Contact Clearance', OrcFxAPI.oeSupport(SupportIndex = 1))
    declinationResultRG = (line.RangeGraph('Declination', OrcFxAPI.pnStaticState)).Mean
    stressResultRG = (line.RangeGraph('Max von Mises Stress', OrcFxAPI.pnStaticState)).Mean
    stressMaxOB = 0.0
    stressMaxSB = 0.0
    dec_prev = 0
    # using declination to establish whether the pipe is in sagbend or overbend
    for dec, stress in zip(declinationResultRG, stressResultRG):
        if dec > dec_prev: # declination is increasing so overbend
            if stress > stressMaxOB:
                stressMaxOB = stress # new maximum found for overbend
        else: # declination is decreasing so sagbend, declination is not increasing so sagbend
            if stress > stressMaxSB:
                stressMaxSB = stress # new maximum found for sagbend
        dec_prev = dec # updates the previous declination value
    # using acceptance criteria to classify 'pass' or 'fail'
    return (topTension <= maxTension
        and minLineTension >= minTension
        and stressMaxOB/1000 <= overbendLimit
        and stressMaxSB/1000 <= sagbendLimit
        and constraintClearance > minConstraintClearance
        and max(codeCheck) <= 1.0
        and bollardPull <= maxBollardPull)

# finds the max and min limiting cases
nearEndBX = farEndBX = line.EndBX
nearLimitReached = farLimitReached = False
while not nearLimitReached and not farLimitReached:
    while CaseIsAcceptable():
        line.EndBX -= increment
    farLimitReached = True
    farEndBX = line.EndBX
    line.EndBX = nearEndBX # resets back to initial position ready for next case
    while CaseIsAcceptable():
        line.EndBX += increment
    nearLimitReached = True
    nearEndBX = line.EndBX

# reports progress to the command line
print ('Limits found...\n')

# uses near and far limits to set up load cases (quantity defined at the start by the user and including the 2 limiting cases)
caseIncrement = abs(farEndBX - nearEndBX)/(noOfCasesToAnalyse-1)
for casenumber in range(noOfCasesToAnalyse):
    filename = 'Case%.3d.dat' % (casenumber + 1)
    line.EndBX = nearEndBX - casenumber*caseIncrement
    model.SaveData(filename)

# creates a new Excel file with specified worksheet and global settings
excelFile = 'Python Pipelay Results.xlsx'
workbook = xlsxwriter.Workbook(excelFile)
worksheet = workbook.add_worksheet('Results')
worksheet.set_landscape()
worksheet.set_paper(9)
worksheet.center_horizontally()
worksheet.fit_to_pages(1, 1)
worksheet.set_zoom(70)
row = 25 # defines the place in the worksheet where the OrcaFlex results input will start
col = 0

# defines the worksheet formatting (font name, size, cell colour etc.)
format_title = workbook.add_format()
format_title.set_bold('bold')
format_title.set_align('center')
format_title.set_align('vcenter')
format_title.set_bg_color('#F2F2F2')
format_title.set_font_size(18)
format_title.set_font_name('Arial')
format_main_data = workbook.add_format()
format_main_data.set_align('center')
format_main_data.set_align('vcenter')
format_main_data.set_font_size(10)
format_main_data.set_font_name('Arial')
format_table_headers = workbook.add_format()
format_table_headers.set_bold('bold')
format_table_headers.set_align('center')
format_table_headers.set_align('vcenter')
format_table_headers.set_text_wrap('text_wrap')
format_table_headers.set_bg_color('#F2F2F2')
format_table_headers.set_border()
format_table_headers.set_font_size(10)
format_table_headers.set_font_name('Arial')
format_table_subheaders = workbook.add_format()
format_table_subheaders.set_align('left')
format_table_subheaders.set_align('vcenter')
format_table_subheaders.set_text_wrap('text_wrap')
format_table_subheaders.set_font_size(10)
format_table_subheaders.set_font_name('Arial')
format_comments = workbook.add_format()
format_comments.set_align('top')
format_comments.set_align('left')
format_comments.set_text_wrap('text_wrap')
format_comments.set_bg_color('#F2F2F2')
format_comments.set_border()
format_comments.set_font_size(12)
format_comments.set_font_name('Arial')
format_comments_header = workbook.add_format()
format_comments_header.set_bold('bold')
format_comments_header.set_align('left')
format_comments_header.set_align('vcenter')
format_comments_header.set_text_wrap('text_wrap')
format_comments_header.set_bg_color('#F2F2F2')
format_comments_header.set_font_size(12)
format_comments_header.set_font_name('Arial')
format_num = workbook.add_format()
format_num.set_border()
format_num.set_align('center')
format_num.set_align('vcenter')
format_num.set_bold('bold')
format_num.set_bg_color('#F2F2F2')
format_num.set_num_format('0')
format_num.set_font_size(10)
format_num.set_font_name('Arial')
format_pass = workbook.add_format()
format_pass.set_bg_color('#92D050')
format_pass.set_align('center')
format_pass.set_align('vcenter')
format_pass.set_font_size(10)
format_pass.set_font_name('Arial')
format_fail = workbook.add_format()
format_fail.set_bg_color('red')
format_fail.set_align('center')
format_fail.set_align('vcenter')
format_fail.set_font_size(10)
format_fail.set_font_name('Arial')
format_no_blanks = workbook.add_format()
format_no_blanks.set_border(1)
format_blanks = workbook.add_format()
format_blanks.set_bg_color('white')
worksheet.conditional_format('A1:ZZ999', {'type': 'no_blanks',
                                          'format': format_no_blanks})

# inserts the title box and basecase data into the worksheet
worksheet.set_row(1, 32)
worksheet.merge_range('A2:U2', 'OrcaFlex Pipelay Table (Python)', format_title)
worksheet.merge_range('A4:B4', 'Basecase Data', format_table_headers)
worksheet.write('A5', 'Vessel:', format_table_subheaders)
worksheet.write('B5', vessel.name, format_table_subheaders)
worksheet.write('A6', 'Pipe:', format_table_subheaders)
worksheet.write('B6', line.linetype[0], format_table_subheaders)
worksheet.write('A7', 'Ramp Angle (deg):', format_table_subheaders)
worksheet.write('B7', (line.EndADeclination-90), format_table_subheaders)
worksheet.write('A8', 'Water Depth (m):', format_table_subheaders)
worksheet.write('B8', model.environment.WaterDepth, format_table_subheaders)
worksheet.write('A9', 'Seabed Slope:', format_table_subheaders)
worksheet.write('B9', model.environment.SeabedSlope, format_table_subheaders)
model = OrcFxAPI.Model(basecaseName) #model opened to calculate current speed at surface and seabed
model.CalculateStatics() #run statics to calculate current speed at surface and seabed
worksheet.write('A10', 'Current Speed (surface, m/s):', format_table_subheaders)
worksheet.write('B10', model.environment.StaticResult('Current Speed', OrcFxAPI.oeEnvironment(0.0, 0.0, 0.0)), format_table_subheaders)
worksheet.write('A11', 'Current Speed (seabed, m/s):', format_table_subheaders)
SeabedOriginZ = model.environment.SeabedOriginZ #defines seabed depth
worksheet.write('B11', model.environment.StaticResult('Current Speed', OrcFxAPI.oeEnvironment(0.0, 0.0, SeabedOriginZ)), format_table_subheaders)
worksheet.write('A12', 'Current Direction (deg):', format_table_subheaders)
worksheet.write('B12', model.environment.RefCurrentDirection, format_table_subheaders)
worksheet.write('A13', 'Code Check:', format_table_subheaders)
worksheet.write('B13', codeCheckReference, format_table_subheaders)

# writes the comments table in the worksheet
worksheet.merge_range('D4:U4', 'Comments', format_comments_header)
worksheet.merge_range('D5:U20',
    '\n'
    'This results sheet is intended to replicate a typical OrcaLay results sheet as a way of demonstrating that Python and OrcaFlex can be used as a viable alternative.\n'
    'It is an example of the type of data that can be automatically presented through the use of Python.\n'
    'The spreadsheet is built and formatted entirely in Python with no user input required in Excel.\n'
    'Cells and rows are highlighted automatically to identify pass/fail.\n'
    'Cells highlighted in red are those that do not meet the necessary acceptance criteria.\n'
    'Rows highlighted in green represent the \'acceptable\' cases that remain within all acceptance criteria.\n'
    'If the reaction force for all supports in a load case is zero, this infers that the line is completely in sagbend, and this will consequently place \'N/A\' in the corresponding overbend cell.\n'
    '\n'
    'User needs will of course vary, and this sheet is only intended as an example. The Python code can be modified as required by the user.'
    , format_comments)

# inserts the acceptance criteria into the worksheet
worksheet.merge_range('A15:B15', 'Acceptance Criteria', format_table_headers)
worksheet.write('A16', 'Max Tension (kN):', format_table_subheaders)
worksheet.write('B16', maxTension, format_table_subheaders)
worksheet.write('A17', 'Min Tension (kN):', format_table_subheaders)
worksheet.write('B17', minTension, format_table_subheaders)
worksheet.write('A18', 'Max Bollard Pull (kN):', format_table_subheaders)
worksheet.write('B18', maxBollardPull, format_table_subheaders)
worksheet.write('A19', 'Uncontrolled Stress Limit (MPa, 72% of yield):', format_table_subheaders)
worksheet.write('B19', sagbendLimit, format_table_subheaders)
worksheet.write('A20', 'Controlled Stress Limit (MPa, 96% of yield):', format_table_subheaders)
worksheet.write('B20', overbendLimit, format_table_subheaders)

# inserts the main table headers and applies formatting
worksheet.set_row(22, 28)
worksheet.set_column(0, 0, 25, format_main_data)
worksheet.set_column(1, 1, 14, format_main_data)
worksheet.merge_range('A23:A25', 'Load Case', format_table_headers)
worksheet.merge_range('B23:B24', 'Anchor X Position', format_table_headers)
worksheet.write('B25', 'm', format_table_headers)
worksheet.set_column(2, 6, 6, format_main_data)
worksheet.merge_range('C23:G23', 'Support Reaction Force', format_table_headers)
worksheet.write_row('C24:G24', '12345', format_num)
worksheet.write('C25', 'kN', format_table_headers)
worksheet.write('D25', '=C25', format_table_headers)
worksheet.write('E25', '=C25', format_table_headers)
worksheet.write('F25', '=C25', format_table_headers)
worksheet.write('G25', '=C25', format_table_headers)
worksheet.set_column(7, 18, 12, format_main_data)
worksheet.merge_range('H23:H24', 'Max Support Lift Out', format_table_headers)
worksheet.write('H25', 'm', format_table_headers)
worksheet.merge_range('I23:J23', 'Tension', format_table_headers)
worksheet.write('I24', 'Top', format_table_headers)
worksheet.write('J24', 'Bottom', format_table_headers)
worksheet.write('I25', 'kN', format_table_headers)
worksheet.write('J25', 'kN', format_table_headers)
worksheet.merge_range('K23:K24', 'Minimum Tension', format_table_headers)
worksheet.write('K25', 'kN', format_table_headers)
worksheet.merge_range('L23:L24', 'Top Bend Moment', format_table_headers)
worksheet.write('L25', 'kN.m', format_table_headers)
worksheet.merge_range('M23:M24', 'Suspended Length', format_table_headers)
worksheet.write('M25', 'm', format_table_headers)
worksheet.merge_range('N23:N24', 'Horizontal Projection', format_table_headers)
worksheet.write('N25', 'm', format_table_headers)
worksheet.merge_range('O23:O24', 'Projection Growth', format_table_headers)
worksheet.write('O25', 'm', format_table_headers)
worksheet.merge_range('P23:Q23', 'Maximum Stress', format_table_headers)
worksheet.write('P24', 'Overbend', format_table_headers)
worksheet.write('Q24', 'Sagbend', format_table_headers)
worksheet.write('P25', 'MPa', format_table_headers)
worksheet.write('Q25', 'MPa', format_table_headers)
worksheet.merge_range('R23:R24', 'Departure Angle', format_table_headers)
worksheet.write('R25', 'deg', format_table_headers)
worksheet.merge_range('S23:S24', 'Constraint Clearance', format_table_headers)
worksheet.write('S25', 'm', format_table_headers)
worksheet.set_column(19, 20, 8.5, format_main_data)
worksheet.merge_range('T23:T25', 'Code Check', format_table_headers)
worksheet.merge_range('U23:U24', 'Bollard Pull', format_table_headers)
worksheet.write('U25', 'kN', format_table_headers)

# inserts a footer into the worksheet identifying data sources
worksheet.set_footer('&LOrcaFlex base file: ' + basecaseName + '\nPython code: ' + inspect.stack()[0][1] + '&RCreated: &D')

# reopens the previously saved dat files, runs statics and then generates the necessary results
for FileName in glob.glob('Case*.dat'):
    model.LoadData(FileName)
    model.CalculateStatics()
    line = model['Pipe']
    vessel = model['Vessel1']
    constraintBuoy = model['Constraint Buoy']
    lineTension = (line.RangeGraph('Effective Tension', OrcFxAPI.pnStaticState)).Mean
    srf1 = vessel.StaticResult('Support Reaction Force', OrcFxAPI.oeSupport(SupportIndex = 1))
    srf2 = vessel.StaticResult('Support Reaction Force', OrcFxAPI.oeSupport(SupportIndex = 2))
    srf3 = vessel.StaticResult('Support Reaction Force', OrcFxAPI.oeSupport(SupportIndex = 3))
    srf4 = vessel.StaticResult('Support Reaction Force', OrcFxAPI.oeSupport(SupportIndex = 4))
    srf5 = vessel.StaticResult('Support Reaction Force', OrcFxAPI.oeSupport(SupportIndex = 5))
    supportLiftOut = vessel.StaticResult('Max Support Lift Out', OrcFxAPI.oeSupport(SupportIndex = 0))
    topTension = line.StaticResult('Effective Tension', OrcFxAPI.oeEndA)
    bottomTension = line.StaticResult('Effective Tension', OrcFxAPI.oeTouchdown)
    minLineTension = min(lineTension)
    topBendMoment = line.StaticResult('Bend Moment', OrcFxAPI.oeEndA)
    suspLength = line.StaticResult('Arc Length', OrcFxAPI.oeTouchdown)
    tdX = line.StaticResult('X', OrcFxAPI.oeTouchdown)
    endAX = line.StaticResult('X', OrcFxAPI.oeEndA)
    horizProj = abs(tdX - endAX)

    # for max overbend and sagbend stress values:
    declinationResultRG = (line.RangeGraph('Declination', OrcFxAPI.pnStaticState)).Mean
    stressResultRG = (line.RangeGraph('Max von Mises Stress', OrcFxAPI.pnStaticState)).Mean
    stressMaxOB = 0.0
    stressMaxSB = 0.0
    dec_prev = 0
    # using declination to establish whether the pipe is in sagbend or overbend
    for dec, stress in zip(declinationResultRG, stressResultRG):
        if dec > dec_prev: # declination is increasing so overbend
            if stress > stressMaxOB:
                stressMaxOB = stress # new maximum found for overbend
        else: # declination is decreasing so sagbend, declination is not increasing so sagbend
            if stress > stressMaxSB:
                stressMaxSB = stress # new maximum found for sagbend
        dec_prev = dec # updates the previous declination value

    #for departure angle, find the point at which the line cuts the sea surface:
    zpos = line.RangeGraph('Z', OrcFxAPI.pnStaticState)
    for z, arcL in zip(zpos.Mean, zpos.X):
        if z == min(zpos.Mean, key = lambda x: abs(x)):
            depAngleAL = arcL
    depAngleDec = line.StaticResult('Declination', OrcFxAPI.oeArcLength(depAngleAL))-90
    constraintClearance = constraintBuoy.StaticResult('Support Contact Clearance', OrcFxAPI.oeSupport(SupportIndex = 1))
    codeCheck = (line.RangeGraph('API RP 1111 Max Combined', OrcFxAPI.pnStaticState)).Mean
    bollardPull = line.StaticResult('End Lx-Force', OrcFxAPI.oeEndA)
    # report progress to the command line
    print ('Processing results for', FileName)

    # writes the data for the currently open dat file to the worksheet
    worksheet.write(row, col, FileName)
    worksheet.write(row, col + 1, round(line.EndBX,3))
    worksheet.write(row, col + 2, round(srf1,3))
    worksheet.write(row, col + 3, round(srf2,3))
    worksheet.write(row, col + 4, round(srf3,3))
    worksheet.write(row, col + 5, round(srf4,3))
    worksheet.write(row, col + 6, round(srf5,3))
    worksheet.write(row, col + 7, round(supportLiftOut,3))
    worksheet.write(row, col + 8, round(topTension,3))
    worksheet.write(row, col + 9, round(bottomTension,3))
    worksheet.write(row, col + 10, round(minLineTension,3))
    worksheet.write(row, col + 11, round(topBendMoment,3))
    worksheet.write(row, col + 12, round(suspLength,2))
    worksheet.write(row, col + 13, round(horizProj,2))
    worksheet.write(row, col + 14, round(suspLength-horizProj,2))
    worksheet.write(row, col + 15, round(stressMaxOB/1000,2))
    worksheet.write(row, col + 16, round(stressMaxSB/1000,2))
    worksheet.write(row, col + 17, round(depAngleDec,2))
    worksheet.write(row, col + 18, round(constraintClearance,2))
    if max(codeCheck) <= 1.0:
        worksheet.write(row, col + 19, 'PASS')
    else:
        worksheet.write(row, col + 19, 'FAIL', format_fail) # formats the cell red if fail
    worksheet.write(row, col + 20, round(abs(bollardPull),2))

    # inserts 'N/A' for overbend stress where there is no support contact (implies completely in sagbend)
    if srf1 + srf2 + srf3 + srf4 + srf5 <= 0:
        worksheet.write(row, col + 15, 'N/A')

    # colours the current row green if all results are within limits
    if(topTension <= maxTension
        and minLineTension >= minTension
        and stressMaxOB/1000 <= overbendLimit
        and stressMaxSB/1000 <= sagbendLimit
        and constraintClearance > minConstraintClearance
        and max(codeCheck) <= 1.0
        and bollardPull <= maxBollardPull):
            worksheet.set_row(row, 15, format_pass)

    # applies conditional formatting to highlight failures
    worksheet.conditional_format(row, col + 8, row, col + 8, {'type': 'cell',
                                            'criteria': '>',
                                            'value': maxTension,
                                            'format': format_fail})
    worksheet.conditional_format(row, col + 9, row, col + 9, {'type': 'cell',
                                            'criteria': '>',
                                            'value': maxTension,
                                            'format': format_fail})
    worksheet.conditional_format(row, col + 10, row, col + 10, {'type': 'cell',
                                            'criteria': '<',
                                            'value': minTension,
                                            'format': format_fail})
    worksheet.conditional_format(row, col + 15, row, col + 15, {'type': 'cell',
                                            'criteria': 'between',
                                            'minimum': overbendLimit,
                                            'maximum': 9999999999999,
                                            'format': format_fail})
    worksheet.conditional_format(row, col + 16, row, col + 16, {'type': 'cell',
                                            'criteria': '>',
                                            'value': sagbendLimit,
                                            'format': format_fail})
    worksheet.conditional_format(row, col + 18, row, col + 18, {'type': 'cell',
                                            'criteria': '<=',
                                            'value': minConstraintClearance,
                                            'format': format_fail})
    worksheet.conditional_format(row, col + 20, row, col + 20, {'type': 'cell',
                                            'criteria': '>',
                                            'value': maxBollardPull,
                                            'format': format_fail})
    # starts a new row ready for the next dat file
    row += 1

# clears formatting on all blank cells
worksheet.conditional_format('A1:ZZ999', {'type': 'blanks',
                                          'format': format_blanks})
# closes the workbook once all data is written
workbook.close()
# reports progress to the command line
print ('\nProcessing Complete. Opening table...\n')
# opens the resultant spreadsheet
os.startfile(excelFile)