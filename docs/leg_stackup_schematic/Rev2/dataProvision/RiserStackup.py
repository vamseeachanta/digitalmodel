import os
import xlrd  
import numpy as np
import math

workbook = xlrd.open_workbook('C:/Users/AceEngineer/Dropbox/0119 Programming/007 Stack-Up/Rev2/testData/riserStackup.xlsx')  # Open Excel file 
sheet = workbook.sheet_by_name(u'Design Data')   # Read Sheet

youngsModulus = 2.12E+08 # young's Modulus of Riser
poissionsRatio = 0.3 # Possion's Ration of Riser
componentName = sheet.col_values(0)    # name of Component
noofJoints = sheet.col_values(1)   # no of joint required for making riser stack up
lengthOfComponent = sheet.col_values(2)  # lenght of single component
componentOD = sheet.col_values(3)  # outer dia. of component
componentThick = sheet.col_values(4)  # thickness of component
componentDryWeight = sheet.col_values(5)  # dry weight of component
componentWetWeight = sheet.col_values(6)  # wet weight of component

waterDepth = 2895.6  #Water depth of the model is 9500 ft
drillFloorElevation = 25.4 #83.3 ft elevation from water level
heavyComponents = 'Diverter', 'LMRP', 'BOP', 'Wellhead'
lightComponents = 'SLK_TOP', 'Pup Joint', 'SLK_BOT','Riser Adaptor'
barrelComponents = 'Inner Barrel', 'Outer Barrel', 'RJT_3','RJT_5','RJT_7'
def StackUpDiagram():
        global topHeight
        topHeight = 70
        print("def textdata(z,x,y,c):", file=stackup)
        print("        basicfont = pygame.font.SysFont('arial', 15)", file=stackup)
        print("        text = basicfont.render(z, True,c)", file= stackup)
        print("        textrect = text.get_rect()",file= stackup)
        print("        textrect.centerx =DISPLAY.get_rect().centerx=x", file= stackup)
        print("        textrect.centery = DISPLAY.get_rect().centery=y", file= stackup)
        print("        DISPLAY.blit(text, textrect)", file = stackup)
        print("        pygame.display.update()", file= stackup)
        
        print("def main():", file=stackup)
        print("        DISPLAY.fill(aceColors.white)", file=stackup)
        print("        pygame.display.set_caption('Riser Stack-Up')", file=stackup)
        print("        pygame.draw.rect(DISPLAY,aceColors.red,(120,"+str(topHeight)+",560,20))  # Elevation", file=stackup)
            
        
        stackUpLength = 0
        ComponentLengthArray = np.zeros(shape=(len(componentName),1)).ravel()
        fromMLArray = np.zeros(shape=(len(componentName),1)).ravel()
       
        for componentCount, (inputcomponentName, inputnoofJoints, inputlengthOfComponent) in enumerate(
                zip(componentName, noofJoints, lengthOfComponent)):
                
                #Length of components and element lengths
                caltotalLengthOfComponent = inputnoofJoints*inputlengthOfComponent
                print("caltotalLengthOfComponent : ",caltotalLengthOfComponent)
                ComponentLengthArray[componentCount] = str(caltotalLengthOfComponent)
                
                if inputcomponentName == 'Diverter':
                        Colour = 'aceColors.pink'
                elif inputcomponentName == 'Upper Flexile Joint':
                        Colour = 'aceColors.red'
                elif inputcomponentName == 'Inner Barrel':
                        Colour = 'aceColors.grey'
                elif inputcomponentName == 'Outer Barrel':
                        Colour = 'aceColors.lightcyan'
                elif inputcomponentName == 'Tensioner Ring':
                        Colour = 'aceColors.grey'
                elif inputcomponentName == 'Mean Sea Level':
                        Colour = 'aceColors.blue'
                elif inputcomponentName == 'Pup Joint':
                        Colour = 'aceColors.black'
                elif inputcomponentName == 'RJT_3':
                        Colour = 'aceColors.yellow'        
                elif inputcomponentName == 'SLK_TOP':
                        Colour = 'aceColors.mettalicwhite'
                elif inputcomponentName == 'RJT_3':
                        Colour = 'aceColors.yellow'
                elif inputcomponentName == 'FUV':
                        Colour = 'aceColors.mettalicwhite'        
                elif inputcomponentName == 'RJT_5':
                        Colour = 'aceColors.red'
                elif inputcomponentName == 'RJT_7':
                        Colour = 'aceColors.green'
                elif inputcomponentName == 'RJT_9':
                        Colour = 'aceColors.orange'
                elif inputcomponentName == 'RJT_10':
                        Colour = 'aceColors.violet'
                elif inputcomponentName == 'SLK_BOT':
                        Colour = 'aceColors.mettalicwhite'
                elif inputcomponentName == 'Lower Flexile Joint':
                        Colour = 'aceColors.red'
                elif inputcomponentName == 'LMRP':
                        Colour = 'aceColors.pink'
                elif inputcomponentName == 'BOP':
                        Colour = 'aceColors.black'
                elif inputcomponentName == 'Wellhead':
                        Colour = 'aceColors.blue'
                elif inputcomponentName == 'Conductor':
                        Colour = 'aceColors.darkgrey'
                elif inputcomponentName == 'MeanSeaLevel':
                        Colour = 'aceColors.lightblue'
                        
                pixel = 20*caltotalLengthOfComponent
                
                print("pixel: ",pixel)
                LenfromLeft = 355
                
                if caltotalLengthOfComponent < 10.00:
                        
                        pixel = 15*caltotalLengthOfComponent
                        print('if condition pixel :',pixel)
                else:
                        pixel =  caltotalLengthOfComponent/20
                        print('else condition pixel :',pixel)
                
                       
                if (inputcomponentName != 'Upper Flexile Joint' and inputcomponentName !='Lower Flexile Joint'):
                        for comp in heavyComponents:
                                if(comp in inputcomponentName):
                                        CompWidth = 70
                                        print("        pygame.draw.rect(DISPLAY,"+str(Colour)+",("+str(LenfromLeft)+","+str(topHeight+50)+","+str(CompWidth)+","+str(pixel)+"))    # "+str(inputcomponentName)+"", file=stackup)
                                        topHeight = topHeight+pixel
                        for comp in barrelComponents:
                                if(comp in inputcomponentName ):
                                        CompWidth =20
                                        if(comp=='Inner Barrel' or comp=='Outer Barrel'):
                                                print("        pygame.draw.rect(DISPLAY,"+str(Colour)+",("+str(LenfromLeft+25)+","+str(topHeight+7)+","+str(CompWidth)+","+str(pixel+90)+"))    # "+str(inputcomponentName)+"", file=stackup)
                                                topHeight = topHeight+pixel+50
                                        elif(comp=='RJT_3','RJT_5','RJT_7'):
                                                print("        pygame.draw.rect(DISPLAY,"+str(Colour)+",("+str(LenfromLeft+25)+","+str(topHeight+40)+","+str(CompWidth)+","+str(pixel+10)+"))    # "+str(inputcomponentName)+"", file=stackup)
                                                topHeight = topHeight+pixel
                        for comp in lightComponents:
                                if(comp in inputcomponentName ):
                                        CompWidth =10
                                        print("        pygame.draw.rect(DISPLAY,"+str(Colour)+",("+str(LenfromLeft+29)+","+str(topHeight+55)+","+str(CompWidth)+","+str(pixel+30)+"))    # "+str(inputcomponentName)+"", file=stackup)
                                        topHeight = topHeight+pixel+30  
                   
                        stackUpLength = caltotalLengthOfComponent+stackUpLength
                       
                elif(inputcomponentName == 'Upper Flexile Joint'):
                        topHeight = 170 
                        h =  topHeight+0
                        print("        pygame.draw.circle(DISPLAY,"+str(Colour)+",("+str(math.floor(LenfromLeft+35))+","+str(math.floor(h))+"),8,0)    # "+str(inputcomponentName)+"", file=stackup)
                elif(inputcomponentName == 'Lower Flexile Joint'):
                        topHeight = 650
                        print("        pygame.draw.circle(DISPLAY,"+str(Colour)+",("+str(math.floor(LenfromLeft+35))+","+str(math.floor(topHeight))+"),8,0)    # "+str(inputcomponentName)+"", file=stackup)
                             
                if inputcomponentName == 'Conductor':
                        RiserLengthML = stackUpLength-caltotalLengthOfComponent
                        fromMLArray[0] = stackUpLength-caltotalLengthOfComponent
                        topHeight = 70
                        print("        pygame.draw.rect(DISPLAY,"+str(Colour)+",("+str(LenfromLeft+29)+","+str(fromMLArray[0])+","+str(CompWidth)+","+str(pixel+30)+"))    # "+str(inputcomponentName)+"", file=stackup)
                                
        print("        pygame.draw.rect(DISPLAY,aceColors.lightblue,(120,"+str(topHeight+250)+",560,10))  #MeanSeaLevel", file=stackup)
        print("        pygame.draw.rect(DISPLAY,aceColors.grey,(325,"+str(topHeight+180)+",130,30)) # Tensioner Ring", file=stackup)
        print("        pygame.draw.line(DISPLAY,aceColors.black,(280,90),(340,250),5) # Right-side",file=stackup)
        print("        pygame.draw.line(DISPLAY,aceColors.black,(285,90),(345,250),5) # Right-side",file=stackup)
        print("        pygame.draw.line(DISPLAY,aceColors.black,(500,90),(430,250),5) # Left-Side",file=stackup)
        print("        pygame.draw.line(DISPLAY,aceColors.black,(505,90),(435,250),5) # Left-Side",file=stackup)
        print("        pygame.display.flip()", file=stackup)
        print('        pygame.image.save(DISPLAY, "C:/Users/AceEngineer/Dropbox/0119 Programming/007 Stack-Up/Rev2/testData/Riser Stack-up_3000ft.png")', file=stackup)
                        
                        
        print("stackUpLength : ",stackUpLength)
        print("RiserLengthML : ",RiserLengthML)

        re=list(ComponentLengthArray)
        re.reverse()
        print(re)
        re.pop(0)
        print(re)

        for rCounter, rComponentLength in enumerate(re):
                                
                fromML = RiserLengthML-rComponentLength
                RiserLengthML = fromML
                fromMLArray[rCounter+1] = RiserLengthML
                
                print (RiserLengthML)
                
        print(fromMLArray)
                
with open('C:/Users/AceEngineer/Dropbox/0119 Programming/007 Stack-Up/Rev2/stackUpDrawing.py','w') as stackup:
    StackUpDiagram()

stackup.close()

with open('C:/Users/AceEngineer/Dropbox/0119 Programming/007 Stack-Up/Rev2/stackUpDrawing.py','a') as stackup:
    print('import os', file=stackup)
    print('import xlrd', file=stackup)
    print('import pygame, sys', file=stackup)
    print('from pygame.locals import *', file=stackup)
    print('from dataProvision.colors import *', file=stackup)
    #print('from colors import aceColors', file=stackup)
    print('DISPLAY=pygame.display.set_mode((800,800),0,32)', file=stackup)
    print('main()', file=stackup)
    

stackup.close()
