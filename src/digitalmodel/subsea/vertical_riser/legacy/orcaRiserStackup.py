import math
import os

import numpy as np
import xlrd

workbook = xlrd.open_workbook('riserStackup.xlsx')  # Open Excel file 
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
lightComponents = 'SLK_TOP', 'Pup Joint','Riser Adaptor'
barrelComponents = 'Inner Barrel', 'Outer Barrel', 'RJT_3', 'RJT_5','RJT_7'
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
        print("        pygame.init()", file=stackup) 
        print("        white = (255,255,255)", file=stackup)
        print("        lightcyan=(220,255,255)", file=stackup)
        print("        blue = (0,0,255)", file=stackup)
        print("        brown = (160,82,45)", file=stackup)
        print("        pink = (255,192,203)", file=stackup)
        print("        mettalicwhite =(233,235,226)", file=stackup)
        print("        red = (255,0,0)", file=stackup)
        print("        grey = (190,190,190)", file=stackup)
        print("        black = (0,0,0)", file=stackup)
        print("        darkgrey = (128,128,128)", file=stackup)
        print("        yellow = (255,255,0)", file=stackup)
        print("        green = (60,179,113)", file=stackup)
        print("        orange= (255,99,71)", file=stackup)
        print("        violet =(186,85,211)", file=stackup)
        print("        DISPLAY.fill(white)", file=stackup)
        print("        pygame.display.set_caption('Riser Stack-Up')", file=stackup)
        print("        pygame.draw.rect(DISPLAY,red,(120,"+str(topHeight)+",560,20))  # Elevation", file=stackup)
        
        
         
        
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
                        Colour = 'pink'
                elif inputcomponentName == 'Upper Flexile Joint':
                        Colour = 'red'
                elif inputcomponentName == 'Inner Barrel':
                        Colour = 'grey'
                elif inputcomponentName == 'Outer Barrel':
                        Colour = 'lightcyan'
                elif inputcomponentName == 'Tensioner Ring':
                        Colour = 'grey'
                elif inputcomponentName == 'Mean Sea Level':
                        Colour = 'blue'
                elif inputcomponentName == 'Pup Joint':
                        Colour = 'black'
                elif inputcomponentName == 'SLK_TOP':
                        Colour = 'mettalicwhite'
                elif inputcomponentName == 'RJT_3':
                        Colour = 'yellow'
                elif inputcomponentName == 'RJT_5':
                        Colour = 'red'
                elif inputcomponentName == 'RJT_7':
                        Colour = 'green'
                elif inputcomponentName == 'RJT_9':
                        Colour = 'orange'
                elif inputcomponentName == 'RJT_10':
                        Colour = 'violet'
                elif inputcomponentName == 'SLK_BOT':
                        Colour = 'mettalicwhite'
                elif inputcomponentName == 'Lower Flexile Joint':
                        Colour = 'red'
                elif inputcomponentName == 'LMRP':
                        Colour = 'pink'
                elif inputcomponentName == 'BOP':
                        Colour = 'black'
                elif inputcomponentName == 'Wellhead':
                        Colour = 'blue'
                elif inputcomponentName == 'Conductor':
                        Colour = 'darkgrey'
                pixel = 20*caltotalLengthOfComponent
                
                print("pixel: ",pixel)
                LenfromLeft = 355
                
                if caltotalLengthOfComponent < 10.00:
                        
                        pixel = 20*caltotalLengthOfComponent
                        print('if condition pixel :',pixel)
                else:
                        pixel =  caltotalLengthOfComponent/10
                        print('else condition pixel :',pixel)
                
                       
                if (inputcomponentName != 'Upper Flexile Joint' and inputcomponentName !='Lower Flexile Joint'):
                        for comp in heavyComponents:
                                if(comp in inputcomponentName):
                                        CompWidth = 70
                                        print("        pygame.draw.rect(DISPLAY,"+str(Colour)+",("+str(LenfromLeft)+","+str(topHeight+50)+","+str(CompWidth)+","+str(pixel)+"))    # "+str(inputcomponentName)+"", file=stackup)
                                        topHeight = topHeight+pixel
##                                if(comp=='LMRP', 'BOP', 'Wellhead'):
##                                        topHeight = 616
##                                        CompWidth = 70
##                                        print("        pygame.draw.rect(DISPLAY,"+str(Colour)+",("+str(LenfromLeft)+","+str(topHeight+50)+","+str(CompWidth)+","+str(pixel)+"))    # "+str(inputcomponentName)+"", file=stackup)
##                                        topHeight = topHeight+pixel
                        #topHeight = 199
                        for comp in barrelComponents:
                                if(comp in inputcomponentName ):
                                        CompWidth =20
                                        if(comp=='Inner Barrel' or comp=='Outer Barrel'):
                                                print("        pygame.draw.rect(DISPLAY,"+str(Colour)+",("+str(LenfromLeft+25)+","+str(topHeight+15)+","+str(CompWidth)+","+str(pixel+90)+"))    # "+str(inputcomponentName)+"", file=stackup)
                                                topHeight = topHeight+pixel+50
                                        else:
                                                topHeight =418 
                                                print("        pygame.draw.rect(DISPLAY,"+str(Colour)+",("+str(LenfromLeft+25)+","+str(topHeight)+","+str(CompWidth)+","+str(pixel+50)+"))    # "+str(inputcomponentName)+"", file=stackup)
                                                topHeight = topHeight+pixel+50
                        for comp in lightComponents:
                                if(comp in inputcomponentName ):
                                        CompWidth =10
                                        print("        pygame.draw.rect(DISPLAY,"+str(Colour)+",("+str(LenfromLeft+29)+","+str(topHeight+55)+","+str(CompWidth)+","+str(pixel+30)+"))    # "+str(inputcomponentName)+"", file=stackup)
                                        topHeight = topHeight+pixel+30  
                   
                        stackUpLength = caltotalLengthOfComponent+stackUpLength
                       
                elif(inputcomponentName == 'Upper Flexile Joint'):
                        topHeight = 183 
                        h =  topHeight+8
                        print("        pygame.draw.circle(DISPLAY,"+str(Colour)+",("+str(math.floor(LenfromLeft+35))+","+str(math.floor(h))+"),8,0)    # "+str(inputcomponentName)+"", file=stackup)
                elif(inputcomponentName == 'Lower Flexile Joint'):
                        topHeight = 600
                        print("        pygame.draw.circle(DISPLAY,"+str(Colour)+",("+str(math.floor(LenfromLeft+35))+","+str(math.floor(topHeight))+"),8,0)    # "+str(inputcomponentName)+"", file=stackup)
                        
                if inputcomponentName == 'Conductor':
                        RiserLengthML = stackUpLength-caltotalLengthOfComponent
                        fromMLArray[0] = stackUpLength-caltotalLengthOfComponent
        print("        pygame.display.flip()", file=stackup) 
                        
                        
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
                
with open('orcaRiserStackup_output.txt','w') as stackup:
    StackUpDiagram()

stackup.close()

with open('orcaRiserStackup_output.txt','a') as stackup:
    print('import os', file=stackup)
    print('import xlrd', file=stackup)
    print('import pygame, sys', file=stackup)
    print('from pygame.locals import *', file=stackup)
    print('DISPLAY=pygame.display.set_mode((800,800),0,32)', file=stackup)
    print('main()', file=stackup)

stackup.close()

# Generated output file executed
def textdata(z,x,y,c):
        basicfont = pygame.font.SysFont('arial', 15)
        text = basicfont.render(z, True,c)
        textrect = text.get_rect()
        textrect.centerx =DISPLAY.get_rect().centerx=x
        textrect.centery = DISPLAY.get_rect().centery=y
        DISPLAY.blit(text, textrect)
        pygame.display.update()
def main():
        pygame.init()
        white = (255,255,255)
        lightcyan=(220,255,255)
        blue = (0,0,255)
        brown = (160,82,45)
        pink = (255,192,203)
        mettalicwhite =(233,235,226)
        red = (255,0,0)
        grey = (190,190,190)
        black = (0,0,0)
        darkgrey = (128,128,128)
        yellow = (255,255,0)
        green = (60,179,113)
        orange= (255,99,71)
        violet =(186,85,211)
        DISPLAY.fill(white)
        pygame.display.set_caption('Riser Stack-Up')
        pygame.draw.rect(DISPLAY,red,(120,70,560,20))  # Elevation
        pygame.draw.rect(DISPLAY,pink,(355,120,70,40.0))    # Diverter
        pygame.draw.circle(DISPLAY,red,(390,191),8,0)    # Upper Flexile Joint
        pygame.draw.rect(DISPLAY,grey,(380,198,20,94.0))    # Inner Barrel
        pygame.draw.rect(DISPLAY,lightcyan,(380,252.0,20,97.0))    # Outer Barrel
        pygame.draw.rect(DISPLAY,black,(384,349.0,10,31.5))    # Pup Joint
        pygame.draw.rect(DISPLAY,mettalicwhite,(384,380.5,10,37.5))    # SLK_TOP
        pygame.draw.rect(DISPLAY,yellow,(380,418,20,140.0))    # RJT_3
        pygame.draw.rect(DISPLAY,yellow,(380,418,20,192.5))    # RJT_3
        pygame.draw.rect(DISPLAY,red,(380,418,20,252.5))    # RJT_5
        pygame.draw.rect(DISPLAY,green,(380,418,20,245.0))    # RJT_7
        pygame.draw.rect(DISPLAY,mettalicwhite,(384,718.0,10,130.0))    # Riser Adaptor
        pygame.draw.circle(DISPLAY,red,(390,600),8,0)    # Lower Flexile Joint
        pygame.draw.rect(DISPLAY,pink,(355,650,70,2.32))    # LMRP
        pygame.draw.rect(DISPLAY,black,(355,652.32,70,3.12))    # BOP
        pygame.draw.rect(DISPLAY,blue,(355,655.44,70,100.0))    # Wellhead
        pygame.display.flip()
import os
import sys

import pygame
import xlrd
from pygame.locals import *

DISPLAY=pygame.display.set_mode((800,800),0,32)
main()

