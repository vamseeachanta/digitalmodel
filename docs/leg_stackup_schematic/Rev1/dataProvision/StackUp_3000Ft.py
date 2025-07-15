def textdata(z,x,y,a,b,c):
    basicfont = pygame.font.SysFont('arial', 15)
    text = basicfont.render(z, True, (a, b, c))
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
    DISPLAY.fill(white)
    
    pygame.display.set_caption('Riser Stack-Up')
    pygame.draw.rect(DISPLAY,red,(120,70,560,20)) # Elevation 6055 Ft
    pygame.draw.rect(DISPLAY,pink,(355,143,70,30)) # Diverter
    #pygame.draw.rect(DISPLAY,black,(380,180,20,40)) #  Joint
    pygame.draw.circle(DISPLAY, red, (390, 180), 8, 0)#UFJ
    pygame.draw.rect(DISPLAY,darkgrey,(382,188,15,62)) # Inner Barrel
    pygame.draw.rect(DISPLAY,lightcyan,(380,250,20,90)) # Outer Barrel
    pygame.draw.rect(DISPLAY,grey,(325,280,130,30)) # Tensioner Ring
    pygame.draw.rect(DISPLAY,blue,(120,320,560,15)) # Mean Sea Level
    pygame.draw.rect(DISPLAY,black,(385,340,10,20)) # PUP Joint
    pygame.draw.rect(DISPLAY,grey,(385,360,10,20)) # Slick Joint Top
    pygame.draw.rect(DISPLAY,yellow,(380,380,20,170)) # Buoyancy Joint
    pygame.draw.rect(DISPLAY,grey,(385,550,10,42)) # Slick Joint BOT
    pygame.draw.circle(DISPLAY, red, (390, 600), 8, 0)#LFJ
    pygame.draw.rect(DISPLAY,pink,(365,607,50,20)) # LMRP
    pygame.draw.rect(DISPLAY,black,(350,627,80,20)) # BOP
    pygame.draw.rect(DISPLAY,grey,(385,647,10,20)) # Joint
    pygame.draw.rect(DISPLAY,blue,(370,652,40,10)) # Well Head
    pygame.draw.rect(DISPLAY,brown,(120,667,560,50)) # Sea Bed
    pygame.draw.rect(DISPLAY,darkgrey,(375,667,30,50)) # conductor
    
    pygame.draw.line(DISPLAY, black,(280,90),(340,280),5)
    pygame.draw.line(DISPLAY, black,(285,90),(345,280),5) # Right-side
    pygame.draw.line(DISPLAY, black,(500,90),(430,280),5)
    pygame.draw.line(DISPLAY, black,(505,90),(435,280),5) # Left-Side

    ####Defining The names###

    pygame.draw.rect(DISPLAY, black, [685, 30, 110, 670], 1) # Elevation Above Mud line
    textdata('Elevation Above ',740,40,0,0,0)
    textdata(' Mud line Ft',740,55,0,0,0)
    
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) # Diverter 2985 Ft
    textdata('2985 ',705,135,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) ## UFJ 2983 Ft
    textdata('2983 ',705,165,0,0,0) 
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) ## Inner Barrel 2981 Ft
    textdata('2981 ',705,185,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) #Outer Barrel 2941 Ft
    textdata('2941 ',705,245,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) #Pup Joint 2871 Ft
    textdata('2900 ',705,314,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) # Slick joint top 2856 Ft
    textdata('2856 ',705,350,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) # Buoyancy Joint 1866 Ft
    textdata('1866 ',705,370,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) # Slick joint bot Joint 441 Ft
    textdata('441 ',705,545,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) # LFJ 61 Ft
    textdata('61 ',705,580,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) # LMRP 59.4 Ft
    textdata('59.4 ',705,599,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) # BOP 36.2 Ft
    textdata('36.2 ',705,618,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) # Wellhead 5 Ft
    textdata('5 ',705,640,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) # Conductor 0 Ft
    textdata('0 ',705,658,0,0,0)
    
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) # Elevation Below Mud line
    textdata('Elevation Below ',55,40,0,0,0)
    textdata(' Mud line Ft',40,55,0,0,0)
    
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) # Diverter 2985 Ft
    textdata('2985 ',90,135,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) ## UFJ 2983 Ft
    textdata('2983 ',90,165,0,0,0) 
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) ## Inner Barrel 2981 Ft
    textdata('2981 ',90,185,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) #Outer Barrel 2941 Ft
    textdata('2941 ',90,245,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) #Pup Joint 2871 Ft
    textdata('2900 ',90,314,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) # Slick joint top 2856 Ft
    textdata('2856 ',90,350,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) # Buoyancy Joint 1866 Ft
    textdata('1866 ',90,370,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) # Slick joint bot Joint 441 Ft
    textdata('441 ',90,545,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) # LFJ 61 Ft
    textdata('61 ',90,580,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) # LMRP 59.4 Ft
    textdata('59.4 ',90,599,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) # BOP 36.2 Ft
    textdata('36.2 ',90,618,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) # Wellhead 5 Ft
    textdata('5 ',90,640,0,0,0)
    pygame.draw.rect(DISPLAY, black, [5, 30, 110, 670], 1) # Conductor 0 Ft
    textdata('0 ',90,658,0,0,0)

    pygame.draw.aaline(DISPLAY,black, (150, 70),(650, 70),True) ##Elevation 
    textdata('Elevation 6055 Ft',650,65,0,0,0)
    pygame.draw.aaline(DISPLAY,black, (150, 142),(650, 142),True) ##Diverter
    textdata('Diverter',630,135,0,0,0)
    pygame.draw.aaline(DISPLAY,black, (150, 171),(650, 171),True) ##UFJ
    textdata('UFJ',625,165,0,0,0)
    pygame.draw.aaline(DISPLAY,black, (150, 188),(650, 188),True) ##Inner Barrel
    textdata('Inner Barrel',180,180,0,0,0)
    pygame.draw.aaline(DISPLAY,black, (150, 250),(650, 250),True) ##Outer Barrel
    textdata('Outer Barrel',620,243,0,0,0)
    pygame.draw.aaline(DISPLAY,black, (150, 280),(650, 280),True) ##Tensioner Ring
    textdata('Tensioner Ring Elevation 6029 Ft',220,273,0,0,0)
    textdata('Mean Sea Level',635,310,0,0,0)
    pygame.draw.aaline(DISPLAY,black, (150, 340),(650, 340),True) ##PUP Joint
    #textdata('PUP Joint',625,345,0,0,0)
    pygame.draw.aaline(DISPLAY,black, (150, 360),(650, 360),True) ##Slick joint TOP
    textdata('Slick Joint TOP',615,350,0,0,0)
    pygame.draw.aaline(DISPLAY,black, (150, 380),(650, 380),True) ##Buoyancy Joint
    textdata('Buoyancy Joint',610,370,0,0,0)
    pygame.draw.aaline(DISPLAY,black, (150, 555),(650, 555),True) ##Slick Joint
    textdata('Slick Joint',190,545,0,0,0)
    pygame.draw.aaline(DISPLAY,black, (150, 591),(650, 591),True) ##LFJ
    textdata('LFJ',625,582,0,0,0)
    pygame.draw.aaline(DISPLAY,black, (150, 607),(650, 607),True) ##LMRP
    textdata('LMRP',180,598,0,0,0)
    pygame.draw.aaline(DISPLAY,black, (150, 627),(650, 627),True) ##BOP
    textdata('BOP',625,615,0,0,0)
    pygame.draw.aaline(DISPLAY,black, (150, 652),(650, 652),True)##Well Head
    textdata('Well Head',625,642,0,0,0)
    #pygame.draw.aaline(DISPLAY,black, (150, 645),(650, 645),True)
    #textdata('Mudline Elevation 0 Ft',600,660,0,0,0)
    textdata('Seabed',180,658,0,0,0)
    textdata('Conductor',625,690,0,0,0)
    pygame.draw.aaline(DISPLAY,black, (150, 667),(650, 667),True)

    pygame.display.flip()
    pygame.image.save(DISPLAY, "Riser Stack-up_3000ft.png")  # Save Output as Png
    
    
import pygame
from pygame.locals import *

DISPLAY=pygame.display.set_mode((800,800),0,32)
##wb = xlrd.open_workbook('RiserStackUp_Data.xlsx')  # Open Excel file
##sh1 = wb.sheet_by_name(u'Colour Codes')
###sh2 = wb.sheet_by_name(u'Stack up Data')
main()
