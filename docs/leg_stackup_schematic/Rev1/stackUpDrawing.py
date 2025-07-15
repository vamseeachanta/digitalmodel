
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

    pygame.display.flip()
    pygame.image.save(DISPLAY, "C:/Users/AceEngineer/Desktop/New folder/Rev1/testData/Riser Stack-up_3000ft.png")  # Save Output as Png
    
    
import pygame
from pygame.locals import *

DISPLAY=pygame.display.set_mode((800,800),0,32)
main()
