def textdata(z,x,y,c):
        basicfont = pygame.font.SysFont('arial', 15)
        text = basicfont.render(z, True,c)
        textrect = text.get_rect()
        textrect.centerx =DISPLAY.get_rect().centerx=x
        textrect.centery = DISPLAY.get_rect().centery=y
        DISPLAY.blit(text, textrect)
        pygame.display.update()
def main():
        DISPLAY.fill(aceColors.white)
        pygame.display.set_caption('Riser Stack-Up')
        pygame.draw.rect(DISPLAY,aceColors.red,(120,70,560,20))  # Elevation
        pygame.draw.rect(DISPLAY,aceColors.pink,(355,120,70,30.0))    # Diverter
        pygame.draw.circle(DISPLAY,aceColors.red,(390,170),8,0)    # Upper Flexile Joint
        pygame.draw.rect(DISPLAY,aceColors.grey,(380,177,20,92.0))    # Inner Barrel
        pygame.draw.rect(DISPLAY,aceColors.lightcyan,(380,229.0,20,93.5))    # Outer Barrel
        pygame.draw.rect(DISPLAY,aceColors.black,(384,330.5,10,30.75))    # Pup Joint
        pygame.draw.rect(DISPLAY,aceColors.mettalicwhite,(384,361.25,10,33.75))    # SLK_TOP
        pygame.draw.rect(DISPLAY,aceColors.yellow,(380,380.0,20,55.0))    # RJT_3
        pygame.draw.rect(DISPLAY,aceColors.yellow,(380,425.0,20,81.25))    # RJT_3
        pygame.draw.rect(DISPLAY,aceColors.red,(380,496.25,20,111.25))    # RJT_5
        pygame.draw.rect(DISPLAY,aceColors.green,(380,597.5,20,107.5))    # RJT_7
        pygame.draw.rect(DISPLAY,aceColors.mettalicwhite,(384,710.0,10,48.75))    # SLK_BOT
        pygame.draw.rect(DISPLAY,aceColors.mettalicwhite,(384,758.75,10,105.0))    # Riser Adaptor
        pygame.draw.circle(DISPLAY,aceColors.red,(390,650),8,0)    # Lower Flexile Joint
        pygame.draw.rect(DISPLAY,aceColors.pink,(355,700,70,1.16))    # LMRP
        pygame.draw.rect(DISPLAY,aceColors.black,(355,701.16,70,1.56))    # BOP
        pygame.draw.rect(DISPLAY,aceColors.blue,(355,702.7199999999999,70,75.0))    # Wellhead
        pygame.draw.rect(DISPLAY,aceColors.darkgrey,(384,9581.400000000001,70,40.0))    # Conductor
        pygame.draw.rect(DISPLAY,aceColors.lightblue,(120,320,560,10))  #MeanSeaLevel
        pygame.draw.rect(DISPLAY,aceColors.grey,(325,250,130,30)) # Tensioner Ring
        pygame.draw.line(DISPLAY,aceColors.black,(280,90),(340,250),5) # Right-side
        pygame.draw.line(DISPLAY,aceColors.black,(285,90),(345,250),5) # Right-side
        pygame.draw.line(DISPLAY,aceColors.black,(500,90),(430,250),5) # Left-Side
        pygame.draw.line(DISPLAY,aceColors.black,(505,90),(435,250),5) # Left-Side
        pygame.display.flip()
        pygame.image.save(DISPLAY, "testData/Riser Stack-up_3000ft.png")
import pygame
from pygame.locals import *
from dataProvision.colors import *
DISPLAY=pygame.display.set_mode((800,800),0,32)
main()
