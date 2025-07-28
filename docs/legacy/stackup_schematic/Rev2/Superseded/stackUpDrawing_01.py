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
        pygame.draw.rect(DISPLAY,aceColors.pink,(355,120,70,40.0))    # Diverter
        pygame.draw.circle(DISPLAY,aceColors.red,(390,170),8,0)    # Upper Flexile Joint
        pygame.draw.rect(DISPLAY,aceColors.grey,(380,177,20,94.0))    # Inner Barrel
        pygame.draw.rect(DISPLAY,aceColors.lightcyan,(380,231.0,20,97.0))    # Outer Barrel
        pygame.draw.rect(DISPLAY,aceColors.black,(384,336.0,10,31.5))    # Pup Joint
        pygame.draw.rect(DISPLAY,aceColors.mettalicwhite,(384,367.5,10,37.5))    # SLK_TOP
        pygame.draw.rect(DISPLAY,aceColors.yellow,(380,400,20,50))    # RJT_3 Top height calculated by hand
        pygame.draw.rect(DISPLAY,aceColors.yellow,(380,450,20,55))    # RJT_3 ''
        pygame.draw.rect(DISPLAY,aceColors.red,(380,505,20,60))    # RJT_5    ''
        pygame.draw.rect(DISPLAY,aceColors.green,(380,565,20,65))    # RJT_7  ''
        pygame.draw.rect(DISPLAY,aceColors.mettalicwhite,(384,630,10,20))    # Riser Adaptor
        pygame.draw.circle(DISPLAY,aceColors.red,(390,636),8,0)    # Lower Flexile Joint
        pygame.draw.rect(DISPLAY,aceColors.pink,(355,650,70,8))    # LMRP
        pygame.draw.rect(DISPLAY,aceColors.black,(355,658.32,70,8))    # BOP
        pygame.draw.rect(DISPLAY,aceColors.blue,(355,666.44,70,50))    # Wellhead
        pygame.draw.rect(DISPLAY,aceColors.brown,(120,706,560,30))  #SeaBed

        
        pygame.draw.rect(DISPLAY,aceColors.blue,(120,310,560,20))  #Mean Sea Level
        pygame.draw.rect(DISPLAY,aceColors.grey,(325,250,130,30)) # Tensioner Ring
        pygame.draw.line(DISPLAY,aceColors.black,(280,90),(340,250),5) # Right-side
        pygame.draw.line(DISPLAY,aceColors.black,(285,90),(345,250),5) # Right-side
        pygame.draw.line(DISPLAY,aceColors.black,(500,90),(430,250),5) # Left-Side
        pygame.draw.line(DISPLAY,aceColors.black,(505,90),(435,250),5) # Left-Side
        pygame.display.flip()
        pygame.image.save(DISPLAY, "Riser Stack-up_3000ft.png")
import pygame
from pygame.locals import *
from dataProvision.colors import *
DISPLAY=pygame.display.set_mode((800,800),0,32)
main()
