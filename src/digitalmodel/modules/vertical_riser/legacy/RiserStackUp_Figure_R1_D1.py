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
