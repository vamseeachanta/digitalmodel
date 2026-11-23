#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Author: Vamsee Achanta
Date Updated: 2017-11-11
Objective: To write output in required format
'''

#it creates plot tittle, color and size
pyplot.title('Effective Tension vs Bending Moment plot', fontsize=14, fontweight='bold', color='black')
#it creates plot X-axis name, fontsize and colour
pyplot.xlabel('Bending Moment (kips-ft)', fontsize=12, fontweight='bold', color='black') 
#it creates plot Y-axis name, fontsize and colour             
pyplot.ylabel('Effective Tension  (kips)', fontsize=12, fontweight='bold', color='black')
#it converted Static max.effective tension value KN to Te and it plots these values along with arc length
pyplot.plot(x_axis, y_axis)
pyplot.savefig("Tension vs Moment.png",dpi=800)
