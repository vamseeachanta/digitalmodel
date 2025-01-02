from DataProvision.plateBucklingCal_G import *
from DataProvision.plateBucklingCal_H import *
from DataProvision.plateBucklingCal_i import *
from DataProvision.plateBucklingCal_J import *
from DataProvision.plateBucklingCal_K import *


import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
box = dict(facecolor='Blue', pad=5, alpha=0.2)
fig=plt.figure(1)
ax1=fig.add_subplot(511)
plt.setp(ax1.get_xticklabels(), visible=False)
ax1.plot([σscryG_simp,σscryH_simp,σscryI_simp,σscryJ_simp,σscryK_simp],color='k')
ax2=fig.add_subplot(512)
plt.setp(ax2.get_xticklabels(), visible=False)
ax2.plot([σucryG_simp2,σucryH_simp2,σucryI_simp2,σucryJ_simp2,σucryK_simp2],color='b')
ax3=fig.add_subplot(513)
plt.setp(ax3.get_xticklabels(), visible=False)
ax3.set_ylabel('Buckling resistance', bbox=box)
ax3.plot([σscryG_side,σscryH_side,σscryI_side,σscryJ_side,σscryK_side],color='g')
ax4=fig.add_subplot(514)
plt.setp(ax4.get_xticklabels(), visible=False)
ax4.plot([σucryG_side2,σucryH_side2,σucryI_side2,σucryJ_side2,σucryK_side2],color='m')
ax5=fig.add_subplot(515)
ax5.set_xlabel('Plate Number', bbox=box)
plt.suptitle('Transerverse Loading', bbox=box)
ax5.plot([σG_yrd,σH_yrd,σI_yrd,σJ_yrd,σK_yrd],color='c')

plt.show()
