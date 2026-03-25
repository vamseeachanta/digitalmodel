import matplotlib as m
import matplotlib.pyplot as plt
import numpy as np

cdict = {
  'red'  :  ( (0.0, 0.25, .25), (0.02, .59, .59), (1., 1., 1.)),
  'green':  ( (0.0, 0.0, 0.0), (0.02, .45, .45), (1., .97, .97)),
  'blue' :  ( (0.0, 1.0, 1.0), (0.02, .75, .75), (1., 0.45, 0.45))
}

cm = m.colors.LinearSegmentedColormap('my_colormap', cdict, 1024)

x = np.arange(0, 10, .1)
y = np.arange(0, 10, .1)
X, Y = np.meshgrid(x,y)

data = 2*( np.sin(X) + np.sin(3*Y) )

def do_plot(n, f, title):
    #plt.clf()
    plt.subplot(1, 3, n)
    plt.pcolor(X, Y, f(data), cmap=cm, vmin=-4, vmax=4)
    plt.title(title)
    plt.colorbar()

plt.figure()
do_plot(1, lambda x:x, "all")
# do_plot(2, lambda x:np.clip(x, -4, 0), "<0")
# do_plot(3, lambda x:np.clip(x, 0, 4), ">0")
plt.show()