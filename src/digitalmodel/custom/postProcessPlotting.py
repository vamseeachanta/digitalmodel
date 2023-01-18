import matplotlib.pyplot as pyplot
import matplotlib.pyplot as plt
import oyaml as yml
from matplotlib import rc

rc('mathtext', default='regular')

''' 
To plot each range graph for  files
'''
def postProcessPlotting(RangeAllFiles, cfg):
    # Plot all ranges and save plot
    rangePlot(RangeAllFiles, cfg)

def rangePlot(RangeAllFiles, cfg):
    for RangeGraphIndex in range(0, len(cfg['RangeGraph'])):
        suptitle = cfg['PlotSettings']['SupTitle']
        title = cfg['RangeGraph'][RangeGraphIndex]['Title']
        plt.suptitle(suptitle, fontsize=11)
        plt.title(title + '\n' + cfg['RangeGraph'][RangeGraphIndex]['Subtitle'], fontsize=10)
        # plt.title(title, fontsize=10)
        # plt.axes([.1,.1,.8,.7])
        # plt.figtext(.5,.9,cfg['RangeGraph'][RangeGraphIndex]['Subtitle'], fontsize=9, ha='center')

        for fileIndex in range(0, len(cfg['Files'])):
            x = RangeAllFiles[fileIndex][RangeGraphIndex]['X']
            VariableName = cfg['RangeGraph'][RangeGraphIndex]['Variable']
            y = RangeAllFiles[fileIndex][RangeGraphIndex][VariableName]
            label=cfg['Files'][fileIndex]['Label']

            plt.plot(x,y, label=label)

        xlabel = cfg['RangeGraph'][RangeGraphIndex]['xlabel']
        ylabel = cfg['RangeGraph'][RangeGraphIndex]['ylabel']
        FileName = cfg['Analysis']['result_folder'] + cfg['Analysis']['file_name'] + '_'+ cfg['RangeGraph'][RangeGraphIndex]['FileName']

        plt.xlabel(xlabel)
        plt.ylabel(ylabel)

        # plt.figtext(.5,.85,'Lorem ipsum dolor sit amet, consectetur adipiscing elit',fontsize=10,ha='center')

        if cfg['RangeGraph'][RangeGraphIndex]['axhline'] != None:
            plt.axhline(y = cfg['RangeGraph'][RangeGraphIndex]['axhline'], color ='r', dashes=[4,2])
        
        plt.legend()
        plt.grid()
        plt.savefig(FileName, dpi=800)
        plt.close()

    print("Saved {0} Range Graphs" .format(len(cfg['RangeGraph'])))
