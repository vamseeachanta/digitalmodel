import xlrd

#pygame.display.set_caption('Riser Stack-Up')
def open_file(path):
    Riserdata = xlrd.open_workbook(path)
    sheet = Riserdata.sheet_by_index(0)
    for i in range(sheet.nrows):
        try:
            data = sheet.row_values(i+1)
            if data[0] == "Upper Flexile Joint" or data[0] == "Lower Flexile Joint":
                print("pygame.draw.circle(DISPLAY,aceColors."+str(data[1])+",("+str(data[2])+","+str(data[3])+"),"+str(data[4])+","+str(data[5])+")  #" +str(data[0]))
                #pygame.draw.circle(DISPLAY,aceColors.+str(data[1])+,(+str(data[2])+,+str(data[3])+),+str(data[4])+,+str(data[5])+) "#"+str(data[0])
            else:
                print("pygame.draw.rect(DISPLAY,aceColors."+str(data[1])+",("+str(data[2])+","+str(data[3])+"),"+str(data[4])+","+str(data[5])+")  #" +str(data[0]))
                #pygame.draw.rect(DISPLAY,aceColors.+str(data[1])+,(+str(data[2])+,+str(data[3])+),+str(data[4])+,+str(data[5])+)
        except IndexError:
            return
#----------------------------------------------------------------------
if __name__ == "__main__":
   path = "Riserdata.xlsx"
   open_file(path)

    
