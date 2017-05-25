#Rolf Fredheim, October 2013

#Read one line at a time: avoids loading everything into memory
writer=open("reshapedMallet11","w+")
with open("mallet_output_files/mb_composition") as f:
    count=0
    for line in f:
        dict = {}
        if count %1000 ==0:print count
        count+=1
        if count>1:
            a=line.split()

            #use text number as ID
            Id=a[1]

            #create cluster topic and proportion pairings using zip function
            d=zip(a[2::2], a[3::2])

            #Add a dictionary for each text
            for i in d:
                #Populate the dictionary with each zip pair
                dict[int(i[0])]=round(float(i[1]),5)
        #Identify number of pairings from second row.
            if count==2:
                ncols=(len(line.split())-2)/2

            writer.write(Id)
            #Dict items are sorted, so we just write the proportion
            for n in range(ncols):
                try:
                    writer.write("\t"+str(dict.items()[n][1]))
                #To make sure there are no missing values
                except:
                    writer.write("\t"+str(0))
            #Add line break
            writer.write("\n")
writer.close()
