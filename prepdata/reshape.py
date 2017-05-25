infile = open('mb_comp.txt', 'r')
outfile = open('com.csv', 'w+')

outfile.write('file,topicnum,weight\n')
for line in infile:
    tokens = line.split('\t')
    fn = tokens[1]
    topics = tokens[2:]
    #outfile.write(fn[46:] + ",")
    for i in range(0,59):
        outfile.write(fn[46:] + ",")
        outfile.write(topics[i*2]+','+topics[i*2+1]+'\n')

