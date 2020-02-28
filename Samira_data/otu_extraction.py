file1 = open('otu_table_psn_v13.txt','r')
linesfile1=file1.readlines()
names=["Anterior_nares","Left_Antecubital_fossa","Palatine_Tonsils","Saliva","Throat","Attached_Keratinized_gingiva","Left_Retroauricular_crease","Posterior_fornix","Stool","Tongue_dorsum","Buccal_mucosa","Mid_vagina","Right_Antecubital_fossa","Subgingival_plaque","Vaginal_introitus","Hard_palate","Right_Retroauricular_crease","Supragingival_plaque"]
sampleID=[]
sampleID2=[]
otu=[]
check=[]
count2=0
count3=0
count4=0
fff=open ("ttt.txt","r")
linefff=fff.readlines()
for z in linesfile1:
   count2 +=1
   if count2==1:
    for k in range(0,2910):
     sampleID2.append(z.split('\t')[k])
print(count2)
#for pppp in range(0,2910):
# fff.write("{} {}".format(sampleID2[pppp]," "))
#for lll in range(0,2910):
# print(sampleID2[lll],"\n")
for x in names:
    count=0
    count4=0
    f=open ("data/"+x+"_2.txt","r")
    linesf=f.readlines()
    for y in linesf:
         count +=1
         sampleID.append(y[:9])

    #print(sampleID[12])
    f2=open("data6/"+x+".txt","w+")
    f2.write("{} {}".format("otuID#","\t"))
    for cc in range(0,count):
     #print(sampleID[cc],"\n") 
     f2.write("{} {}".format(sampleID[cc],"\t"))
    f2.write("\n")
    #print(type(sampleIDg[3]),type(sampleID2g[3]))
    for ll in range(0,2910):
      check.append(0)
    for b in range(0,2910):

     for a in range(0,count):


       if sampleID[a]==sampleID2[b]:
          check[b]=1
          print(x)
    for w in linesfile1:
           count4 +=1
           if count4>=2:
            for uu in range(0,2910):
             if uu==0:
              f2.write("{} {}".format(w.split('\t')[0],"\t"))
             if uu>=1:
               if check[uu]==1:
                  print(count4)
                  f2.write("{} {}".format(w.split('\t')[uu],"\t"))
            f2.write("\n") 
    f2.close()         
    f.close()
file1.close

