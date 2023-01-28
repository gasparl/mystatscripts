#!/usr/bin/env python
# encoding: utf-8
from codecs import open
from os import listdir, getcwd, rename
from os.path import join
from statistics import mean, stdev
from scipy.stats import pearsonr, ttest_rel, ttest_ind
from decimal import Decimal, ROUND_HALF_UP

out_file = "rtcit_stats.txt"

def execute():
    global invalid
    for currentFile in datafiles:
        print(currentFile)
        invalid = 0
        doFile(currentFile)
        #filtFile(currentFile)
    showStats()
    if False:
        showB_Stats("0", "1")
        showB_Stats("0", "2")
        showB_Stats("1", "2")
    #writeBetween()
    #filtDoubleIPs()
    #disp_age_gender()
    #save_var()

def disp_age_gender():
    age_l = preDict['d_age']
    gender_l = preDict['d_gender']
    print('______')
    print('Age:')
    print('  M =', ro(mean(age_l),2), ', SD =', ro(stdev(age_l),2))
    print('Gender:')
    print('  male:', str(gender_l.count('1')),';', ro(100*gender_l.count('1')/len(gender_l),2),"%")
    print('  female:', str(gender_l.count('2')),';', ro(100*gender_l.count('2')/len(gender_l),2),"%")

def save_var():
    from pickle import dump
    unids = preDict['d_subject']
    print(unids)
    cnds = preDict['d_condition']
    print(cnds)
    dump([unids, cnds],  open( "unids_conds_Exp1_Full.p", "wb" ))
    #dump(preDict,  open( "predictors_Exp1_Temp.p", "wb" ))

preDict = { 'd_condition' : [], 'd_subject' : [], 'd_gender' : [], 'd_age' : [], 'd1RTmeanDifs' : [], 'd2RTmeanPr' : [], 'd3RTmeanIr' : [], 'd4RTmeanAll' : [], 'd5AccuracyDifs' : [], 'd6AccuracyPr' : [], 'd7AccuracyIrr' : [], 'd8AccuracyAll' : [], 'd9AccTarget' : [], 'd10RTmeanTarget' : [], 'Coh_dCIT' : [] }
safe = 0
dataPath = '' #'C:/Users/Gaspar/Desktop/Qciat_results/'

all_trial_toofew = []
def filtFile(currentFile):
    if invalid != 0:
        if invalid == 1:
            xprefix = "xNotFull_"
        if invalid == 2:
            xprefix = "xAcc50_"
        if invalid == 3:
            xprefix = "xTooFewValid_"
        rename(currentFile, xprefix + currentFile)
        print('renamed:', currentFile)

def filtDoubleIPs():
    bad_indices = [i for i, x in enumerate(IPs) if IPs.count(x) > 1]
    doubleIP_files = []
    for IP_index in bad_indices:
        doubleIP_files += [datafiles[IP_index]]
        #print(IPs[IP_index])
    for badfile in list(set(doubleIP_files)):
        rename(badfile, "xIP_" + badfile)
        print('renamed for double IP:', badfile)

datafiles = []
for file in listdir(join(getcwd(), dataPath)):
    if file.lower().endswith('.txt') and ( file.startswith('uvacit_') or file.startswith('yyx') ) and len(file) > 13:
        datafiles.append(file)

###### exclusions

# 1ST: EXCLUDE DOUBLE-IP (using xFiles too)
check_fullness = False  # 2nd: check if data file is full # True / False
check_acc = False  # 3rd: check if acc below 50% # True / False
# 4th: EXCLUDE INDUCER ACC BELOW 50%
validate = False  # 5th: check has at least 50% trials # True / False

####################

highsal = ['"dates"','"countries"']
lowsal = ['"colors"','"animals"']
allsal = ['"dates"','"countries"', '"colors"','"animals"'] [ 0:2 ]
fils = ['""', '"#"', '"%"'] #[ 0 ]
blur = ['"no"', '"yes"']# [ 0 ]
condToCheck = ["0", "1", "2"][ 0:3 ]
# disp_age_gender()

IPs = []
def doFile(currentFile):
    global data, theNames, fixNameTypes, IPs, doubleIP_files, ALLIP, BADIP, current_condition
    data = open(dataPath + currentFile, 'r', encoding='utf-8')
    thisIP = data.readline().split('\t')[2]
    if thisIP in IPs:
        print("IP more than once!!*******", thisIP)
    IPs += [thisIP]

    current_condition = data.readline().split('\t')[8]

    if current_condition in condToCheck:
        temp_line = data.readline()
        preDict['d_subject'].append(temp_line.split('\t')[9])
        preDict['d_gender'].append(temp_line.split('\t')[6])
        preDict['d_age'].append(int(temp_line.split('\t')[5]))
        preDict['d_condition'].append(current_condition)

        selectData()
    data.close()


validProbes = []
def selectData():
    global slowCount, safe, correctCount, wrongCount, trialsCount, correctRTs, wrongRTs, tTrialCount, tCorrectCount, validProbes, invalid

    data.seek(0)

    correctCount = {'irrs' : 0, 'probes' : 0}
    trialsCount = {'irrs' : 0, 'probes' : 0}

    tTrialCount = 0
    tCorrectCount = 0
    tRTs = []
    valid_trials = 0
    all_trials = 0

    slowCount = {'irrs' : 0, 'probes' : 0}
    correctCount = {'irrs' : 0, 'probes' : 0}
    wrongCount = {'irrs' : 0, 'probes' : 0}
    trialsCount = {'irrs' : 0, 'probes' : 0}
    correctRTs = {'irrs' : [], 'probes' : []}
    wrongRTs = {'irrs' : [], 'probes' : []}

    for line in data: ##  and columns[16] in fils \
        columns = line.split('\t')
        if len(columns) > 3 and columns[8].isdigit() and columns[19] != '9999' and columns[15] in allsal :

            if columns[14] == '"probe"':
                temptype = 'probes'
                trialsCount[temptype] += 1
                if columns[19] == '1':
                    correctCount[temptype] += 1
            elif columns[14] == '"target"':
                tTrialCount += 1
                if columns[19] == '1':
                    tCorrectCount += 1
            elif columns[14] == '"irrelevant"':
                temptype = 'irrs'
                trialsCount[temptype] += 1
                if columns[19] == '1':
                    correctCount[temptype] += 1
            else:
                temptype = "other"

            if (columns[14] == '"probe"' or columns[14] == '"irrelevant"') and float(columns[12]) >= 150 and float(columns[12]) <= 800:
                if columns[19] == '1':
                    correctRTs[temptype] +=  [float(columns[12])]
            if columns[14] == '"target"' and float(columns[12]) >= 150 and float(columns[12]) <= 800:
                if columns[19] == '1':
                     tRTs +=  [float(columns[12])]

        # all trials
        if check_fullness and len(columns) > 3 and columns[8].isdigit():
                all_trials += 1
        # valid trials
        if validate and len(columns) > 3 and columns[8].isdigit() and float(columns[12]) >= 150 and float(columns[12]) <= 800 and columns[19] == '1':
                valid_trials += 1

    if trialsCount['probes'] == 0 :
        print("!!!!!!!!!!!! 0 probe trials")
        trialsCount['probes'] = 9999
    if trialsCount['irrs'] == 0 :
        print("!!!!!!!!!!!! 0 irr trials")
        trialsCount['irrs'] = 9999
    if tTrialCount == 0 :
        print("!!!!!!!!!!!! 0 target trials")
        tTrialCount = 9999
    if check_acc and (float(correctCount['irrs'])/trialsCount['irrs']) <= 0.5:
        print("too few correct irrs:", float(tCorrectCount) / tTrialCount )
        invalid = 2
    if check_acc and float(float(correctCount['probes'])/trialsCount['probes']) <= 0.5:
        print("too few correct probes:", float(tCorrectCount) / tTrialCount )
        invalid = 2
    if check_acc and float(tCorrectCount) / tTrialCount  <= 0.5:
        print("too few correct Targets:", float(tCorrectCount) / tTrialCount )
        invalid = 2

    preDict['d5AccuracyDifs'].append( float(correctCount['irrs'])/trialsCount['irrs'] - float(correctCount['probes'])/trialsCount['probes'] )
    preDict['d6AccuracyPr'].append( float(correctCount['probes'])/trialsCount['probes'] )
    preDict['d7AccuracyIrr'].append( float(correctCount['irrs'])/trialsCount['irrs'] )
    preDict['d8AccuracyAll'].append( float(correctCount['irrs'] + correctCount['probes'])/ (trialsCount['irrs'] + trialsCount['probes']) )
    preDict['d9AccTarget'].append( float(tCorrectCount) / tTrialCount )

    validProbes.append(len(correctRTs['probes']))
    print(len(correctRTs['probes']))


    possible_all_trials = [432, 648] # [431, 432, 648, 647]
    if check_fullness and (all_trials not in possible_all_trials):
            print('*************************************************************************')
            print('all_trials:', str(all_trials))
            invalid = 1
    #print(valid_trials)
    if validate and valid_trials < (648/2) :
        if current_condition == '2' or current_condition == '5':
            print("---------------too few valid (<324):", valid_trials )
            invalid = 3
        elif valid_trials < (432/2):
            print("--------------too few valid (<216):", valid_trials )
            invalid = 3


    if len(correctRTs['probes']) < 2 :
        correctRTs['probes'] = [99999, 99995]
        print("!!!!!!!!!!!! too few (<2) probe")
    if len(correctRTs['irrs']) < 2 :
        correctRTs['irrs'] = [99999, 99995]
        print("!!!!!!!!!! too few (<2) irrs")
    if len(tRTs) < 2 :
        tRTs = [99999, 99995]
        print("!!!!!!!!!!!! too few (<2) targets")
    #### if len(correctRTs['probes']) < 36 :
        #print("!!!!   too few (<36) probe")
    #if len(correctRTs['irrs']) < 144 :
        #print("!!!!   too few (<144) irrs")
    #if len(tRTs) < 36 :
        #print("!!!!   too few (<36) targets")

    preDict['d10RTmeanTarget'].append(mean(tRTs))

    preDict['d1RTmeanDifs'].append(mean(correctRTs['probes']) - mean(correctRTs['irrs']) )
    preDict['d2RTmeanPr'].append(mean(correctRTs['probes']))
    preDict['d3RTmeanIr'].append(mean(correctRTs['irrs']) )
    preDict['d4RTmeanAll'].append(mean(correctRTs['probes'] + correctRTs['irrs']) )

    c0 = correctRTs['probes']
    c1 = correctRTs['irrs']
    cohens_d = (mean(c0) - mean(c1)) / ( stdev(c1) )
    preDict['Coh_dCIT'].append(cohens_d)

def ro(to_round, le = 3, z = False):
    to_round = Decimal(to_round)
    if le > 0:
        plus = '0' * le
        out_num = str(Decimal(to_round.quantize(Decimal('.' + plus), rounding=ROUND_HALF_UP)))
    elif le == 0:
        out_num = str(round(to_round))
    else:
        out_num = 'Wrong input...'
    if z and out_num.startswith('0'):
        out_num = out_num[1:]
    if z and out_num.startswith('-0'):
        out_num = '-' + out_num[2:]
    return out_num
def showStats():
    var1 = preDict['d2RTmeanPr']
    var2 = preDict['d3RTmeanIr']
    M1 = mean(var1)
    SD1 = stdev(var1)
    M2 =mean(var2)
    SD2 = stdev(var2)
    r = pearsonr(var1, var2)[0]
    Dwithin =  (M1 - M2) /  ( SD1 **2 + SD2**2 - 2 * r * SD1 * SD2 )**0.5
    print('_____________________')
    print(allsal, fils, blur, condToCheck)
    print('_____________________')
    t, p = ttest_rel(var1, var2)
    print("n =", len(preDict['d2RTmeanPr']) )
    df = len(var1) -1
    symb = "="
    if p < 0.001:
        p = 0.001
        symb = "<"
    print("(t(" + str(df) + ") = " + ro(t,2) + ", p", symb, ro(p,3,True) + ", dwithin = " + ro(Dwithin,2,True) + ")")
    print('probes:\t(M =', ro(M1,2) + ', SD =', ro(SD1,2) + ")")
    print('irrs:\t(M =', ro(M2,2) + ', SD =', ro(SD2,2) + ")")
    dif = preDict['d1RTmeanDifs']
    print('difs:\t', ro(mean(dif)) + '\t' + ro(stdev(dif)))

    print("probe per participant:", ro(mean(validProbes)))
    #print("dCIT:", ro(mean(preDict['Coh_dCIT'])), "sd:", ro(stdev(preDict['Coh_dCIT'])))
    #print(preDict['Coh_dCIT'])

def dPredict():
    hits = 0
    for value in preDict['Coh_dCIT']:
        if value > 0.1:
            hits += 1
    print(hits, len(preDict['Coh_dCIT']))
    print(hits/len(preDict['Coh_dCIT']))

def showB_Stats(cond1, cond2):
    print("\n** BETWEEN-stats for conds " + cond1 + " & " + cond2 + ":" )
    var1 = []
    var2 = []
    measur = 'd1RTmeanDifs' # 'd1RTmeanDifs' or 'd5AccuracyDifs'
    for i in range(len(preDict['d_condition'])):
        if preDict['d_condition'][i] == cond1:
            var1.append(preDict[measur][i])
        elif preDict['d_condition'][i] == cond2:
            var2.append(preDict[measur][i])
    n1 = len(var1)
    M1 = mean(var1)
    SD1 = stdev(var1)
    n2 = len(var2)
    M2 =mean(var2)
    SD2 = stdev(var2)
    Dbetween = (M1 - M2) / ( ((n1 - 1)*(SD1**2) + (n2 - 1)*(SD2**2)) / (n1 + n2 -2) )**0.5
    print('_____________________')
    print(allsal, fils, blur, condToCheck)
    print('_____________________')
    t, p = ttest_ind(var1, var2)
    print("n =", len(preDict['d_condition']) )
    df = n1 + n2 - 2
    symb = "="
    if p < 0.001:
        p = 0.001
        symb = "<"
    print("(t(" + str(df) + ") = " + ro(t,2) + ", p", symb, ro(p,3,True) + ", dbetween = " + ro(Dbetween,2,True) + ")")
    print('first:\t(M =', ro(M1,2) + ', SD =', ro(SD1,2) + ")")
    print('second:\t(M =', ro(M2,2) + ', SD =', ro(SD2,2) + ")")
    # showB_Stats("0", "2")

def writeBetween():
    output2 = open(dataPath + out_file, 'w', encoding='utf-8')
    dKeys = list(preDict.keys())
    predictLines = list(zip(*list(preDict.values())))
    output2.write('\t'.join(str(x) for x in dKeys) + '\n')
    i=0
    for line in predictLines:
            output2.write('\t'.join(str(x) for x in line) + '\n')
            i+=1
    output2.close()


# EXECUTE

execute()


#system('taskkill /IM Notepad2.exe /FI "WINDOWTITLE eq x*"')
#system("start "+ dataPath + "aUVAxAnalysis_results.txt")


