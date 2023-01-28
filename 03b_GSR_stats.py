# -*- coding: utf-8 -*-

# uses the data from preprocessed GSR (.p file) to calculate statistics and draw plots

from math import ceil
from scipy import nanmean
from scipy.stats import zscore, ttest_ind
from statistics import mean, stdev
from sklearn.metrics import roc_auc_score
from decimal import Decimal, ROUND_HALF_UP
import numpy as np
from scipy.signal import detrend
import matplotlib.pyplot as plt
from colorsys import hsv_to_rgb
from datetime import datetime
from pickle import load

spot_min = 21 #### for testing, should be 5
spot_max = 22 #### for testing, should be 28
data = {}
for spt_num in range(spot_min,spot_max): ## all spots: range(5,28)
    data[str(spt_num)] = load( open( "gsr_mu_segments_at_spot_" + str(spt_num) + ".p", "rb" ) ) # data[spt_num] is { 'guilt' : [], 'subject' : [], 'all_segments' : [] }

freq = 500 # Hz

#############################
#spot_to_check = 21 # which out of the 32 spots to check (From left to right. 1 spot per sec.)
window_start = -5 # sec
window_end = 0 # sec
baseline = 3 # sec
maxtrial = 4 # number of trials (Max 40. Must be even.)
zscored = True  # normalization, True or False (1 or 0)
save_note = 'interval-based' #"\n\n" + "this is for GSR"
trm = 1*freq # just to trim one sec from sides of epochs
detrended = 0 # True or False (1 or 0)
save_it = True # True or False (1 or 0)
#############################
half_length = 5*freq # for both direction display
w_start = window_start*freq
w_end = window_end*freq
w_bl = baseline*freq

interval = 16164 # Hz (= 32.328 seconds)
spots = [x * 500 for x in range(spot_min, spot_max)]
target_spot = 10592 # 21.184 sec - not needed in this search script

all_results = { 'spot' : [], 'PEAK_p_value' : [], 'PEAK_cohen_d' : [], 'PEAK_auc' : [], 'PEAK_mean_dif': [], 'SCL_p_value' : [], 'SCL_cohen_d' : [], 'SCL_auc' : [], 'SCL_mean_dif' : [] }

def execute():
    for spot in spots:
        run_analyses(spot)
        if save_it:
            write_stats()
    if save_it:
        write_search_results()

def basic_variables():
    global pre_dict, GSR_M_epochs, GSR_M_windows, GSR_M_spot, save_main_results
    pre_dict = { 'guilt' : [], 'subject' : [], 'PEAK' : [], 'SCL' : [] }
    GSR_M_windows = []
    GSR_M_epochs = []
    save_main_results = ''

def run_analyses(current_spot_to_check):
    global current_file, spot_to_check, spot_num, result_sum
    spot_to_check = current_spot_to_check
    spot_num = str(int(spot_to_check/500))
    all_results['spot'].append(spot_num)
    basic_variables()
    for idx, subject in enumerate(data[spot_num]['subject']):
        #print('_____________________')
        guilt = data[spot_num]['guilt'][idx]
        pre_dict['subject'].append(subject)
        pre_dict['guilt'].append(guilt)
        print('(sub' + subject + ') -', guilt)
        #print(data)
        process_GSR(data[spot_num]['all_segments'][idx])
    make_plots(GSR_M_epochs, 'entire')
    make_plots(GSR_M_windows, 'window')
    result_sum = ''
    calc_stats('PEAK')
    calc_stats('SCL')

def calc_stats(predictor_name):
    global result_sum
    var_i = []
    var_g = []
    predictor = pre_dict[predictor_name]
    for idx, val in enumerate(pre_dict['guilt']):
        if val == 0:
            var_i.append(predictor[idx])
        if val == 1:
            var_g.append(predictor[idx])
    M1 = mean(var_g)
    SD1 = stdev(var_g)
    n1 = len(var_g)
    M2 =mean(var_i)
    SD2 = stdev(var_i)
    n2 = len(var_i)
    d_between = (M1 - M2) / ( ((n1 - 1)*(SD1**2) + (n2 - 1)*(SD2**2)) / (n1 + n2 -2) )**0.5
    p = ttest_ind(var_i, var_g)[1]
    auc = roc_auc_score(pre_dict['guilt'], predictor)
    result_part_sum = ('\n__________________' + '\nPredictor: ' + predictor_name + "\nCohen's d: " + ro(d_between) + "; p value: " + ro(p) + "; AUC: " + ro(auc) + '\n  guilty:\t' + ro(M1) + '\t' + ro(SD1) + '\ninnocent:\t' + ro(M2) + '\t' + ro(SD2) )
    print(result_part_sum)
    result_sum += result_part_sum
    # for search
    all_results[predictor_name + '_p_value'].append(p)
    all_results[predictor_name + '_cohen_d'].append(d_between)
    all_results[predictor_name + '_auc'].append(auc)
    all_results[predictor_name + '_mean_dif'].append(M1-M2)

def write_stats():
    output = open("GSR_predictors_spot" + spot_num.zfill(2) + "_"+ datetime.now().strftime("%Y%m%d_%H%M") + ".txt", 'w', encoding='utf-8')
    d_keys = list(pre_dict.keys())
    predict_lines = list(zip(*list(pre_dict.values())))
    output.write('\t'.join(str(x) for x in d_keys) + '\n')
    i=0
    for line in predict_lines:
            output.write('\t'.join(str(x) for x in line) + '\n')
            i+=1

    output.write(save_main_results)
    output.write('\n\nFor SPSS:')
    for key in d_keys:
        output.write('\n' + key)
    output.close()

def write_search_results():
    output = open("mu_GSR_search_" + datetime.now().strftime("%Y%m%d_%H%M") + ".txt", 'w', encoding='utf-8')
    d_keys = list(all_results.keys())
    predict_lines = list(zip(*list(all_results.values())))
    output.write('\t'.join(str(x) for x in d_keys) + '\n')
    i=0
    for line in predict_lines:
            output.write('\t'.join(str(x) for x in line) + '\n')
            i+=1
    output.write('\n\nFor SPSS:')
    for key in d_keys:
        output.write('\n' + key)
    output.close()

def ro(toRound):
    toRound = Decimal(toRound)
    return str(Decimal(toRound.quantize(Decimal('.000'), rounding=ROUND_HALF_UP)))

def process_GSR(saved_segments):
    segments_per_subject = []
    window_per_subject = []
    base_segments_per_subject = saved_segments[0:maxtrial]
    for segment in base_segments_per_subject:
        windw = segment[half_length+w_start:half_length+w_end]
        if zscored:
            segment = zscore(segment)
            windw = zscore(windw)
        if detrended:
            segment = detrend(segment)
            windw = detrend(windw)
        segments_per_subject.append(segment)
        window_per_subject.append(windw)

    segments_per_subject = np.array(segments_per_subject).mean(axis=0) # e.g. for plotting
    GSR_M_epochs.append(segments_per_subject) # e.g. for plotting

    window_per_subject = np.array(window_per_subject).mean(axis=0) # e.g. for plotting
    GSR_M_windows.append(window_per_subject) # e.g. for plotting

    pre_dict['SCL'].append(mean(window_per_subject[w_bl:]))
    pre_dict['PEAK'].append(find_peak_pair(window_per_subject)[2]) #get individual peak differences


def downsample(signal, dsample_rate):
    signal = np.array(signal)
    pad_size = ceil(float(signal.size)/dsample_rate)*dsample_rate - signal.size
    signal_padded = np.append(signal, np.zeros(pad_size)*np.NaN)
    newsignal = nanmean(signal_padded.reshape(-1,dsample_rate), axis=1)
    return newsignal

def get_colrs(num): # creates a given number of RGB color codes that are nice and distinct - to be used in plots
    HSV_tuples = [(x*1.0/num, 0.5, 0.5) for x in range(num)] # evenly spaced colors
    RGB_tuples = list(map(lambda x: hsv_to_rgb(*x), HSV_tuples))
    the_colors = []
    for idx, tpl in enumerate(RGB_tuples):
        if idx % 2 == 0:
            modif = 1.5 # multiplier
        else:
            modif = 1
        the_colors.append(tuple(x * modif for x in tpl)) # changes hue for every second color
    return the_colors

def make_plots(epochs, type_of_segment):
    #epochs = GSR_M_epochs
    epochs_i = []
    epochs_g = []
    subjects_i = []
    subjects_g = []
    for idx, val in enumerate(pre_dict['guilt']):
        if val == 0:
            epochs_i.append(epochs[idx])
            subjects_i.append(pre_dict['subject'][idx])
        if val == 1:
            epochs_g.append(epochs[idx])
            subjects_g.append(pre_dict['subject'][idx])

    detr = ''
    if detrended:
        detr = '; detrended'
    title_add = ' (' + str(int(maxtrial)) + ' trials, in ' + type_of_segment + detr + '; both directions)'

    #initiate plot,
    plt.figure(figsize=(10, 8))

    plot_condition(epochs_g, subjects_g, "EDA in guilty group" + title_add, "g", type_of_segment)
    plot_condition(epochs_i, subjects_i, "EDA in innocent group" + title_add, "i", type_of_segment)

    plt.tight_layout(h_pad=4.0)
    ###
    if False: #save_it:
        fig = plt.gcf()
    #
    plt.show()

    if False: #save_it:
        if detrended:
            detr = 'detrended_'
        fig.savefig('GSR_from' + str(int(abs(window_start))) + 'to' + str(int(abs(window_end))) + '_' + detr + str(int(maxtrial)).zfill(2) + '_trials_in_' + type_of_segment + '_both_directions.png', dpi = 250, bbox_inches='tight')
    plt.close()

def plot_condition(epochs, subjects, title, guilt_or_inn, type_of_segment):
    dsample_rate = 1
    colrs = get_colrs(len(epochs))
    epochs_all = np.array(epochs).mean(axis=0)
    epochs_all = downsample(epochs_all, dsample_rate)

    #create and optimize figure
    if  guilt_or_inn == "g":
        ax = plt.subplot(211)
    elif guilt_or_inn == "i":
        ax = plt.subplot(212)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.get_xaxis().tick_bottom()
    ax.get_yaxis().tick_left()
    ax.set_ylim(-2,2)
    ymin, ymax = ax.get_ylim()

    legends = []
    #add individuals & their peaks
    for idx, epoc in enumerate(epochs):
        epoc = downsample(epoc, dsample_rate)
        legends.append( plt.plot(epoc, '-', lw = 1, color = colrs[idx], label = subjects[idx].zfill(2))[0] )
        draw_peak(epoc, colrs[idx])

    # add group average
    legends.append(plt.plot(epochs_all, lw = 2, color="black", label='all')[0])
    draw_peak(epochs_all, "black")

    #sort and add labels
    handles, labels = ax.get_legend_handles_labels()
    labels, handles = zip(*sorted(zip(labels, handles), key=lambda t: t[0])) # sort both labels and handles by labels
    labels = labels[-1:] + labels[:-1] # last to first
    handles = handles[-1:] + handles[:-1] # last to first

    #set title, legends, labels, ticks
    plt.title(title, fontsize=14)
    ax.legend(handles, labels, loc = 'upper left', bbox_to_anchor = (1.0, 0.9), fontsize=10 )
    plt.ylabel('EDA (z-scored mV)', fontsize=10)
    plt.xlabel('time (seconds)', fontsize=10)
    yrange = range(int(ymin+1), int(ymax), 1)
    plt.yticks(yrange, [str(x) for x in yrange], fontsize=10)
    if type_of_segment == 'window':
        xrange = range( 0, int(len(epochs_all)+1), int(1*500/dsample_rate) )
        plt.xticks(xrange, [ str(int( (x*dsample_rate + w_start  ) / 500) ) for x in xrange], fontsize=10)
    else:
        xrange = range( 0, int(len(epochs_all)+1), int(500/dsample_rate*2))
        plt.xticks(xrange, [str(int( (x*dsample_rate - half_length ) / 500) ) for x in xrange], fontsize=10)

    # add vertical line at target
    if type_of_segment == 'window':
        t_pos = ((-w_start)/dsample_rate)
    else:
        t_pos = (half_length)/dsample_rate
    plt.plot((t_pos, t_pos), (ymin, ymax*0.9), linestyle='--', color = 'gray', lw=1)
    win_start = t_pos + w_start
    plt.plot((win_start, win_start), (ymin, ymax*0.9), linestyle='-.', color = 'gray', lw=0.8)
    win_end = t_pos + w_end
    plt.plot((win_end, win_end), (ymin, ymax*0.9), linestyle='-.', color = 'gray', lw=0.8)

    # make_plots(GSR_M_epochs, 'entire')
    # make_plots(GSR_M_windows, 'window')



##### PEAK FINDING
def draw_peak(signl, colr):
    p_low, p_high = find_peak_pair(signl)[0:2] # returns tuple: (value, indexOfvalue)

    if p_high:
        plt.plot(p_high[1], p_high[0], marker = 7, fillstyle = 'bottom', markersize = 16, color = colr)
    if p_low:
        plt.plot(p_low[1], p_low[0], marker = 6, fillstyle = 'top', markersize = 16, color = colr)

def find_peak_pair(signal_base):
        # find highs
        signal = list(signal_base)
        maxpeaks = []
        previous = signal[0]
        idx = 0
        while idx < (len(signal)-2) :
            idx+=1
            point = signal[idx]
            nextp = signal[idx+1]
            if previous < point and point > nextp:
                maxpeaks.append( (point, idx) )
            elif previous < point and point == nextp:
                peak_start = idx
                while signal[idx] == signal[idx+1] and idx < (len(signal)-2) :
                    idx+=1
                    point = signal[idx]
                if signal[idx] < signal[idx+1]:
                    maxpeaks.append( (point, int( (peak_start + idx) / 2) ) )
            previous = point
        if len(maxpeaks) > 0:
            # find lows
            signal = list(signal_base)
            minpeaks = []
            previous = signal[0]
            idx = 0
            while idx < (len(signal)-2) :
                idx+=1
                point = signal[idx]
                nextp = signal[idx+1]
                if previous > point and point < nextp:
                    minpeaks.append( (point, idx) )
                elif previous > point and point == nextp:
                    peak_start = idx
                    while signal[idx] == signal[idx+1] and idx < (len(signal)-2) :
                        idx+=1
                        point = signal[idx]
                    if signal[idx] < signal[idx+1]:
                        minpeaks.append( (point, int( (peak_start + idx) / 2) ) )
                previous = point
            # makes min-max pairs
            peak_pairs = []
            for i, maxpk in enumerate(maxpeaks):
                minpeaks_before_high = [minp for minp in minpeaks if minp[1] < maxpk[1]]
                if len(minpeaks_before_high) > 0:
                    previous_minp = minpeaks_before_high[-1]
                    low_val = previous_minp[0]
                else:
                    previous_minp = False
                    low_val = 0
                peak_magnitude = maxpk[0] - low_val
                peak_pairs.append( ( previous_minp, maxpk, peak_magnitude) )
            # return min-max pair for largest peak (if any)
            return max(peak_pairs,key=lambda item:item[2])
        else:
            return (False, False, 0)
    ##################################


##### EXECUTE ######
print(datetime.now().strftime("%Y%m%d_%H%M"))
execute()