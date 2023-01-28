# -*- coding: utf-8 -*-

### this code is for extracting and preprocessing GSR data around the target spot, from the experiment in Maastricht (matlab files)
### the python ('p') file you get (e.g. 'gsr_mu_segments.p') is going to contain GSR data segments, and can be used for further statistics and plotting

from os import listdir, getcwd
from os.path import join
from math import ceil
from scipy import nanmean
from scipy.io import loadmat
from scipy.stats import zscore
import numpy as np
from scipy.signal import butter, lfilter, detrend
import matplotlib.pyplot as plt
from colorsys import hsv_to_rgb
from datetime import datetime
from pickle import dump
dataPath = ''
datafiles = []
for filename in listdir(join(getcwd(), dataPath)):
    if filename.lower().endswith(('raw_data.mat')) :
        datafiles.append(filename)

#datafiles = datafiles[3:7] #### for testing
print(datafiles)

freq = 500 # Hz
target_from_left = 'R242' # marker codes
target_from_right = 'R250'
outset_from_left = 'R241'
outset_from_right = 'R249'
finish_from_left = 'R244'
finish_from_right = 'R252'
search_results = { 'spot_to_check' : [], 'p_value' : [], 'cohen_d' : [], 'auc' : [], 'predictor' : [] }

#############################
#spot_to_check = 21 # which out of the 32 spots to check (From left to right. 1 spot per sec.)
window_length = 2 # sec
slide_length = -2 # sec
bl_length = 3 # sec
#maxtrial = 4 # number of trials (Max 40. Must be even.)
zscored = True  # normalization
detrended = True  # linear detrending
save_note = 'interval-based' #"\n\n" + "this is for GSR"
trm = 0*freq # just to trim one sec from sides of epochs
#############################
half_length = 5*freq # determine half-length for both direction display

interval = 16164 # Hz (= 32.328 seconds)
spots = [x * 500 for x in range(21, 22)] # start, end+1, default: (5, 28), real target: 21
current_spot_to_check = 10592 # 21.184 sec


def execute():
    global maxtrial
    maxtrial = 18 # save all 18 trials (excluding the first and the last)
    for spot in spots:
        run_analyses(spot)
        dump( to_save, open( ("gsr_mu_segments_at_spot_" + spot_num + ".p"), "wb" ) ) ## this is the important part: saves the main variables for further analyses (and plotting)

    #write_search_results()

def basic_variables():
    global wi, sl, bl, spot_from_L, spot_from_R, pre_dict, GSR_M_epochs, GSR_M_windows, GSR_M_spot, save_main_results, to_save
    wi = window_length * freq # adjust seconds to data point frequency
    sl = slide_length * freq
    bl = bl_length * freq

    spot_from_L = spot_to_check # 10592 data points
    spot_from_R = interval - spot_to_check # 5572 data points

    pre_dict = { 'guilt' : [], 'subject' : [], 'GSR_mean' : [], 'GSR_corrected' : [] }
    GSR_M_windows = []
    GSR_M_epochs = []
    to_save = { 'guilt' : [], 'subject' : [], 'all_segments' : [] }
    save_main_results = ''

def run_analyses(current_spot_to_check):
    global data, current_file, spot_to_check, spot_num
    spot_to_check = current_spot_to_check
    spot_num = str(int(spot_to_check/500))
    print("____________________Current spot: ", spot_num)
    basic_variables()
    for current_file in datafiles:
        #print('_____________________')
        get_subject_info()
        print(current_file, '(sub' + subject + ') -', guilt)
        data = loadmat(current_file)
        #print(data)
        get_times()
        process_GSR()
    make_plots(GSR_M_epochs, 'entire') # this is not important for this code, included only as a check
    make_plots(GSR_M_windows, 'window') # this is not important for this code, included only as a check

def get_subject_info(): # guilt and condition from filename
    global guilt, subject
    cond = current_file[-14]
    if int(cond) < 3:
        guilt = 0 #innoxent
    else:
        guilt = 1 #guilty
    subject = current_file.split('_')[0][2:]
    pre_dict['guilt'].append(guilt)
    pre_dict['subject'].append(subject)
    # for pickle
    to_save['guilt'].append(guilt)
    to_save['subject'].append(subject)

def get_times():
    global data, target_times_full, target_times
    times_L_full = []
    times_R_full = []
    for row in data['Markers'][0]: # look for trial outsets
        if row[1] in [outset_from_left]:
            times_L_full.append(row[3][0][0])
        elif row[1] in [outset_from_right]:
            times_R_full.append(row[3][0][0])
        if len(times_L_full) >= (maxtrial/2 +2) and len(times_R_full) >= (maxtrial/2 +2):
            break

    times_L_full = times_L_full[0:int(maxtrial/2 +2)]
    times_R_full = times_R_full[0:int(maxtrial/2 +2)]
    times_L_full_spotted = [[t, spot_from_L] for t in times_L_full] # adjust to get target times
    times_R_full_spotted = [[t, spot_from_R] for t in times_R_full]
    #print('times_L_full_spotted', times_L_full_spotted)
    #print('times_R_full_spotted', times_R_full_spotted)

    target_times_full = times_L_full_spotted + times_R_full_spotted

    target_times_full.sort()
    target_times_full = target_times_full[1:maxtrial+1]
    #print(times_L_full_spotted)
    #print(times_R_full_spotted)
    #print(target_times_full)
    to_subtract = target_times_full[0][0]
    target_times = [[t[0] - to_subtract, t[1]-trm] for t in target_times_full]
    #print('len target_times max:',len(target_times))
    #print(target_times)

def butter_lowpass(to_filt):
    cutoff = 1
    order = 4
    nyq = 0.5 * freq
    normal_cutoff = cutoff / nyq
    b, a = butter(order, normal_cutoff, btype='low', analog=False)
    y = lfilter(b, a, to_filt)
    return y
def butter_highpass(to_filt):
    cutoff = 1
    order = 1
    nyq = 0.5 * freq
    normal_cutoff = cutoff / nyq
    b, a = butter(order, normal_cutoff, btype='low', analog=False)
    y = lfilter(b, a, to_filt)
    return y

def process_GSR():
    GSR = []
    for item in data['GSR']: #~1,000,000
        GSR.append(item[0])
    #print('len(GSR):', len(GSR))
    GSR = butter_lowpass(GSR)
    #GSR = butter_highpass(GSR)
    GSR = GSR[ target_times_full[0][0] : ]
    GSR = GSR[ : target_times[-1][0] + interval ]
    #print('len(GSR):', len(GSR))
    segments_per_subject = []
    window_per_subject = []
    #print(target_times)
    #  in target_indices[1]: spot_from_L is 10592, spot_from_R is 5572
    for target_indices in target_times:
        segment_base = GSR[target_indices[0]+trm:target_indices[0]+interval-trm]
        if detrended:
            segment_base = detrend(segment_base)
        segment = segment_base[target_indices[1]-half_length:target_indices[1]+half_length]
        windw = segment_base[target_indices[1]-bl+sl:target_indices[1]+wi+sl]
        if zscored:
            segment = zscore(segment)
            windw = zscore(windw)
        segments_per_subject.append(segment)
        window_per_subject.append(windw)

    to_save['all_segments'].append(segments_per_subject) ## SAVE all segments for subject

    segments_per_subject = np.array(segments_per_subject) # e.g. for plotting
    GSR_M_epochs.append(segments_per_subject.mean(axis=0)) # e.g. for plotting

    window_per_subject = np.array(window_per_subject) # e.g. for plotting
    GSR_M_windows.append(window_per_subject.mean(axis=0)) # e.g. for plotting


def downsample(signal, dsample_rate):
    signal = np.array(signal)
    pad_size = ceil(float(signal.size)/dsample_rate)*dsample_rate - signal.size
    signal_padded = np.append(signal, np.zeros(pad_size)*np.NaN)
    newsignal = nanmean(signal_padded.reshape(-1,dsample_rate), axis=1)
    return newsignal
def get_colrs(num):
    HSV_tuples = [(x*1.0/num, 0.5, 0.5) for x in range(num)]
    RGB_tuples = list(map(lambda x: hsv_to_rgb(*x), HSV_tuples))
    the_colors = []
    for idx, tpl in enumerate(RGB_tuples):
        if idx % 2 == 0:
            modif = 1.5 # multiplier
        else:
            modif = 1
        the_colors.append(tuple(x * modif for x in tpl)) # could be subtraction or sth
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
    #fig = plt.gcf()
    #
    plt.show()
    if detrended:
        detr = 'detrended_'
    #fig.savefig('D:/lukacs/workplace/outputs/GSR_' + detr + str(int(maxtrial)).zfill(2) + '_trials_in_' + type_of_segment + '_both_directions.png', dpi = 250, bbox_inches='tight')
    plt.close()

def plot_condition(epochs, subjects, title, guilt_or_inn, type_of_segment):
    dsample_rate = 1
    colrs = get_colrs(len(epochs))
    epochs_all = np.array(epochs).mean(axis=0)
    epochs_all = downsample(epochs_all, dsample_rate)
    epochs_all = zscore(epochs_all)

    #create and optimise figure
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
    #add individuals
    for idx, epoc in enumerate(epochs):
        epoc = downsample(epoc, dsample_rate)
        epoc = zscore(epoc)
        legends.append( plt.plot(epoc, '-', lw = 1, color = colrs[idx], label = subjects[idx].zfill(2))[0] )
    # add group average
    legends.append(plt.plot(epochs_all, lw = 2, color="black", label='all')[0])

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
        xrange = range( 0, int(len(epochs_all)), int(1*500/dsample_rate) )
        plt.xticks(xrange, [str(int( (x*dsample_rate -len(epochs_all)/2  ) / 500) ) for x in xrange], fontsize=10)
    else:
        xrange = range( 0, int(len(epochs_all)), int(500/dsample_rate*2))
        plt.xticks(xrange, [str(int( (x*dsample_rate - half_length ) / 500) ) for x in xrange], fontsize=10)

    # add vertical line at target
    if type_of_segment == 'window':
        t_pos = ((wi+bl)/dsample_rate)*0.98
    else:
        t_pos = (half_length)/dsample_rate
    plt.plot((t_pos, t_pos), (ymin, ymax*0.9), linestyle='--', color = 'gray', lw=1)
    win_start = t_pos - wi/dsample_rate
    plt.plot((win_start, win_start), (ymin, ymax*0.9), linestyle='-.', color = 'gray', lw=0.8)
    if type_of_segment != 'window':
        base_start = win_start - bl/dsample_rate
        plt.plot((base_start, base_start), (ymin, ymax*0.9), linestyle='-.', color = 'gray', lw=0.8)

    # make_plots(GSR_M_epochs, 'entire')
    # make_plots(GSR_M_windows, 'window')


##### EXECUTE ######
print(datetime.now().strftime("%Y%m%d_%H%M"))
execute()