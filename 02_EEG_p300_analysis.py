from datetime import datetime
from os import listdir, getcwd
from os.path import join
from scipy.io import loadmat
import numpy as np
import matplotlib.pyplot as plt
from colorsys import hsv_to_rgb
from scipy.signal import butter, lfilter
from random import choice
from math import ceil

print("Start" , str(datetime.now()) )
n_iter = 100 # number of bootstrapping comparisons; usually 100, sometimes 1000
save_subj_plots = False
disp_subj_plots = False
save_group_plot = False
disp_group_plots = True
do_boots = False

probe_event_nums = [21, 31, 41]
irr_event_nums = [22,23,24,25,26,32,33,34,35,36,42,43,44,45,46]
comparisons = [([71],[72,73,74,75], 'secondary_p3', 'secondary task (target vs. nontarget)'), ([21],[22,23,24,25,26], 'firstnames_p3', 'Forenames'), ([31],[32,33,34,35,36], 'dates_p3', 'Dates'), ([41],[42,43,44,45,46], 'animals_p3', 'Animals'), ([21, 31, 41],[22,23,24,25,26,32,33,34,35,36,42,43,44,45,46], 'primary_p3', 'all Item Categories')] # event info for each comparison: any pairs of target(s) vs nontargets (including probe(s) vs. irrelevants), along with text for output (in stats summary and on plots)
# induced group: [11, 5, 13, 15, 27, 7, 9, 33, 35, 25, 1, 21, 29, 3, 17, 19]

dataPath = '' # (relative) path to the .set files
datafiles = []
for filename in listdir(join(getcwd(), dataPath)): # get the names of all appropriate datafiles in the folder of this script
    if filename.endswith(('_Pz.set')): # e.g. '_exp1.set' to get all exp1 files - but can be full filename to check only one data
        datafiles.append(filename)
    datafiles.sort()

group_name = ['_induced', '_standard']
font_small = 22
font_large = 25

def main_f():
    global pre_dict, dict_key_use, comparison_title, subject, plot_data, group_append
    print("- start main - " , str(datetime.now()) )
    pre_dict = {"subject_id" : [], "group" : [], "epochs_per_item" : []} # dictionary for the eventual results; boots_main is for the maintask (personal names), boots_sub is for the secondary task (sets of simple numbers)
    plot_data = {} # for group level plots
    for t_vs_nt in comparisons:
        dict_key_text = t_vs_nt[2]
        pre_dict[dict_key_text + '_probe'] = []
        pre_dict[dict_key_text + '_irrelevant'] = []
        pre_dict[dict_key_text + '_boots'] = []

        plot_data[dict_key_text + '_irrelevants' + group_name[0] ] = []
        plot_data[dict_key_text + '_probes' + group_name[0] ] = []
        plot_data[dict_key_text + '_irrelevants' + group_name[1] ] = []
        plot_data[dict_key_text + '_probes' + group_name[1] ] = []
    subject = 0
    for current_file in datafiles:
        #print('_____________________')
        #subject = current_file.split('_')[0] # get subject number
        subject = subject + 1
        if subject == 24:
            subject = 25
        if subject == 31:
            subject = 32
        if subject in [11, 5, 13, 15, 27, 7, 9, 33, 35, 25, 1, 21, 29, 3, 17, 19]:
            group = 1
            group_append = group_name[0]
        else:
            group = 2
            group_append = group_name[1]
        print(current_file, '(sub' + str(subject).zfill(2) + ')')
        process_data(current_file)
        pre_dict["subject_id"].append(str(subject).zfill(2))
        pre_dict["group"].append(group)
        probe_epocs = [ len(event_dict[str(key)]) for key in probe_event_nums ]
        irr_epocs = [ len(event_dict[str(key)]) for key in irr_event_nums]
        print('num of probe_epocs: ', probe_epocs)
        #print('num of irr_epocs: ', irr_epocs)
        avg_num_of_epochs = (sum(probe_epocs) + sum(irr_epocs)) / len(probe_epocs+irr_epocs)
        pre_dict["epochs_per_item"].append(avg_num_of_epochs)
        #print("------------------------", round(avg_num_of_epochs,2))
        for t_vs_nt in comparisons: # do all target vs nontarget comparison (stats and plots)
            dict_key_use = t_vs_nt[2]
            comparison_title = t_vs_nt[3]
            if do_boots:
                pre_dict[dict_key_use + '_boots'].append( boots( t_vs_nt[0], t_vs_nt[1] ) )
                pre_dict[dict_key_use + '_probe'].append( p3_pp( t_vs_nt[0] ) )
                pre_dict[dict_key_use + '_irrelevant'].append( p3_pp( t_vs_nt[1] ) )
            make_subj_plot(t_vs_nt[0], t_vs_nt[1])
    for t_vs_nt in comparisons: # do all target vs nontarget comparison for group plots
        dict_key_use = t_vs_nt[2]
        comparison_title = t_vs_nt[3]
        for grpp in [ 'Standard', 'Induced']:
            make_main_plot( grpp )
    if do_boots:
        write_results('write') # 'print' / 'write' / 'both'

def p3_pp(events): # simple p-p P3 measure (separately for target and nontarget)
    events_sum = sum( [ event_dict[str(key)] for key in (events) ], [] ) # sum epochs for p3 calculations
    p3_pp = peak_to_peak(events_sum)
    return p3_pp # p-p P3

def write_results(to_out): # writes results to txt file
    d_keys = list(pre_dict.keys())
    predict_lines = list(zip(*list(pre_dict.values())))
    text_to_write = '\t'.join(str(x) for x in d_keys) + '\n'
    for line in predict_lines:
        text_to_write += '\t'.join(str(x) for x in line) + '\n'
    if to_out == 'print' or to_out == 'both':
        print(text_to_write)
    if to_out == 'write' or to_out == 'both':
        output = open("boots_p300_results_" + datetime.now().strftime("%Y%m%d_%H%M") + ".txt", 'w', encoding='utf-8')
        output.write(text_to_write)
        output.close()

def process_data(file_name): # extracts EEG data from the .set files
    global event_dict
    alldata = loadmat(dataPath+file_name)
    eegdata = list(zip(*alldata['EEG']['data'][0][0][0])) #e.g. 618x717 -> 717x618
    event_per_epoch = alldata['EEG']['epoch'][0][0][0] #e.g. 618
    events = []
    for event in event_per_epoch: # events (769-780) for each epoch in sequential order
        events.append(event[1][0][0])
    # events: from 0 to 617 (618)
    event_dict = {}
    for indx, epoch in enumerate(eegdata): # group epochs per events into two dictionaries: event_dict
        event_now = str(events[indx])
        if event_now not in event_dict.keys():
            event_dict[event_now]=[]
        event_dict[event_now].append(epoch)
    # event_dict["event"] has around 40-70 epochs with 717 datapoints each

def boots(target_events, other_events): # bootstrapping as described in the article (for main)
    probe_pool = sum( [ event_dict[str(key)] for key in (target_events) ], [] ) # targets (71/21/31/41)
    n_sample = len(probe_pool)
    #irr_pool = sum( [ event_dict[str(key)] for key in (other_events) ], [] )
    p_larger_than_irr = 0
    for __ in range(n_iter):
        probe_sample = []
        irr_sample = []
        for i in range(0, ceil(n_sample/len(target_events))):
            for key in target_events:
                probe_sample.append(choice(event_dict[str(key)]))
        for i in range(0, ceil(n_sample/len(other_events))):
            for key in other_events:
                irr_sample.append(choice(event_dict[str(key)]))
        if peak_to_peak(probe_sample) > peak_to_peak(irr_sample): # see peak_to_peak function
            p_larger_than_irr += 1
    return(p_larger_than_irr) # how many times out of the X (n_iter) iterations the probe EEG-response sample average is larger than the irrelevants


def peak_to_peak(arrs, search_start_ms = 450, search_mid_ms = 900, search_end_ms = 1500): # peak-to-peak P300 as described in the article; searches from 400ms to 800ms then from first finding until 1300ms (+100 required due to -100ms baseline included)
    arr = np.array( arrs ).mean(axis=0)
    max_segm = -9999 # (max cannot be larger than this)
    min_segm = 9999 # (min cannot be smaller than this)
    max_segm_start = 0
    #min_segm_start = 0
    len_arr_per_1500 = len(arr)/1500
    seg_lengt = int(100*len_arr_per_1500)
    search_start = int(search_start_ms*len_arr_per_1500)
    search_mid = int(search_mid_ms*len_arr_per_1500)
    search_end = int(search_end_ms*len_arr_per_1500)-seg_lengt
    for start_point in range(search_start,search_mid):
        segment = np.mean(arr[start_point:start_point+seg_lengt])
        if segment > max_segm:
            max_segm = segment
            max_segm_start = start_point
    for start_point in range(max_segm_start,search_end):
        segment = np.mean(arr[start_point:start_point+seg_lengt])
        if segment < min_segm:
            min_segm = segment
            #min_segm_start = int(start_point/len_arr_per_1500) # start of segment in ms
    max_segm_start = int(max_segm_start/len_arr_per_1500) # start of segment in ms
    #max_segm, max_segm_start, min_segm, min_segm_start
    if max_segm == -9999 or min_segm == 9999: # (just a check: these should be replaced, if not, there was a problem)
        print("!!! Max/min not OK:", max_segm, min_segm)
    return max_segm - min_segm # the difference between the two segments is the p-p P300

def make_subj_plot(target_events, other_events): # plots for ERPs; for visual checks
    global len_ms_100, plot_data
    all_items_to_plot = {key: event_dict[str(key)] for key in ( target_events + other_events )}

    #initiate plot
    plt.figure(figsize=(10, 6))
    #create and optimise figure
    ax = plt.subplot()
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.get_xaxis().tick_bottom()
    ax.get_yaxis().tick_left()

    legends = []
    colrs = get_colrs(len(all_items_to_plot))
    targ_items_to_plot = {key: event_dict[str(key)] for key in ( target_events )}
    nontarg_items_to_plot = {key: event_dict[str(key)] for key in ( other_events )}
    targ_epocs = []
    nontarg_epocs = []
    for evnt, epoc in sorted(targ_items_to_plot.items()):
        targ_epocs += epoc
    for evnt, epoc in sorted(nontarg_items_to_plot.items()):
        nontarg_epocs += epoc
    num_of_targ_epocs = len(targ_epocs)
    targ_epocs = np.array(targ_epocs).mean(axis=0)
    num_of_nontarg_epocs = len(nontarg_epocs)
    nontarg_epocs = np.array(nontarg_epocs).mean(axis=0)
    ep_lenght = len(nontarg_epocs)
    len_ms_100 = ep_lenght/1500*100
    #targ_epocs = butter_lowpass(targ_epocs)
    #nontarg_epocs = butter_lowpass(nontarg_epocs)

    # correct baseline to be y = 0 at x = 0
    targ_epocs = targ_epocs - targ_epocs[int(len_ms_100)]
    nontarg_epocs = nontarg_epocs - nontarg_epocs[int(len_ms_100)]
    plot_data[dict_key_use + '_probes' + group_append].append(targ_epocs)
    plot_data[dict_key_use + '_irrelevants' + group_append].append(nontarg_epocs)

    i = 0
    probe_label = ' average'
    if len(all_items_to_plot.items()) < 9: # only add each item
        probe_label =  ''
        for evnt, epoc in sorted(all_items_to_plot.items()):
            num_of_epocs = len(epoc)
            epoc = np.array(epoc).mean(axis=0)
            #epoc = butter_lowpass(epoc)
            epoc = epoc - epoc[int(len_ms_100)] # correct baseline to be y = 0 at x = 0
            if evnt not in target_events:
                legends.append( plt.plot(epoc, '--', lw = 1, color = colrs[i], label = str(evnt) + " x " + str(num_of_epocs) )[0] )
            i += 1

    # add average probe
    legends.append( plt.plot(targ_epocs, '-', lw = 2, color = 'black', label = 'probe' + probe_label + ' x ' + str(num_of_targ_epocs) )[0] )
    legends.append( plt.plot(nontarg_epocs, '--', lw = 2, color = 'black', label = 'irrelevant average'  + ' x ' + str(num_of_nontarg_epocs))[0] )

    #sort and add labels
    handles, labels = ax.get_legend_handles_labels()
    labels, handles = zip(*sorted(zip(labels, handles), key=lambda t: t[0])) # sort both labels and handles by labels
    labels = list(labels) # making it mutable
    handles = list(handles)
    labels.insert(0, labels.pop(-2)) # second to last to first
    labels.insert(0, labels.pop(-1)) # last to first
    handles.insert(0, handles.pop(-2)) # second to last to first
    handles.insert(0, handles.pop(-1)) # last to first

    #set title, legends, labels, ticks
    plt.title("ERP plot for " + comparison_title, fontsize=font_large)
    #ax.legend(handles, labels, loc = 'upper left', bbox_to_anchor = (1.0, 0.9), fontsize=font_small )
    plt.ylabel('Amplitude (μV)', fontsize=font_small)
    plt.xlabel('Time (ms)', fontsize=font_small)
    #xrange = np.arange( 0, int(ep_lenght), 512/5) # SHOULD BE ADJUSTED TO -100/0/+100
    #plt.xticks(xrange, [str(int( x / 512 * 1000) ) for x in xrange], fontsize=10)
    plt.xticks([len_ms_100, len_ms_100*5, len_ms_100*9, len_ms_100*15], ["0", "400","800","1400"], fontsize=font_small)
    plt.yticks(fontsize=font_small)
    #ax.set_ylim(-2,2)
    ymin, ymax = ax.get_ylim()
    plt.plot((len_ms_100, len_ms_100), (ymin*1.1, ymax*0.9), linestyle='--', color = 'gray', lw=1)
    plt.plot((len_ms_100*5, len_ms_100*5), (ymin*1.1, ymax*0.9), linestyle='--', color = 'gray', lw=0.5)
    plt.plot((len_ms_100*9, len_ms_100*9), (ymin*1.1, ymax*0.9), linestyle='--', color = 'gray', lw=0.5)
    plt.plot((len_ms_100*15, len_ms_100*15), (ymin*1.1, ymax*0.95), linestyle='--', color = 'gray', lw=1)
    plt.tight_layout(h_pad=4.0)
    ###
    if save_subj_plots:
        fig = plt.gcf()
    #
    if disp_subj_plots:
        plt.show()
    if save_subj_plots:
        fig.savefig(str(subject).zfill(2) + '_' + dict_key_use + '.png', dpi = 250, bbox_inches='tight')
    plt.close()

def make_main_plot( grp_to_plt ): # plots for group ERPs
    #initiate plot
    plt.figure(figsize=(10, 6))
    #create and optimise figure
    ax = plt.subplot()
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.get_xaxis().tick_bottom()
    ax.get_yaxis().tick_left()

    legends = []

    targ_epocs_induced = np.array(plot_data[dict_key_use + '_probes' + group_name[0] ]).mean(axis=0)
    nontarg_epocs_induced = np.array(plot_data[dict_key_use + '_irrelevants' + group_name[0] ]).mean(axis=0)
    targ_epocs_standard = np.array(plot_data[dict_key_use + '_probes' + group_name[1] ]).mean(axis=0)
    nontarg_epocs_standard = np.array(plot_data[dict_key_use + '_irrelevants' + group_name[1] ]).mean(axis=0)

    ep_lenght = len(targ_epocs_induced)
    len_ms_100 = ep_lenght/1500*100

    if grp_to_plt != 'Induced':
        # add averages
        legends.append( plt.plot(targ_epocs_standard, '-', lw = 2, color = 'black', label = 'Probes' )[0] )
        legends.append( plt.plot(nontarg_epocs_standard, '--', lw = 2, color = 'black', label = 'Irrelevants' )[0] )
    else:
        legends.append( plt.plot(targ_epocs_induced, '-', lw = 2, color = 'black', label = 'Probes xxx' )[0] )
        legends.append( plt.plot(nontarg_epocs_induced, '--', lw = 2, color = 'black', label = 'Irrelevants xxx' )[0] )

    #sort and add labels
    handles, labels = ax.get_legend_handles_labels()
    #labels, handles = zip(*sorted(zip(labels, handles), key=lambda t: t[0])) # sort both labels and handles by labels

    #ax.fill_between(ax.lines[0].get_xdata(), targ_epocs_standard, nontarg_epocs_standard, edgecolor ="#585858", hatch="|", facecolor = 'none', linewidth=0.0)

    if grp_to_plt != 'Induced':
        ax.fill_between(ax.lines[0].get_xdata(), targ_epocs_standard, nontarg_epocs_standard, edgecolor ="", hatch="", facecolor = 'none', linewidth=0.0)
    else:
        ax.fill_between(ax.lines[0].get_xdata(), targ_epocs_induced, nontarg_epocs_induced, edgecolor ="", hatch="", facecolor = 'none', linewidth=0.0)

    #set title, legends, labels, ticks
    plt.title("ERP for " + comparison_title + " in " + grp_to_plt + " Group", fontsize= font_large)
    #ax.legend(handles, labels, loc = 'upper left', bbox_to_anchor = (1.0, 0.9), fontsize=font_small )
    plt.ylabel('Amplitude (μV)', fontsize=font_small)
    plt.xlabel('Time (ms)', fontsize=font_small)
    #xrange = np.arange( 0, int(ep_lenght), 512/5) # SHOULD BE ADJUSTED TO -100/0/+100
    #plt.xticks(xrange, [str(int( x / 512 * 1000) ) for x in xrange], fontsize=10)
    plt.xticks([len_ms_100, len_ms_100*5, len_ms_100*9, len_ms_100*15], ["0", "400","800","1400"], fontsize=font_small)
    plt.yticks([0,5,10,15,20], fontsize=font_small)
    ax.set_ylim(-2.5,25)
    ymin, ymax = ax.get_ylim()
    plt.plot((len_ms_100, len_ms_100), (ymin*1.1, ymax*0.95), linestyle='--', color = 'gray', lw=1)
    plt.plot((len_ms_100*5, len_ms_100*5), (ymin*1.1, ymax*0.95), linestyle='--', color = 'gray', lw=0.5)
    plt.plot((len_ms_100*9, len_ms_100*9), (ymin*1.1, ymax*0.95), linestyle='--', color = 'gray', lw=0.5)
    plt.plot((len_ms_100*15, len_ms_100*15), (ymin*1.1, ymax*0.95), linestyle='--', color = 'gray', lw=1)
    plt.tight_layout(h_pad=4.0)
    ###
    if save_group_plot:
        fig = plt.gcf()
    #
    if disp_group_plots:
        plt.show()
    if save_group_plot:
        fig.savefig('group_plot_' + dict_key_use + "_" + grp_to_plt + '.tif', dpi = 600, bbox_inches='tight', transparent=False)
    plt.close()

def butter_lowpass(to_filt):
    cutoff = 10
    order = 4
    nyq = 0.5 * 250
    normal_cutoff = cutoff / nyq
    b, a = butter(order, normal_cutoff, btype='low', analog=False)
    y = lfilter(b, a, to_filt)
    return y

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

###EXECUTE
for __ in range(1):
    main_f()

print("Fin." , str(datetime.now()) )
