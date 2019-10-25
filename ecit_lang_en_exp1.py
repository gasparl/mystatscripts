#!/usr/bin/env python2
# -*- coding: utf-8 -*-
from __future__ import absolute_import, division, print_function, unicode_literals
from psychopy.visual import Window, TextStim, Rect
from psychopy.core import wait, Clock, quit
from psychopy.event import clearEvents, waitKeys, getKeys, Mouse
from psychopy.gui import Dlg
from time import gmtime, strftime
from codecs import open
from random import shuffle, choice, randint
from copy import deepcopy
from psychopy.iohub import launchHubServer
from numpy import mean, std
from datetime import datetime
from itertools import permutations

## for testing
testing = False # True for testing, False for real recording
min_ratio = 0.1 # if testing
###
main_ddline = 1 # sec
isi_min_max = (500, 800)
feed_time = 0.5
instruction_color = '#9999FF'

############ MAIN ITEMS

vocab_items = {
        # actual_fam_irr
    'actual_fam_targ': ["meaningful",  "understood", "proper", "credible", "honest"],
        # actual_fam_probe
    'actual_fam_probe': ["known", "genuine", "recognized", "true"],
        # actual_unfam_irr
    'actual_unfam_irr': ["unknown", "unfamiliar", "wrong","fake","untrue", "meaningless", "improper", "different", "sham"],
        # pseudo_irr
    'pseudo_irr': ["onscaft", "omunodial", "wrute","quike","ontreg", "sieringlest", "implated", "deborent", "chag"]
}

        # 1: actual_fam_probe, actual_fam_irr; actual_fam_probe, pseudo_irr
        # 2: actual_fam_probe, pseudo_irr; actual_fam_probe, actual_fam_irr

for key in vocab_items:
    shuffle(vocab_items[key])

###########################################################################

if testing:
    fullscreen = False
    escape_key = 'escape'
    instr_wait = 0.1
    test_trial_num = 4
else:
    escape_key = 'notallowed'
    instr_wait = 0.5
    fullscreen = True

targetkey = 'i'
nontargetkey = 'e'

# EXECUTE all main functions here
def execute():
    start_input() # prompt to input stuff
    # now initiate stuff
    basic_variables() # basic variables assigned, calculated
    set_screen() # creates psychopy screen and stim objects
    # window opens
    prune() # select items
    create_file() # create output file
    create_item_base() # base of items to be presented
    set_block_info() # sets block text and related infos based on conditions
    maus.setVisible(False) # hide mouse

    next_block() # begin task & run until finished

    final_check()

    print("************** LexTALE starts **************")

    lex_screen()
    lex_file()
    run_lextale()

    print("************** END OF EXPERIMENT **************")

    ending() # saves demographic & final infos, gives feedback

    waitKeys(keyList = ['b']) # press B to end the exp (prevents subject from closing window)
    quit()

def ending():
    full_duration = round( ( datetime.now() - start_date ).total_seconds()/60, 2)

    dcit = '-'
    if len(all_main_rts['probe']) > 1 and len(all_main_rts['irrelevant']) > 1:
        dcit = (mean(all_main_rts['probe']) - mean(all_main_rts['irrelevant'])) / std(all_main_rts['irrelevant'])

    info = 'Danke für die Teilnahme. Wenn Sie möchten, können Sie gehen, aber bitte seien Sie leise dabei.\n\nFür Informationen wenden Sie sich bitte an den Versuchsleiter (oder schreiben Sie eine E-mail an Gaspar Lukacs).'
    w_selected_list = [ gues[0] for gues in list( w_selected.values() ) ]
    lextale_score = (lextale_data['corr_word'] / 40 * 100 + lextale_data['corr_nonword'] / 20 * 100) / 2;
    data_out.write(dems + "/" +
      "/".join( [ str(nmbr) for nmbr in
      [practice_repeated['block1'],
      practice_repeated['block2'],
      practice_repeated['block3'],
      full_duration,
      dcit,
      lextale_score,
      len( set( list( true_probes.values() ) ) & set( w_selected_list ) )
      ] ] ) + "/" +
    "|".join( w_selected_list ) +
      "\n")
    data_out.close()
    show_instruction( info )

def set_screen(): # screen properties
    global win, start_text, left_label, right_label, center_disp, instruction_page, maus
    win = Window([1280, 1000], color='Black', fullscr = fullscreen, screen = 0, units = 'pix', allowGUI = True) # 1280 1024
    start_text = TextStim(win, color=instruction_color, font='Verdana', text = u'Um anzufangen, bitte die Leertaste drücken.', pos = [0,-300], height=35, bold = True, wrapWidth= 1100)
    left_label = TextStim(win, color='white', font='Verdana', text = 'unvertraut', pos = [-350,-160], height=35, alignHoriz='center')
    right_label = TextStim(win, color='white', font='Verdana', text = 'vertraut', pos = [350,-160], height=35, alignHoriz='center')
    center_disp = TextStim(win, color='white', font='Arial', text = '', height = 45)
    instruction_page = TextStim(win, wrapWidth = 1200, height = 28, font='Verdana', color = instruction_color)
    maus = Mouse(win = win)

def task_instructions( whichtext = ''):
    keys_info = 'Während des Tests sehen Sie Wörter in der Mitte des Bildschirms auftauchen. Sie müssen jedes Wort entweder mit der linken "E" oder mit der rechten "I" Antworttaste kategorisieren. '
    inducer_info = 'Kategorisieren Sie Ausdrücke, die sich auf Vertrautheit beziehen, mit der rechten Taste. Diese Ausdrücke sind: ' + ', '.join(targetref_words).upper() + '.\n\nKategorisieren Sie Ausdrücke, die sich auf Unvertrautheit beziehen, mit der linken Taste. Diese Ausdrücke sind: ' + ', '.join(nontargref_words).upper() + '.'
    main_item_info = ' Kategorisieren Sie die folgenden Wörter als Vertraut mit der rechten Taste: ' + ', '.join(the_targets).upper() + "\n\nKategorisieren Sie alle anderen Wörter als Unvertraut mit der linken Taste. (Diese anderen Wörter sind: " + ', '.join(the_main_items).upper() + ".)"
    if whichtext == 'targetcheck':
        return 'Super, Sie haben die erste Übungsrunde geschafft. Jetzt werden Wörter dazu kommen, die Sie ebenso kategorisieren müssen. ' + main_item_info
    elif whichtext == 'firstblock':
        return keys_info + '\n\nEs werden drei kurze Übungsrunden stattfinden. In der ersten Runde müssen Sie Ausdrücke kategorisieren, die mit Vertrautheit/Unvertrautheit zu tun haben. '  + inducer_info
    elif block_num > 1:
        return  keys_info + inducer_info + '\n\nDie restlichen Items sind Englische Wörter oder Pseudowörter.' + main_item_info + '\n\nHinweis: achten Sie nur auf die Begriffe die mit der rechten Taste kategorisiert werden müssen (' + ', '.join(the_targets + list(targetref_words)).upper() + ') und drücken Sie für alles andere die linke Taste.'
    else:
        return  keys_info + inducer_info

def set_block_info():
    global block_info, block_num, incorrect, tooslow, move_on
    move_on = '\n\nUm weiterzugehen, drücken Sie die Leertaste.\n\nFalls nötig, drücken Sie die Taste ENTER (oder eine der Pfeiltasten) um die vollständigen Anweisungen erneut zu lesen.'
    block_info = [""]
    target_reminder = [
        'müssen Sie, zusätzlich zu den Begriffen "VERTRAUT", "BEDEUTSAM", "WAHR", nur ein Englisches Wort als Vertraut kategorisieren: "' +
        blcks_base[0][1]['word'].upper() +
        '". ',
        'müssen Sie, zusätzlich zu den Begriffen "VERTRAUT", "BEDEUTSAM", "WAHR", nur ein Englisches Wort als Vertraut kategorisieren: "' +
        blcks_base[1][1]['word'].upper() +
        '". ',
        'müssen Sie, zusätzlich zu den Begriffen "VERTRAUT", "BEDEUTSAM", "WAHR", nur ein Englisches Wort als Vertraut kategorisieren: "' +
        blcks_base[2][1]['word'].upper() +
        '". ',
        'müssen Sie, zusätzlich zu den Begriffen "VERTRAUT", "BEDEUTSAM", "WAHR", nur ein Englisches Wort als Vertraut kategorisieren: "' +
        blcks_base[3][1]['word'].upper() +
        '". '
      ]

    block_info.append( task_instructions('firstblock') + '\n\nUm weiterzugehen, drücken Sie die Leertaste.')

    block_info.append('Nun, in dieser zweiten Übungsrunde wollen wir herausfinden, ob Sie die Aufgabe genau verstanden haben. Um sicherzustellen, dass Sie Ihre jeweiligen Antworten richtig kategorisieren, werden Sie für diese Aufgabe genügend Zeit haben. Sie dürfen maximal zwei Fehler machen. Wählen Sie mehr als zweimal eine nicht korrekte Antwort (oder geben keine Antwort für mehr als 10 Sekunden ein), müssen Sie diese Übungsrunde wiederholen.\nDas Üben dauert unter Umständen etwas und die Runden müssen vielleicht mehrere Male wiederholt werden. Das ist absolut normal - bitte haben Sie Geduld.' + move_on)

    block_info.append('Sie haben die zweite Übungsrunde geschafft. Nun folgt die dritte und letzte Übungsrunde. In dieser dritten Übungsrunde wird die Antwortzeit verkürzt sein. Eine bestimmte Anzahl an falschen Antworten ist aber erlaubt. Die Wörter am unteren Bildschirmrand ("Unvertraut", "Vertraut") werden nicht mehr angezeigt, die Aufgabe bleibt jedoch dieselbe. \n\nVersuchen Sie, so genau und schnell wie möglich zu antworten.' + move_on)

    block_info.append("Gut gemacht. Nun beginnt der eigentliche Test. Die Aufgabe bleibt dieselbe. Es wird vier Blöcke, getrennt durch Pausen, geben. Im ersten Block, " +
      target_reminder[0] +
      "\n\nVersuchen Sie, so genau und schnell wie möglich zu antworten.\n" + move_on)

    block_info.append(
      "Der erste Block ist nun beendet. Im zweiten Block, " +
      target_reminder[1] +
      "Abgesehen davon bleibt die Aufgabe dieselbe.\n\nVersuchen Sie, so genau und schnell wie möglich zu antworten." + move_on)

    block_info.append(
      "Im dritten Block, " +
      target_reminder[2] +
      "\n\nVersuchen Sie, so genau und schnell wie möglich zu antworten.\n" + move_on)

    block_info.append(
      "In diesem letzten Block, " +
      target_reminder[3] +
      " \n\nVersuchen Sie, so genau und schnell wie möglich zu antworten." + move_on)

def start_input():
    global subj_id, dems, condition, gender, categories, true_probes, true_forename, true_surname, start_date, targetref_words, nontargref_words
    input_box = Dlg(title=u'Grunddaten', labelButtonOK=u'OK', labelButtonCancel=u'Abbrechen')
    input_box.addText(text=u'')
    input_box.addField(label=u'c.', tip = '1-2')
    input_box.addField(label=u'VP', tip = 'Ziffern')
    input_box.addText(text=u'')
    input_box.addText(text=u'Bitte ausfüllen:')
    input_box.addField(label=u'Geschlecht', initial = '', choices=[u'männlich',u'weiblich'] )
    input_box.addField(label=u'Alter', tip = 'Ziffern')
    input_box.addField(label=u'Herkunftsland', initial = '', choices=[u'Österreich',u'Deutschland',u'Schweiz'] )
    input_box.addField(label=u'Händigkeit', initial = '', choices=[u'rechtshändig',u'linkshändig'], tip = '(bevorzugte Hand zum Schreiben)' )
    input_box.addText(text=u'')
    input_box.show()
    if input_box.OK:
        stop = False
        try:
            condition = int(input_box.data[0])
        except ValueError:
            condition = 99
            print("Condition must be a one-digit number!")
        # check if variables correctly given
        if condition not in [1,2]: # range(1,9):
            if testing:
                condition = 1 # set value for testing to skip Dlg input box
                print("condition was not set, now set to " + str(condition) + " for testing.")
            else:
                print("condition was not set correctly (should be 1 or 2)")
                stop = True
        try:
            subj_num = int(input_box.data[1])
        except ValueError:
            if testing:
                subj_num = 99 # set value for testing to skip Dlg input box
                print("subj_num was not set, now set to " + str(subj_num) + " for testing.")
            else:
                print("vp (subject number) was not set correctly (should be simple number)")
                stop = True
        try:
            age = int(input_box.data[3])
        except ValueError:
            if testing:
                age = 11 # set value for testing to skip Dlg input box
                print("age was not set, now set to " + str(age) + " for testing.")
            else:
                print("age was not set correctly (should be simple number)")
                stop = True
        if stop:
            print("\nTry again with correct inputs.\n")
            quit()
        subj_id = str(subj_num).zfill(3) + "_" + str(strftime("%Y%m%d%H%M%S", gmtime()))
        if input_box.data[2] == 'weiblich':
            gender = 2
        else:
            gender = 1
        dems = 'dems/gender/age/country/hand/reps1/rep2/rep3/drtn/dcit/lextale/correct/w1234' + '\t' + str(gender) + '/' + str(age)  + '/' + input_box.data[4]  + '/' + input_box.data[5]

        # 1: actual_fam_probe, actual_fam_irr; actual_fam_probe, pseudo_irr
        # 2: actual_fam_probe, pseudo_irr; actual_fam_probe, actual_fam_irr

        if condition == 1:
            categories = ['unfam_irr1', 'unfam_irr2', 'pseudo_irr1', 'pseudo_irr2']
        else:
            categories = ['pseudo_irr1', 'pseudo_irr2', 'unfam_irr1', 'unfam_irr2']

        targetref_words = ('VERTRAUT', 'BEDEUTSAM', 'WAHR')
        nontargref_words = ('UNVERTRAUT', 'GEFÄLSCHT', 'UNBEKANNT', 'ANDERE', 'SONSTIGES', 'UNBEDEUTEND')

        probes_list = vocab_items['actual_fam_probe']
        shuffle(probes_list)
        true_probes = {}
        for cat in categories:
            true_probes[cat] = probes_list.pop()
        start_date = datetime.now()
    else:
        quit()

def prune():
    global items_to_filt

    item_base_temp = {}
    all_finals = []
    containers = []
    all_targs = deepcopy(vocab_items['actual_fam_targ'])
    shuffle(all_targs)
    if condition == 1:
        containers.append( deepcopy(vocab_items['actual_unfam_irr']) )
        containers.append( deepcopy(vocab_items['actual_unfam_irr']) )
        containers.append( deepcopy(vocab_items['pseudo_irr']) )
        containers.append( deepcopy(vocab_items['pseudo_irr']) )
    elif condition == 2:
        containers.append( deepcopy(vocab_items['pseudo_irr']) )
        containers.append( deepcopy(vocab_items['pseudo_irr']) )
        containers.append( deepcopy(vocab_items['actual_unfam_irr']) )
        containers.append( deepcopy(vocab_items['actual_unfam_irr']) )
    else:
        print('problem with conditions (in prune)')
    for cat_ind, categ in enumerate(categories):
        probe = true_probes[categ]
        container = [it for it in containers[cat_ind] if it not in all_finals ]
        final6 = [probe]
        maxdif = 0
        container = [ elm for elm in container if elm[0] != probe[0] ]

        while len(final6) < 5 and maxdif < 99:
            temps = [ elm for elm in container if abs(len(probe)-len(elm)) <= maxdif ]
            if len(temps) > 0:
                final6.append( choice(temps) )
                container = [ elm for elm in container if elm[0] != final6[-1][0] ]
            else:
                maxdif += 1

        probs_lower = [ pr.lower() for pr in true_probes.values() ]
        cont2 = [it for it in all_targs if (it not in all_finals) and (it.lower() not in probs_lower )]

        maxdif2 = 0
        while len(final6) < 6 and maxdif2 < 99:
            temps = [ elm for elm in cont2 if abs(len(probe)-len(elm)) <= maxdif2 ]
            if len(temps) > 0:
                final6.insert( 1, temps[0] )
                break
            else:
                maxdif2 += 1

        all_finals += final6
        item_base_temp[categ] = final6[:]
    items_to_filt = item_base_temp

noneword = 'ich weiß nicht'
def final_check():
    global w_selected

    select_instr = 'Die Lügendetektion ist jetzt beendet. In jeder der folgenden vier Listen gibt es ein einziges englisches Wort, das ein Synonym für Vertrautheit oder Korrektheit ist - im Gegensatz zu den anderen Wörtern in der Liste. (Die anderen Wörter sind entweder Pseudowörter oder beziehen sich auf Fremdheit oder Falschheit). Bitte wählen Sie das Wort aus, das sich auf Korrektheit/Vertrautheit bezieht. Wenn Sie es nicht wissen, wählen Sie "Ich weiß nicht". Sie können wählen (oder die Wahl aufheben), indem Sie auf die Wörter klicken.'
    maus.setVisible(True)
    selection_instr = TextStim(win, text = select_instr, color=instruction_color, pos=(0,230), wrapWidth=1150, height = 30)
    ok_button = TextStim(win, text = 'OK', bold = True, color=instruction_color, pos=(400,-290), height = 30)
    textstim_dicts = {}
    w_counts = {}
    w_selected = {}
    none_clicked = {}
    wdth = -400
    ts_lower = [t.lower() for t in the_targets]
    for categ in categories:
        to_display = [targt for targt in items_to_filt[categ] if targt.lower() not in ts_lower ]
        shuffle(to_display)
        to_display = to_display + [noneword]
        textstim_dicts[categ] = {}
        hght = 50
        for i in range(len(to_display)):
            textstim_dicts[categ][i] = TextStim(win, text = to_display[i].capitalize(), height = 30, color='white', pos = (wdth, hght ))
            hght -= 40
        wdth += 250
        w_counts[categ] = 0
        w_selected[categ] = []
        none_clicked[categ] = False
    change = False
    while True:
        for categ in categories:
            for i in range(len(textstim_dicts[categ])):
                current_stim = textstim_dicts[categ][i]
                if maus.isPressedIn(current_stim, buttons=[0]):
                    if current_stim.text == noneword:
                        if current_stim.bold == True:
                            current_stim.bold = False
                            none_clicked[categ] = False
                            change = True
                        elif w_counts[categ] == 0:
                            current_stim.bold = True
                            none_clicked[categ] = True
                            change = True
                    elif none_clicked[categ] == False:
                        if current_stim.bold == True:
                            current_stim.bold = False
                            w_counts[categ] -= 1
                            w_selected[categ].remove(current_stim.text)
                            change = True
                        elif w_counts[categ] < 1:
                            current_stim.bold = True
                            w_counts[categ] += 1
                            w_selected[categ].append(current_stim.text)
                            change = True
                current_stim.draw()
        ok_button.draw()
        selection_instr.draw()
        win.flip()
        if maus.isPressedIn( ok_button, buttons = [0] ):
            do_break = True
            for categ in categories:
                if  none_clicked[categ] == False and w_counts[categ] == 0:
                    do_break = False
            if do_break:
                break
            else:
                instruction_page.setText('Bitte wählen Sie ein Element in jeder Kategorie!')
                instruction_page.draw()
                win.flip()
                wait(2)
        if change == True:
            change = False
            wait(0.3)
            clearEvents()
    for categ in categories:
        w_selected[categ] = [ ws.lower() for ws in w_selected[categ] ]


def trm(raw_inp):
    return [w for w in raw_inp.replace(',', ' ').split(' ') if w != ''][:4]
def target_check():
    show_instruction( task_instructions('targetcheck') + '\n\nUm weiterzugehen, drücken Sie die Leertaste.' )

    all_words = the_main_items+the_targets
    all_words.sort()
    t_check_text = u'Bitte geben Sie die Wörter ein (durch Leertaste oder Komma getrennt), die Sie als vertraut kategorisieren müssen und drücken Sie dann ENTER.\n\n(Als Hilfestellung sind hier alle genutzten Wörter aufgelistet: '+ ', '.join(all_words).upper() + '.)'

    instr_display =  TextStim(win, color=instruction_color, font='Verdana', text = t_check_text, pos=(0, 150), height=30, wrapWidth=1100, colorSpace='rgb')
    input_prompt =  TextStim(win, color=instruction_color, font='Verdana', text = '', pos=(-100, 40), alignHoriz = 'right', height=35)
    input_display =  TextStim(win, color='white', pos=(-300, -100), alignHoriz = 'left', height=35, bold = True, colorSpace='rgb')
    typedin = ''
    while True:
         input_display.setText(typedin)
         instr_display.draw()
         input_prompt.draw()
         input_display.draw()
         win.flip()
         char = waitKeys()[0]
         if char == 'backspace' and len(typedin) > 0:
             typedin = typedin[:-1]
         elif char == escape_key:
             break
         elif char == 'return':
             if len( trm(typedin) ) > 0:
                 break
         elif len(char) == 1 and char.isalpha():
             typedin += char.upper()
         elif char == 'space':
             typedin += ' '
         elif char == 'comma':
             typedin += ','
    typedin_words = trm(typedin)
    if not ( char == escape_key or typedin_words[0] == 'PASS' or (the_targets[0] in typedin_words and the_targets[1] in typedin_words and the_targets[2] in typedin_words and the_targets[3] in typedin_words) ):
        instruction_page.setText('Inkorrekt. Lesen Sie bitte die Instruktionen nochmals!')
        instruction_page.draw()
        win.flip()
        wait(2)
        target_check()

def create_item_base():
    global blcks_base, stims_base, targetrefs, nontargrefs, the_targets, the_main_items, task_probes
    stim_base_tmp = {}
    the_targets = []
    task_probes = []
    the_main_items = []
    for categ in categories:
        stim_base_tmp[categ] = []
        words_temp = []

        words_temp = [ itm.upper() for itm in items_to_filt[categ] ]

        for idx, itm in enumerate(words_temp): ## create basic dictionaries for the 6 crucial items, with types and categories
            if idx == 0:
                itmtype = "probe"
                the_main_items.append(itm)
                task_probes.append(itm)
            elif idx == 1:
                itmtype = "target"
                the_targets.append(itm)
            else:
                itmtype = "irrelevant" + str(idx-1)
                the_main_items.append(itm)
            stim_base_tmp[categ].append({'word': itm, 'item_type': itmtype, 'categ': categ })
    stims_base = deepcopy(stim_base_tmp)
    the_main_items.sort()
    blcks_base = []
    for cat in categories:
        blcks_base.append( deepcopy( stim_base_tmp[cat] ) )

    targetrefs = []
    nontargrefs = []
    for ref_word in targetref_words:
        targetrefs.append({'word': ref_word, 'item_type': 'targetref', 'categ': 'inducer' })
    for ref_word in nontargref_words:
        nontargrefs.append({'word': ref_word, 'item_type': 'nontargref', 'categ': 'inducer' })

def main_items():
    global blcks_base, crrnt_phase
    print('main_items()')
    crrnt_phase = 'main'
    block_stim_base = blcks_base.pop(0)
    main_stims = add_inducers(block_stim_base)
    stim_dicts_f = [dct for sublist in main_stims for dct in sublist] # flatten
    if testing == True:
        stim_dicts_f = stim_dicts_f[0:test_trial_num]
    return stim_dicts_f

def rndmz_details(block_stim_base):
    item_order=[]
    prev_last = '' # prev order is the item order of the previous trial sequence
    for i in range(0,18):# each i represents a sequence of 6 trials
        item_order_temp = deepcopy(block_stim_base) # create a temporary item order, this represents the item order WITHIN one trial sequence
        shuffle(item_order_temp) # shuffle this
        while prev_last == item_order_temp[0]: # if the last one of the previous block is the first of this one
            shuffle(item_order_temp) # reshuffle
        item_order.append(deepcopy(item_order_temp)) # make this the item order for this trial sequence
        prev_last = item_order_temp[-1]
    return item_order

def add_inducers(block_stim_base):
    word_assignment = {}
    # First we want to assign which words get an inducer. We want each word to get an inducer in half (9) of the trials. In addition, we want half of the words in one trial sequence (3) to have an inducer. Thus we make 9 permumtations of yes/no.
    yesno_perm = list(set(permutations('yyynnn')))
    shuffle(yesno_perm)
    options = yesno_perm[:9]
    blck_rev = []     # create an empty list for the reversed block
    for opt in options: # then we loop through to create the reverse
        optz_new = list(range(6))
        for index, item in enumerate(opt):
            if (item == "n"):
                optz_new[index] = "y"
            else:
                optz_new[index] = "n"
        blck_rev.append(deepcopy(optz_new)) # everytime add the current reversed line to the reversed block
    blck1 = options[0:3] + blck_rev[0:3] # because we wanna split them up in 3 we create blcks of 6 which are each  time 3 lines and then the reverse of those 3 lines
    blck2 = options[3:6] + blck_rev[3:6]
    blck3 = options[6:9] + blck_rev[6:9]
    shuffle(blck1)
    shuffle(blck2)
    shuffle(blck3)
    #create final block
    blck_fin = blck1 + blck2 + blck3
    for indx, dct in enumerate(block_stim_base): #assign the yes/nos to the words
        word_assignment[ dct['word'] ] = [opt[indx] for opt in blck_fin] # combine them to create an inducer assignment for all 18 trial sequences and assign them to the dict
    #  We then need to decide which inducer is shown thus we make a list
    inducer_lists = inducer_randomized() # randomize 6 lists of inducer words
    inducer_per_main = {}
    for dct in block_stim_base:
            inducer_per_main[ dct['word'] ] = inducer_lists.pop()
    # now insert the inducers
    final_item_order = []
    for t_indx, trial_seq in enumerate(rndmz_details(block_stim_base)): # trial sequence represents the order in which the x amount of words are presented within one sequence (n=6) of trials
        final_temp = []
        for i_indx, item in enumerate(trial_seq): # item represents each individual word (or trial)
            if word_assignment[item["word"]][t_indx] == "y": # check if the word should get an inducer
                inducer_pick= inducer_per_main[item["word"]].pop(0) # pick the right inducer
            # then we should delete this element so inducer so we use pop
                final_temp.append(inducer_pick) # append the inducer to our item order
            final_temp.append(item) # append the item to our item order
        final_item_order.append(deepcopy(final_temp)) # create final item order list
    return final_item_order

def inducer_randomized(): # 6 possible inducer orders
    targetrefs_perm = list(permutations(targetrefs)) # 3 x 2 = 6 arrangements
    shuffle(targetrefs_perm)
    nontarg_temp = deepcopy(nontargrefs)
    shuffle(nontarg_temp)
    nontargrefs_perm1 = list(permutations(nontarg_temp[:3])) # 3 x 2 = 6
    nontargrefs_perm2 = list(permutations(nontarg_temp[3:])) # 3 x 2 = 6
    nontargrefs_perm = []
    for i in range(3): # 6/2 = 3
        nontargrefs_perm.append(nontargrefs_perm1.pop(0)+nontargrefs_perm2.pop(0))
        nontargrefs_perm.append(nontargrefs_perm2.pop(0)+nontargrefs_perm1.pop(0))
    shuffle(nontargrefs_perm)
    inducer_lists = []
    for trefs, ntrefs in zip(targetrefs_perm, nontargrefs_perm):
        trefs = list(trefs)
        ntrefs = list(ntrefs)
        lst_temp = ntrefs
        nums = list(range(len(trefs+ntrefs)))
        insert_locs = []
        for i in range(len(trefs)): # tref never repeats successively
            new_rand = choice(nums)
            insert_locs.append(new_rand)
            nums = [n for n in nums if abs(n-new_rand) > 1]
        for loc in sorted(insert_locs): # trefs to the 3 locs
            lst_temp.insert( loc, trefs.pop() )
        inducer_lists.append(deepcopy(lst_temp))
    return inducer_lists

def inducer_items():
    print('inducer_items()')
    blck_itms_temp = deepcopy(targetrefs + nontargrefs + targetrefs + nontargrefs) # inducers x2
    shuffle(blck_itms_temp) # shuffle it, why not
    safecount = 0 # just to not freeze the app if sth goes wrong
    stim_dicts_f = [] # in here the final list of dictionary items is collected, one by one
    while len(blck_itms_temp) > 0: # stop if all items from blck_itms_temp were use up
        dict_item = blck_itms_temp[0]
        safecount += 1
        if safecount > 911:
            print('break due to unfeasable safecount')
            break
        good_indexes = [] # will collect the indexes where the dict item may be inserted
        dummy_dict = [{ 'word': '-', 'item_type': '-' }] # dummy dict to the end
        for f_index, f_item in enumerate(stim_dicts_f + dummy_dict):
            if dict_item['word'] in diginto_dict(stim_dicts_f, f_index, 'word', 4):
                continue # if there is, continue without adding the index as good index
            good_indexes.append(f_index) # if did not continue above, do add as good index
        if len(good_indexes) == 0:
            print('no good_indexes - count', safecount)
            shuffle(blck_itms_temp) # reshuffle
        else: # if there are good places, choose one randomly, insert the new item, and remove it from blck_itms_temp
            stim_dicts_f.insert( choice(good_indexes) , blck_itms_temp.pop(0))
    if testing == True:
        stim_dicts_f = stim_dicts_f[0:test_trial_num]
    return stim_dicts_f # return final list (for blck_items var assignment)

def practice_items():
    print('practice_items()')
    blck_itms_temp = deepcopy(targetrefs + nontargrefs) # get inducers
    for cat in categories:
        blck_itms_temp += deepcopy( stims_base[cat] ) # add all other items
    shuffle(blck_itms_temp) # shuffle it, why not
    # below the pseudorandomization to avoid close repetition of similar items (same item type)
    safecount = 0 # just to not freeze the app if sth goes wrong
    stim_dicts_f = [] # in here the final list of dictionary items is collected, one by one
    while len(blck_itms_temp) > 0: # stop if all items from blck_itms_temp were use up (added to stim_dicts_f and removed with pop() )
        dict_item = blck_itms_temp[0] # assign first dictionary item as separate variable; for easier access below
        safecount += 1
        if safecount > 911:
            print('break due to unfeasable safecount')
            break
        good_indexes = [] # will collect the indexes where the dict item may be inserted
        dummy_dict = [{ 'word': '-', 'item_type': '-' }] # dummy dict to the end; if the item is to be inserted to the end, there is no following dict that could cause an unwanted repetition
        for f_index, f_item in enumerate(stim_dicts_f + dummy_dict): # check all potential indexes for insertion in the stim_dicts_f as it is so far (plus 1 place at the end)
            if dict_item['item_type'] in diginto_dict(stim_dicts_f, f_index, 'item_type', 1): # checks whether there is preceding or following identical item_type around the potential index (see diginto_dict function)
                continue # if there is, continue without adding the index as good index
            good_indexes.append(f_index) # if did not continue above, do add as good index
        if len(good_indexes) == 0: # if by chance no good indexes found, print notification and reshuffle the items
            print('no good_indexes - count', safecount) # this should normally happen max couple of times
            blck_itms_temp.insert( len(blck_itms_temp), blck_itms_temp.pop(0) ) # move first element to last, and let's hope next first item is luckier and has place
        else: # if there are good places, choose one randomly, insert the new item, and remove it from blck_itms_temp
            stim_dicts_f.insert( choice(good_indexes) , blck_itms_temp.pop(0))
    if testing == True:
        stim_dicts_f = stim_dicts_f[0:test_trial_num]
    return stim_dicts_f # return final list (for blck_items var assignment)

def diginto_dict(dct, indx, key_name, min_dstnc):
    if indx - min_dstnc < 0: # if starting index is negative, it counts from the end of the list; thats no good
        strt = 0 # so if negative, we just set it to 0
    else:
        strt = indx - min_dstnc # if not negative, it can remain the same
    return [ d[key_name] for d in dct[ strt : indx+min_dstnc ] ] # return all values for the specified dict key within the specified distance (from the specified dictionary)


def basic_variables():
    global stopwatch, guilt, block_num, all_main_rts, kb_device, practice_repeated, firsttime, lextale_data
    stopwatch = Clock()
    guilt = 1 # always guilty
    block_num = 0
    all_main_rts = { 'probe' : [], 'irrelevant': [] }
    practice_repeated = { 'block1' : 0, 'block2': 0, 'block3': 0 }
    firsttime = True
    io = launchHubServer()
    kb_device = io.devices.keyboard
    lextale_data = {'corr_nonword': 0, 'corr_word': 0}

# create output file, begin writing, reset parameters
def create_file():
    global data_out
    f_name = 'exp_ecit_lang_' + str(condition) + "_" + subj_id + '.txt'
    data_out=open(f_name, 'a', encoding='utf-8')
    data_out.write( '\t'.join( [ "subject_id", "condition", "phase", "block_number", "trial_number", "stimulus_shown", "category", "stim_type", "response_key", "rt_start", "incorrect", "too_slow", "press_duration", "isi", "date_in_ms" ] ) + "\n" )
    print("File created:", f_name)

def str_if_num( num_val ):
    if isinstance(num_val, str) or isinstance(num_val, unicode):
        return num_val
    else:
        return str( num_val*1000 )

def add_resp():
    global incorrect, tooslow
    data_out.write( '\t'.join( [ subj_id, str(condition), crrnt_phase, str(block_num), str(trial_num+1), stim_text, stim_current["categ"], stim_type, resp_key, str_if_num(rt_start), str(incorrect), str(tooslow), str_if_num(press_dur), str_if_num( isi_min_max[0]/1000 + isi_delay ), str(strftime("%Y%m%d%H%M%S", gmtime())) ] ) + '\n' )
    print("resp key:", resp_key, "for stim:", stim_text, "incorrect:", incorrect, "rt_start:", rt_start)

def start_with_space():
    start_text.draw() # start with space
    center_disp.setText("+")
    center_disp.draw()
    draw_labels()
    win.flip()
    inst_resp = waitKeys(keyList = ['space',escape_key])
    end_on_esc(inst_resp[0])
    draw_labels()
    win.flip()
    wait(isi_min_max[0]/1000)

def draw_labels():
    if block_num <= 2:
        left_label.draw()
        right_label.draw()

def next_block():
    global ddline, block_num, rt_data_dict, blck_itms, firsttime, crrnt_phase
    if len(blcks_base) > 0:
        crrnt_phase = 'practice'
        if ( block_num in (0, 1, 2, 3) ):
            if block_num == 0 or practice_eval():
                block_num+=1
            rt_data_dict = {}
            if block_num == 1:
                blck_itms = inducer_items()
                ddline = 1.5
            elif block_num == 2:
                if firsttime:
                    target_check() # ensures subject payed attention to target items
                    firsttime = False
                blck_itms = practice_items()
                ddline = 10
            elif block_num == 3:
                blck_itms = practice_items()
                ddline = main_ddline
            else:
                blck_itms = main_items()
        else:
            block_num+=1
            blck_itms = main_items()
        run_block()

def practice_eval():
    global min_ratio, first_wrong_count
    is_valid = True
    if first_wrong == True and testing == False:
        is_valid = False
        first_wrong_count = 0
    elif block_num != 2:
        types_failed = []
        if testing == False:
            if block_num == 1:
                min_ratio = 0.8
            else:
                min_ratio = 0.5
        for it_type in rt_data_dict:
            it_type_feed_dict = { 'targetref': "sich auf Vertrautheit beziehende Ausdrücke",
        'nontargref': "sich auf Unvertrautheit beziehende Ausdrücke",
        'main_item': "als unvertraut zu kategorisierende Wörter",
        'target': "als vertraut zu kategorisierende Wörter" }
            rts_correct = [ rt_item for rt_item in rt_data_dict[it_type] if rt_item > 0.15 ]
            corr_ratio = len( rts_correct )/ len( rt_data_dict[it_type] )
            if corr_ratio < min_ratio:
              is_valid = False
              types_failed.append(
                " " +
                it_type_feed_dict[it_type] +
                " (" + str( int( corr_ratio*100 ) ) +
                "% correct)"
              )
            if is_valid == False:
                block_info[block_num] = "Sie müssen diese Übungsrunde wiederholen, da Sie zu wenige richtige Antworten gegeben haben.\n\nSie benötigen mindestens " + str( int(min_ratio*100) ) + "% richtige Antworten für jeden der Antworttypen, jedoch gaben Sie nicht genügend richtige Antworten für folgende(n) Antworttyp(en):" + ", ".join(types_failed) +  ".\n\nBitte geben Sie genaue und im Zeitlimit liegende Antworten.\n\nMachen Sie sich keine Sorgen, wenn Sie diese Übungsrunde mehrmals wiederholen müssen." + move_on
    if is_valid == False:
        practice_repeated['block' + str(block_num)] += 1
    return is_valid

def show_instruction(instruction_text):
    instruction_page.setText(instruction_text)
    instruction_page.draw()
    win.flip()
    wait(instr_wait)
    inst_resp = waitKeys(keyList = ['space', escape_key])
    end_on_esc(inst_resp[0])

def show_block_instr():
    instruction_page.setText( block_info[block_num] )
    instruction_page.draw()
    win.flip()
    wait(instr_wait)
    show_again = ['left', 'up', 'right', 'down','return']
    inst_resp = waitKeys( keyList = [ 'space', escape_key ] + show_again )
    end_on_esc( inst_resp[0] )
    if inst_resp[0] in show_again:
        show_instruction( task_instructions() + '\n\nUm weiterzugehen, drücken Sie die Leertaste.' )
        show_block_instr()

first_wrong_count = 0
def run_block():
    global block_num, trial_num, stim_current, stim_text, stim_type, incorrect, tooslow, first_wrong, show_feed, ddline, isi_delay, resp_key, rt_start, press_dur, first_wrong_count
    show_block_instr()
    first_wrong = False
    print("len(blck_itms):", len(blck_itms))
    start_with_space()
    for trial_num in range(len(blck_itms)): # go through all stimuli of current block
        print("------- Trial number:", trial_num, "In block:", block_num, "C:", condition )
        stim_current = blck_itms[trial_num]
        incorrect = 0
        tooslow = 0
        stim_type = stim_current["item_type"]
        stim_text = stim_current["word"]
        isi_delay = randint(1, isi_min_max[1]-isi_min_max[0]) / 1000
        wait(isi_delay) # wait ISI
        center_disp.setText(stim_text.upper())
        draw_labels()
        center_disp.draw()
        win.callOnFlip(stopwatch.reset)
        kb_device.clearEvents()
        clearEvents()
        win.flip()
        response = waitKeys(maxWait = ddline, keyList=[targetkey, nontargetkey, escape_key], timeStamped=stopwatch)
        if not response:
            rt_start = stopwatch.getTime()
            resp_key = '-'
            tooslow += 1
            show_tooslow()
        else:
            resp_key = response[0][0]
            rt_start = response[0][1]
            end_on_esc(resp_key)
            if resp_key == targetkey:
                if stim_type in ("target", "targetref"):
                    incorrect = 0
                    tooslow = 0
                else:
                    incorrect += 1
                    show_false()
            elif resp_key == nontargetkey:
                if stim_type[:10] in ("probe","irrelevant", "nontargref"):
                    incorrect = 0
                    tooslow = 0
                else:
                    incorrect += 1
                    show_false()
        draw_labels()
        win.flip()
        wait(isi_min_max[0]/1000)
        press_dur = '-' # remains this if none found, or not with correct key
        for ke in kb_device.getReleases(): # get io keypress events for duration
            try:
                if ke.key == resp_key: # if matches the pygame key, should be fine
                    press_dur = ke.duration # store io keypress duration
            except Exception:
                pass
            break
        add_resp() # store trial data
        if block_num == 2: # check if comprehension check has to be repeated
            if (incorrect+tooslow) > 0:
                first_wrong_count = first_wrong_count + 1
                if first_wrong_count == 3:
                    first_wrong = True
                    break
        else:
            collect_rts()
    next_block()

def collect_rts(): # for practice evaluation & dcit calculation
    global rt_data_dict, all_main_rts, rt_start
    if (incorrect+tooslow) > 0:
        rt_start = -9
    if stim_type[:10] in ("probe","irrelevant"):
        group_type = 'main_item'
    else:
        group_type = stim_type
    if group_type not in rt_data_dict:
        rt_data_dict[group_type] = []
    rt_data_dict[group_type].append(rt_start)
    if block_num in (4,5,7,8) and stim_type[:10] in ("probe","irrelevant") and incorrect != 1 and tooslow != 1 and rt_start > 0.15 and rt_start < main_ddline:
        all_main_rts[ stim_type[:10] ].append(rt_start)

def show_false():
    center_disp.text = 'Inkorrekt!'
    center_disp.color = '#ff1111'
    center_disp.draw()
    draw_labels()
    win.flip()
    wait(feed_time)
    center_disp.color = 'white'
def show_tooslow():
    center_disp.text = 'Zu langsam!'
    center_disp.color = '#ff1111'
    center_disp.draw()
    draw_labels()
    win.flip()
    wait(feed_time)
    center_disp.color = 'white'


# end session
def end_on_esc(escap):
    if escap == escape_key : # escape
        print("Trying to escape?")
        instruction_page.setText('Sure you want to discontinue and quit the experiment?\n\nPress "y" to quit, or press "n" to continue.')
        instruction_page.draw()
        win.flip()
        wait(instr_wait)
        quit_resp = waitKeys(keyList = ['y', 'n'])
        if quit_resp[0] == 'y':
            print("************ ESCAPED ************")
            data_out.close()
            win.close()
            quit()
        else:
            clearEvents()
            print("Continuing...")


# LEXTALE STUFF


def lex_screen(): # screen properties
    global win, start_text, lex_left_lab, lex_right_lab, lex_cent_disp, lex_inst_page, lex_left_bg, lex_right_bg, lex_ok_butt, lex_ok_text
    text_left = 'no'
    text_right = 'yes'
    h_dist = 80
    v_dist = -190
    lex_left_bg = Rect(win, fillColor = 'red', pos = [-h_dist,v_dist-7], width = 130, height = 85)
    lex_right_bg = Rect(win, fillColor = 'green', pos = [h_dist,v_dist-7], width = 130, height = 85)
    lex_left_lab = TextStim(win, color='white', font='Verdana', text = text_left, pos = [-h_dist,v_dist], height=50, alignHoriz='center')
    lex_right_lab = TextStim(win, color='white', font='Verdana', text = text_right, pos = [h_dist,v_dist], height=50, alignHoriz='center')
    lex_cent_disp = TextStim(win, color='white', font='Arial', text = '', height = 55)
    lex_inst_page = TextStim(win, wrapWidth = 1200, pos = [0, 35], height = 28, font='Verdana', color = instruction_color)
    lex_ok_butt = Rect(win, fillColor = 'black', lineColor = instruction_color, pos = [0,-345], width = 80, height = 50)
    lex_ok_text = TextStim(win, text = 'OK', bold = True, color=instruction_color, pos=(0,-345), height = 30)

def lex_file():
    global lex_dat_out
    f_name = 'lextale_en_results_' + subj_id + '.txt'
    lex_dat_out=open(f_name, 'a', encoding='utf-8')
    lex_dat_out.write( '\t'.join( [ "subject_id", "trial_number", "stimulus_shown", "dummy", "real", "response_key", "rt_start", "incorrect", "date_in_ms" ] ) + "\n" )
    print("File created:", f_name)

def add_lexresp():
    lex_dat_out.write( '\t'.join( [ subj_id, str(trial_num+1), stim_text, str(stim_type), str(stim_status), response, str(rt_start*1000), str(incorrect), str(strftime("%Y%m%d%H%M%S", gmtime())) ] ) + '\n' )
    print("resp key:", response, "for stim:", stim_text, "incorrect:", incorrect, "rt_start:", rt_start)

def draw_lexlabels():
    lex_left_bg.draw()
    lex_right_bg.draw()
    lex_left_lab.draw()
    lex_right_lab.draw()

def run_lextale():
    global trial_num, stim_text, stim_type, stim_status, incorrect, response, rt_start
    print("len(blck_itms):", len(lextale_items))
    maus.setVisible(True)
    lex_inst_page.setText(lextale_instructions)
    lex_inst_page.draw()
    lex_ok_butt.draw()
    lex_ok_text.draw()
    win.flip()
    while not maus.isPressedIn( lex_ok_butt, buttons = [0] ):
        pass
    stopwatch = Clock()
    for trial_num in range(len(lextale_items)): # go through all stimuli of current block
        print("------- Trial number:", trial_num )
        stim_current = lextale_items[trial_num]
        stim_type = stim_current["dummy"]
        stim_text = stim_current["word"]
        stim_status = stim_current["wstatus"]

        lex_cent_disp.setText(stim_text)
        draw_lexlabels()
        lex_cent_disp.draw()
        win.callOnFlip(stopwatch.reset)
        clearEvents()
        win.flip()
        while True:
            if maus.isPressedIn( lex_right_bg, buttons = [0] ):
                rt_start = stopwatch.getTime()
                response = 'yes'
                lex_right_bg.fillColor = "darkgreen"
                draw_lexlabels()
                lex_cent_disp.draw()
                win.flip()
                while maus.isPressedIn( lex_right_bg, buttons = [0] ):
                    pass
                lex_right_bg.fillColor = "green"
                if stim_status == 1:
                    incorrect = 0
                    if stim_type == 0:
                        lextale_data['corr_word'] = lextale_data['corr_word'] + 1
                else:
                    incorrect = 1
                break
            elif maus.isPressedIn( lex_left_bg, buttons = [0] ):
                rt_start = stopwatch.getTime()
                response = 'no'
                lex_left_bg.fillColor = "darkred"
                draw_lexlabels()
                lex_cent_disp.draw()
                win.flip()
                while maus.isPressedIn( lex_left_bg, buttons = [0] ):
                    pass
                lex_left_bg.fillColor = "red"
                if stim_status == 0:
                    incorrect = 0
                    if stim_type == 0:
                        lextale_data['corr_nonword'] = lextale_data['corr_nonword'] + 1
                else:
                    incorrect = 1
                break
            elif len(getKeys(keyList=[escape_key], timeStamped=stopwatch)) > 0:
                end_on_esc(escape_key)
                draw_lexlabels()
                lex_cent_disp.draw()
                win.flip()
        add_lexresp() # store trial data

lextale_instructions = 'Now comes the short English test, the final part of this experiment. This test consists of about 60 trials, in each of which you will see a string of letters. Your task is to decide whether this is an existing English word or not.  If you think it is an existing English word, you click on "yes", and if you think it is not an existing English word, you click on "no".\n\nIf you are sure that the word exists, even though you don’t know its exact meaning, you may still respond "yes". But if you are not sure if it is an existing word, you should respond "no". \n\nIn this experiment, we use British English rather than American English spelling. For example: "realise" instead of "realize"; "colour" instead of "color", and so on. Please don’t let this confuse you. This experiment is not about detecting such subtle spelling differences anyway. \n\nYou have as much time as you like for each decision. This part of the experiment will take about 5 minutes.\n\nIf everything is clear, you can now start the experiment.'

lextale_items = [{'word': 'platery', 'wstatus': 0, 'dummy': 1},
    {'word': 'denial', 'wstatus': 1, 'dummy': 1},
    {'word': 'generic', 'wstatus': 1, 'dummy': 1},
    {'word': 'mensible', 'wstatus': 0, 'dummy': 0},
    {'word': 'scornful', 'wstatus': 1, 'dummy': 0},
    {'word': 'stoutly', 'wstatus': 1, 'dummy': 0},
    {'word': 'ablaze', 'wstatus': 1, 'dummy': 0},
    {'word': 'kermshaw', 'wstatus': 0, 'dummy': 0},
    {'word': 'moonlit', 'wstatus': 1, 'dummy': 0},
    {'word': 'lofty', 'wstatus': 1, 'dummy': 0},
    {'word': 'hurricane', 'wstatus': 1, 'dummy': 0},
    {'word': 'flaw', 'wstatus': 1, 'dummy': 0},
    {'word': 'alberation', 'wstatus': 0, 'dummy': 0},
    {'word': 'unkempt', 'wstatus': 1, 'dummy': 0},
    {'word': 'breeding', 'wstatus': 1, 'dummy': 0},
    {'word': 'festivity', 'wstatus': 1, 'dummy': 0},
    {'word': 'screech', 'wstatus': 1, 'dummy': 0},
    {'word': 'savoury', 'wstatus': 1, 'dummy': 0},
    {'word': 'plaudate', 'wstatus': 0, 'dummy': 0},
    {'word': 'shin', 'wstatus': 1, 'dummy': 0},
    {'word': 'fluid', 'wstatus': 1, 'dummy': 0},
    {'word': 'spaunch', 'wstatus': 0, 'dummy': 0},
    {'word': 'allied', 'wstatus': 1, 'dummy': 0},
    {'word': 'slain', 'wstatus': 1, 'dummy': 0},
    {'word': 'recipient', 'wstatus': 1, 'dummy': 0},
    {'word': 'exprate', 'wstatus': 0, 'dummy': 0},
    {'word': 'eloquence', 'wstatus': 1, 'dummy': 0},
    {'word': 'cleanliness', 'wstatus': 1, 'dummy': 0},
    {'word': 'dispatch', 'wstatus': 1, 'dummy': 0},
    {'word': 'rebondicate', 'wstatus': 0, 'dummy': 0},
    {'word': 'ingenious', 'wstatus': 1, 'dummy': 0},
    {'word': 'bewitch', 'wstatus': 1, 'dummy': 0},
    {'word': 'skave', 'wstatus': 0, 'dummy': 0},
    {'word': 'plaintively', 'wstatus': 1, 'dummy': 0},
    {'word': 'kilp', 'wstatus': 0, 'dummy': 0},
    {'word': 'interfate', 'wstatus': 0, 'dummy': 0},
    {'word': 'hasty', 'wstatus': 1, 'dummy': 0},
    {'word': 'lengthy', 'wstatus': 1, 'dummy': 0},
    {'word': 'fray', 'wstatus': 1, 'dummy': 0},
    {'word': 'crumper', 'wstatus': 0, 'dummy': 0},
    {'word': 'upkeep', 'wstatus': 1, 'dummy': 0},
    {'word': 'majestic', 'wstatus': 1, 'dummy': 0},
    {'word': 'magrity', 'wstatus': 0, 'dummy': 0},
    {'word': 'nourishment', 'wstatus': 1, 'dummy': 0},
    {'word': 'abergy', 'wstatus': 0, 'dummy': 0},
    {'word': 'proom', 'wstatus': 0, 'dummy': 0},
    {'word': 'turmoil', 'wstatus': 1, 'dummy': 0},
    {'word': 'carbohydrate', 'wstatus': 1, 'dummy': 0},
    {'word': 'scholar', 'wstatus': 1, 'dummy': 0},
    {'word': 'turtle', 'wstatus': 1, 'dummy': 0},
    {'word': 'fellick', 'wstatus': 0, 'dummy': 0},
    {'word': 'destription', 'wstatus': 0, 'dummy': 0},
    {'word': 'cylinder', 'wstatus': 1, 'dummy': 0},
    {'word': 'censorship', 'wstatus': 1, 'dummy': 0},
    {'word': 'celestial', 'wstatus': 1, 'dummy': 0},
    {'word': 'rascal', 'wstatus': 1, 'dummy': 0},
    {'word': 'purrage', 'wstatus': 0, 'dummy': 0},
    {'word': 'pulsh', 'wstatus': 0, 'dummy': 0},
    {'word': 'muddy', 'wstatus': 1, 'dummy': 0},
    {'word': 'quirty', 'wstatus': 0, 'dummy': 0},
    {'word': 'pudour', 'wstatus': 0, 'dummy': 0},
    {'word': 'listless', 'wstatus': 1, 'dummy': 0},
    {'word': 'wrought', 'wstatus': 1, 'dummy': 0}]

# EXECUTE
execute()
