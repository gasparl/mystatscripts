/*jshint esversion: 6 */

// texts to display before blocks

var block_texts = [];
var first_categ;
var ins = "iNk9";

function lst(arr) {
    return '<b>' + arr.join("</b>, <b>").toUpperCase() + '</b>';
}

function set_cit_type() {
    window.inducers_instructions =
        'You have to categorize each item that appears during the test by pressing the key "E" on the left or the key "I" on the right.</br></br>Press the right ("I") key, when you see the following items: <b>' +
        targetref_words.sort().join("</b>, <b>").toUpperCase() +
        '</b>. Press the left ("E") key, when you see the following items: <b>' +
        nontargref_words.sort().join("</b>, <b>").toUpperCase() +
        '</b>.';
    $("#task_instruction").html(
        inducers_instructions +
        '</br></br>Press the right ("I") key when you see the following target country: <b>' +
        the_targets.sort().join("</b>, <b>").toUpperCase() +
        '.</b><br>Press the left ("E") key to all other countries (<b>' +
        the_nontargs.sort().join("</b>, <b>").toUpperCase() +
        '</b>).'

    );
    $("#targs_names").html('<b>' + the_targets.sort().join("</b>, <b>").toUpperCase() + '</b>');
    $("#nontargs_names").html('<b>' + the_nontargs.sort().join("</b>, <b>").toUpperCase() + '</b>');
}

function set_block_texts() {
    var country_for_disp = capitalize(the_probes[0]);
    countrs_orig.forEach(function(countr, index) {
        if (countr.toLowerCase() == the_probes[0]) {
            country_for_disp = countr;
        }
    });
    let fillerinfo = [];

    let neutfilltext = '</br></br>Press the right ("I") key, when you see the following items: ' +
        lst(neut_target) +
        '. Press the left ("E") key, when you see the following items: ' +
        lst(neut_nontarg) +
        '.<br><br>';
    let numfilltext = '</br></br>Press the right ("I") key, when you see the following items: ' +
        lst(nums_target) +
        '. Press the left ("E") key, when you see the following items: ' +
        lst(nums_nontarg) +
        '.<br><br>';

    if (fill_modes[0] === "nofiller") {
        fillerinfo.push('For now, only country names will be in the task. Otherwise, the task is the same.');
        if (fill_modes[1] === "numbers") {
            fillerinfo.push('the secondary items will be again in the task (the items ' + lst(nums_target) + ' and ' + lst(nums_nontarg) + '). ');
            fillerinfo.push('the secondary items will be different. ' + neutfilltext);
        } else {
            fillerinfo.push('the secondary items will be again in the task (the items ' + lst(neut_target) + ' and ' + lst(neut_nontarg) + '). ');
            fillerinfo.push('the secondary items will be different. ' + numfilltext);
        }
    } else {
        fillerinfo.push('The task is the same.');
        if (fill_modes[1] === "numbers") {
            fillerinfo.push('the secondary items will be different. ' + numfilltext);
            fillerinfo.push('only country names will be in the task. ');
        } else if (fill_modes[1] === "neutral") {
            fillerinfo.push('the secondary items will be different. ' + neutfilltext);
            fillerinfo.push('only the country names will be in the task. ');
        } else {
            fillerinfo.push('only country names will be in the task. ');
            if (fill_modes[2] === "neutral") {
                fillerinfo.push('the secondary items will be different. ' + neutfilltext);
            } else {
                fillerinfo.push('the secondary items will be different. ' + numfilltext);
            }
        }
    }
    block_texts[0] = "";
    block_texts[1] =
        'During the experiment, various items (words or strings of numbers) will appear in the middle of the screen. You have to categorize each item by pressing the key "E" on the left or the key "I" on the right.</br></br>There will be three short practice rounds. In this first practice round, you have to categorize "secondary" items. ' + inducers_instructions +
        '<br><br><span id="feedback_id1">In each category of items, you need to respond to at least 80% correctly and in time.<br><br></span><p id="chances_id"></p>';
    $("#infotext").html(block_texts[1]);
    block_texts[2] =
        'Now, in this second practice round, we just want to see that you clearly understand the task. Therefore, you will have a lot of time to choose each of your responses, just make sure you choose accurately. <b>You must respond to each item correctly.</b> If you choose an incorrect response (or not give response for over 10 seconds), you will have to repeat this practice round.<br><br>If needed, click <b>show full instructions again</b> to reread the details.<span id="feedback_id2"></span><p id="chances_id"></p>';
    block_texts[3] =
        "<span id='feedback_id3'>You passed the second practice round. In this third and last practice round, you will again have to respond fast, but a certain rate of error is allowed.<br><br>The task is difficult, so don't be surprised if you make mistakes, but do your best: <b>try to be as accurate and as fast as possible</b>.<br></span><p id='chances_id'></p>";
    block_texts[4] =
        "Good job. Now begins the actual test. " + fillerinfo[0] + "<br><br>The minimum accuracy will now be much less strict than in the practice phase, but you must keep paying attention in order to perform the test validly. <b>Again: try to be as accurate and as fast as possible.</b><br><br>When you are ready, click on <b>Start</b> to start the first block of the main test.";
    block_texts[5] =
        "<span id='feedback_id5'>The first block is now done. In the next block, everything will be the same, except that " + fillerinfo[1] + "To get used to this, you will have another very short practice round. The task is the same.</span><p id='chances_id'></p>";
    block_texts[6] =
        "Well done. Now comes the second block. <b>Again: try to be as accurate and as fast as possible.</b>";
    block_texts[7] =
        '<span id="feedback_id7">In this third and final block, ' + fillerinfo[2] +
        'Again, the task is the same otherwise.<br><br>You now have another short practice phase to get used to this arrangement.</span><p id="chances_id"></p>';
    block_texts[8] =
        "Well done. Now comes the final block. <b>Again: try to be as accurate and as fast as possible.</b>";
}

// stimulus sequence generation

function inducer_items() {
    console.log('inducer_items()');
    var blck_itms_temp = JSON.parse(JSON.stringify(targetrefs.concat(nontargrefs, targetrefs, nontargrefs))); // inducers x2
    blck_itms_temp = shuffle(blck_itms_temp); // shuffle it, why not
    var safecount = 0; // just to not freeze the app if sth goes wrong
    var stim_dicts_f = []; // in here the final list of dictionary items is collected, one by one
    while (blck_itms_temp.length > 0) { // stop if all items from blck_itms_temp were use up
        var dict_item = blck_itms_temp[0];
        safecount++;
        if (safecount > 911) {
            console.log('break due to unfeasable safecount');
            break;
        }
        var good_indexes = []; // will collect the indexes where the dict item may be inserted
        var dummy_dict = [{
            'word': '-',
            'type': '-'
        }]; // dummy dict to the end
        var stim_dicts_f_d = stim_dicts_f.concat(dummy_dict);
        stim_dicts_f_d.forEach(function(f_item, f_index) {
            if (!diginto_dict(stim_dicts_f, f_index, 'word', 4).includes(dict_item.word)) {
                good_indexes.push(f_index); // if fine, do add as good index
            }
        });
        if (good_indexes.length == 0) {
            console.log('no good_indexes - count', safecount);
            blck_itms_temp = shuffle(blck_itms_temp); // reshuffle
        } else { // if there are good places, choose one randomly, insert the new item, and remove it from blck_itms_temp
            stim_dicts_f.splice(rchoice(good_indexes), 0, blck_itms_temp.shift());
        }
    }
    return (stim_dicts_f); // return final list (for blck_items var assignment)
}

function diginto_dict(dcts, indx, key_name, min_dstnc) {
    var strt;
    if (indx - min_dstnc < 0) { // if starting index is negative, it counts from the end of the list; thats no good
        strt = 0; // so if negative, we just set it to 0
    } else {
        strt = indx - min_dstnc; // if not negative, it can remain the same
    }
    let all_vals = dcts.slice(strt, indx + min_dstnc).map(a => a[key_name]);
    return (all_vals); // return all values for the specified dict key within the specified distance (from the specified dictionary)
}

function practice_items() {
    console.log('practice_items()');
    var blck_itms_temp = JSON.parse(JSON.stringify(targetrefs.concat(nontargrefs))); // get inducers
    stim_base_unique.forEach(function(cat) {
        blck_itms_temp = blck_itms_temp.concat(JSON.parse(JSON.stringify(cat))); // add all other items
    }); // TODO create practice items per category; test each separately
    blck_itms_temp = shuffle(blck_itms_temp); // shuffle it, why not
    // below the pseudorandomization to avoid close repetition of similar items (same item type)
    var safecount = 0; // just to not freeze the app if sth goes wrong
    var stim_dicts_f = []; // in here the final list of dictionary items is collected, one by one
    while (blck_itms_temp.length > 0) { // stop if all items from blck_itms_temp were use up (added to stim_dicts_f and removed with pop() )
        var dict_item = blck_itms_temp[0]; // assign first dictionary item as separate variable; for easier access below
        safecount += 1;
        if (safecount > 911) {
            console.log('break due to unfeasable safecount');
            break;
        }
        var good_indexes = []; // will collect the indexes where the dict item may be inserted
        var dummy_dict = [{
            'word': '-',
            'type': '-'
        }]; // dummy dict to the end; if the item is to be inserted to the end, there is no following dict that could cause an unwanted repetition

        var stim_dicts_f_d = stim_dicts_f.concat(dummy_dict);
        stim_dicts_f_d.forEach(function(f_item, f_index) {
            if (!diginto_dict(stim_dicts_f, f_index, 'type', 1).includes(dict_item.type)) {
                good_indexes.push(f_index); // if fine, do add as good index
            }
        });
        if (good_indexes.length == 0) {
            console.log('no good_indexes - count', safecount);
            blck_itms_temp = shuffle(blck_itms_temp); // reshuffle
        } else { // if there are good places, choose one randomly, insert the new item, and remove it from blck_itms_temp
            stim_dicts_f.splice(rchoice(good_indexes), 0, blck_itms_temp.shift());
        }
    }
    return (stim_dicts_f); // return final list (for blck_items var assignment)
}

function rndmz_details(block_stim_base) {
    var item_order = [];
    var prev_last = ''; // prev order is the item order of the previous trial sequence
    for (var i = 0; i < 18; i++) { // each i represents a sequence of 6 trials
        var item_order_temp = JSON.parse(JSON.stringify(block_stim_base)); // create a temporary item order, this represents the item order WITHIN one trial sequence
        item_order_temp = shuffle(item_order_temp); // shuffle this
        if (prev_last == item_order_temp[0].word) { // if the last one of the previous block is the first of this one
            var cutout = item_order_temp.splice(0, 1)[0]; // cut the element at index 'from'
            item_order_temp.splice(randomdigit(1, 5), 0, cutout);
        }
        item_order.push(JSON.parse(JSON.stringify(item_order_temp))); // make this the item order for this trial sequence
        prev_last = item_order_temp[item_order_temp.length - 1].word;
    }
    return (item_order);
}

function standard_items(block_stim_base) {
    return [].concat.apply([], rndmz_details(stim_base.pop(0)));
}

function inducer_randomized() { // 6 possible inducer orders
    var targetrefs_perm = shuffle(permute(targetrefs)); // 3 x 2 = 6 arrangements
    var nontarg_temp = JSON.parse(JSON.stringify(nontargrefs));
    nontarg_temp = shuffle(nontarg_temp);
    var nontargrefs_perm1 = permute(nontarg_temp.slice(0, 3)); // 3 x 2 = 6
    var nontargrefs_perm2 = permute(nontarg_temp.slice(3, 6)); // 3 x 2 = 6
    var nontargrefs_perm = [];
    for (var i = 0; i < 3; i++) { // 6/2 = 3
        nontargrefs_perm.push(nontargrefs_perm1.shift().concat(nontargrefs_perm2.shift()));
        nontargrefs_perm.push(nontargrefs_perm2.shift().concat(nontargrefs_perm1.shift()));
    }
    nontargrefs_perm = shuffle(nontargrefs_perm); // another 6 arrangements
    var inducer_lists = [];
    zip(targetrefs_perm, nontargrefs_perm).forEach(function(perm_pair) {
        var trefs = perm_pair[0];
        var ntrefs = perm_pair[1];
        var lst_temp = JSON.parse(JSON.stringify(ntrefs));
        var nums = range(0, trefs.length + ntrefs.length);
        var insert_locs = [];
        range(0, trefs.length).forEach(function(i) {
            var new_rand = rchoice(nums);
            insert_locs.push(new_rand);
            nums = nums.filter(n => Math.abs(n - new_rand) > 1);
        });
        insert_locs.sort();
        insert_locs.forEach(function(loc) {
            lst_temp.splice(loc, 0, trefs.pop());
        });
        inducer_lists.push(JSON.parse(JSON.stringify(lst_temp)));
    });
    return (inducer_lists);
}

function add_inducers(block_stim_base) {
    // First we want to assign which words get an inducer. We want each word to get an inducer in half (9) of the trials. In addition, we want half of the words in one trial sequence (3) to have an inducer. Thus we make 9 permutations of yes/no.
    var yesno_perm = permute(['y', 'y', 'y', 'n', 'n', 'n']).map(ar => JSON.stringify(ar)).filter((itm, idx, arr) => arr.indexOf(itm) === idx).map(str => JSON.parse(str));
    yesno_perm = shuffle(yesno_perm);
    var options = yesno_perm.slice(0, 9);
    var blck_rev = []; // create an empty list for the reversed block
    options.forEach(function(opt) {
        var optz_new = range(0, 6);
        opt.forEach(function(item, index) {
            if (item == "n") {
                optz_new[index] = "y";
            } else {
                optz_new[index] = "n";
            }
        });
        blck_rev.push(JSON.parse(JSON.stringify(optz_new))); // everytime add the current reversed line to the reversed block
    });
    var blck1 = options.slice(0, 3).concat(blck_rev.slice(0, 3)); // because we wanna split them up in 3 we create blcks of 6 which are each  time 3 lines and then the reverse of those 3 lines
    var blck2 = options.slice(3, 6).concat(blck_rev.slice(3, 6));
    var blck3 = options.slice(6, 9).concat(blck_rev.slice(6, 9));
    blck1 = shuffle(blck1);
    blck2 = shuffle(blck2);
    blck3 = shuffle(blck3);
    //create final block
    var blck_fin = blck1.concat(blck2, blck3);
    var word_assignment = {};
    block_stim_base.forEach(function(dct, indx) { //assign the yes/nos to the words
        word_assignment[dct.word] = blck_fin.map(a => a[indx]); // combine them to create an inducer assignment for all 18 trial sequences and assign them to the dict
    });
    //  We then need to decide which inducer is shown thus we make a list
    var inducer_lists = inducer_randomized(); // randomize 6 lists of inducer words
    var inducer_per_main = {};
    block_stim_base.forEach(function(dct) {
        inducer_per_main[dct.word] = inducer_lists.shift();
    });
    // now insert the inducers
    var final_item_order = [];
    rndmz_details(block_stim_base).forEach(function(trial_seq, t_indx) { // trial sequence represents the order in which the x amount of words are presented within one sequence (n=6) of trials
        var final_temp = [];
        trial_seq.forEach(function(item, i_indx) { // item represents each individual word (or trial)
            if (word_assignment[item.word][t_indx] == "y") { // check if the word should get an inducer
                // pick the right inducer
                // then we should delete this element so inducer so we use pop
                final_temp.push(inducer_per_main[item.word].shift()); // append the inducer to our item order
            }
            final_temp.push(item); // append the item to our item order
        });
        final_item_order.push(JSON.parse(JSON.stringify(final_temp))); // create final item order list
    });
    return ([].concat.apply([], final_item_order));
}

function main_items() {
    console.log('main_items()');
    return (add_inducers(stim_base.pop(0)));
}

// the task

var teststim,
    tooslow,
    incorrect,
    block_trialnum,
    rt_data_dict,
    trial_stim,
    keys_code,
    can_start;
var practice_repeated = {
    block1: 0,
    block2: 0,
    block3: 0,
    block5: 0,
    block7: 0,
    block9: 0
};

var cit_data = ["subject_id", "fillertype", "phase", "block_number", "trial_number", "stimulus_shown", "category", "stim_type", "response_key", "rt_start", "incorrect", "too_slow", "press_duration", "isi", "date_in_ms"].join('\t') + '\n';

var correct_key = "none";
var blocknum = 1;
var rt_start = 99999;
var press_dur = 99999;
var stim_start = 0;
listen = false;
listn_end = false;

// too slow
function flash_too_slow() {
    $("#tooslow").show();
    setTimeout(function() {
        $("#tooslow").hide();
        tooslow = 1;
        keys_code = "x";
        prac_fail();
        post_resp_hold();
    }, tooslow_delay);
}

// false
function flash_false() {
    $("#false").show();
    setTimeout(function() {
        $("#false").hide();
        incorrect = 1;
        prac_fail();
        post_resp_hold();
    }, false_delay);
}

// if there is a sole mistake in the first practice block, the block is repeated
var no_prac_fail = true;

function prac_fail() {
    if (blocknum == 2) {
        teststim = [];
        alert(
            "You did not respond correctly. You will start this practice once again. Please read the instructions carefully."
        );
        no_prac_fail = false;
    }
}


// item display timing
function monkeyPatchRequestPostAnimationFrame() {
    const channel = new MessageChannel();
    const callbacks = [];
    let timestamp = 0;
    let called = false;
    channel.port2.onmessage = e => {
        called = false;
        const toCall = callbacks.slice();
        callbacks.length = 0;
        toCall.forEach(fn => {
            try {
                fn(timestamp);
            } catch (e) {}
        });
    };
    window.requestPostAnimationFrame = function(callback) {
        if (typeof callback !== 'function') {
            throw new TypeError('Argument 1 is not callable');
        }
        callbacks.push(callback);
        if (!called) {
            requestAnimationFrame((time) => {
                timestamp = time;
                channel.port1.postMessage('');
            });
            called = true;
        }
    };
}

if (typeof requestPostAnimationFrame !== 'function') {
    monkeyPatchRequestPostAnimationFrame();
}

function chromeWorkaroundLoop() {
    if (warmup_needed) {
        requestAnimationFrame(chromeWorkaroundLoop);
    }
}

function item_display() {
    if (trial_stim.type == "target" || trial_stim.type == "targetref") {
        correct_key = "i";
    } else {
        correct_key = "e";
    }
    //if (typeof key_press_sim === "function") {
    //    key_press_sim(); //remove
    //} //remove
    window.warmup_needed = true;
    chromeWorkaroundLoop();
    setTimeout(function() {
        $("#stimulus").text(text_to_show);
        requestPostAnimationFrame(function() {
            stim_start = now();
            warmup_needed = false;
            listen = true;
            response_window = setTimeout(function() {
                rt_start = now() - stim_start;
                listen = false;
                flash_too_slow();
            }, response_deadline);
        });
    }, raf_warmup);
}
// isi
var isi_delay;

function isi() {
    $("#stimulus").text("");
    isi_delay = randomdigit(1, isi_delay_minmax[1] - isi_delay_minmax[0]);
    setTimeout(function() {
        item_display();
    }, isi_delay);
}

function post_resp_hold() {
    $("#stimulus").text("");
    setTimeout(function() {
        listn_end = false;
        add_response();
    }, isi_delay_minmax[0]);
}

it_type_feed_dict = {
    targetref: "right-side secondary items",
    nontargref: "left-side secondary items",
    main_item: "non-target countries",
    target: "target country"
};
var practice_chances = max_practice; // give 5 chances for first practice
//evaluate practice performance plus give feedback
function practice_eval(min_ratio) {
    //at least 60% on each item. if not, warn accordingly
    var is_valid = true;
    var types_failed = [];
    if (blocknum == 2) {
        is_valid = no_prac_fail;
        no_prac_fail = true;
    } else {
        for (var it_type in rt_data_dict) {
            var rts_correct = $.grep(rt_data_dict[it_type], function(rt_item) {
                return rt_item > 150;
            });
            corr_ratio = rts_correct.length / rt_data_dict[it_type].length;
            if (corr_ratio < min_ratio) {
                is_valid = false;
                types_failed.push(
                    " " +
                    it_type_feed_dict[it_type] +
                    " (" +
                    Math.floor(corr_ratio * 10000) / 100 +
                    "% correct)"
                );
            }
        }
    }
    if (is_valid == false) {
        practice_chances--;
        if (practice_chances == 0) {
            end_task("z_Invalid_practice_" + blocknum + "_");
            subject_id = 0;
            blocknum = -99;
            alert(
                "You repeated this practice phase too many times. Therefore, unfortunately, you cannot continue the experiment."
            );
            window.location = end_url;
        } else if (blocknum != 2) {
            var feedback_text =
                "<b>You will have to repeat this practice round, because of too few correct responses.</b><br><br>You need at least " + min_ratio * 100 + "% accuracy on each item type, but you did not have enough correct responses for the following one(s):" +
                types_failed.join(",") +
                ".<br><br>Try to make responses both accurately and in time.<br>";
            $("#feedback_id" + blocknum).html(feedback_text);
        }
    }
    return is_valid;
}
//evaluate main block performance
function main_eval() {
    //at least 50% on each item. if not, warn accordingly. kickout below 40%
    var verylow = false;
    var types_failed = [];
    for (var it_type in rt_data_dict) {
        var rts_correct = $.grep(rt_data_dict[it_type], function(rt_item) {
            return rt_item > 150;
        });
        corr_ratio = rts_correct.length / rt_data_dict[it_type].length;
        if (corr_ratio < 0.3) {
            end_task("z_Invalid_lowacc_");
            subject_id = 0;
            blocknum = -99;
            alert(
                "You had such an extremely low accuracy that is not possible if one pays attention to the task. Therefore, unfortunately, you cannot continue the experiment."
            );
            window.location = end_url;
            return (false);
        } else if (corr_ratio < 0.5) {
            verylow = true;
            types_failed.push(
                " " +
                it_type_feed_dict[it_type] +
                " (" +
                Math.floor(corr_ratio * 10000) / 100 +
                "% correct)"
            );
        }
    }
    if (verylow == true && blocknum > 3) {
        var feedback_text =
            "Warning: you had very low accuracy in this last block to the following item type(s):" +
            types_failed.join(",") +
            ". Please pay attention and make responses in time accurately.";
        alert(feedback_text);
    }
    return (true);
}

var text_to_show;

function next_trial() {
    if (teststim.length > 0) {
        tooslow = 0;
        incorrect = 0;
        rt_start = 99999;
        press_dur = 99999;
        keys_code = "";
        trial_stim = teststim[0];
        block_trialnum++;
        text_to_show = trial_stim.word.toUpperCase();
        isi();
    } else {
        $("#stimulus").text("");
        if ((crrnt_phase != 'practice' && main_eval()) || (blocknum == 1 && practice_eval(0.8)) || (blocknum != 1 && practice_eval(0.6))) {
            practice_chances = max_practice; // give chances for practice blocks
            blocknum++;
            if (blocknum == 4 || blocknum == 5 || blocknum == 7) {
                set_fillers(fill_modes.shift());
                set_cit_type();
            }
            $("#infotext").html(block_texts[blocknum]);
            if (blocknum == 2) {
                $("#div_cit_main").hide();
                $("#div_target_intro").show();
                $("#showfull_id").show();
                $("*").css("cursor", "auto");
            } else {
                nextblock();
            }
        } else {
            practice_repeated["block" + blocknum] += 1;
            nextblock();
        }
    }
}

function add_response() {
    var curr_type;
    if (
        ["targetref", "nontargref", "target"].indexOf(trial_stim.type) >= 0
    ) {
        curr_type = trial_stim.type;
    } else {
        curr_type = "main_item";
    }
    if (!(curr_type in rt_data_dict)) {
        rt_data_dict[curr_type] = [];
    }
    if (incorrect == 1 || tooslow == 1) {
        rt_data_dict[curr_type].push(-1);
    } else {
        rt_data_dict[curr_type].push(rt_start);
    }
    if (press_dur == 99999) {
        press_dur = '-';
    }
    var key_letter;
    if (keys_code == 69) {
        key_letter = "e";
    } else if (keys_code == 73) {
        key_letter = "i";
    } else {
        key_letter = keys_code;
    }
    cit_data += [subj_id,
        fillertype,
        crrnt_phase,
        blocknum,
        block_trialnum,
        text_to_show,
        trial_stim.cat,
        trial_stim.type,
        key_letter,
        rt_start,
        incorrect,
        tooslow,
        press_dur,
        isi_delay + isi_delay_minmax[0] + raf_warmup,
        String(new Date().getTime())
    ].join('\t') + '\n';
    teststim.shift();
    next_trial();
}

var crrnt_phase;

function nextblock() {
    $("*").css("cursor", "auto");
    if (blocknum <= num_of_blocks) {
        crrnt_phase = 'practice';
        block_trialnum = 0;
        if (blocknum == 1) {
            response_deadline = response_deadline_main;
            teststim = inducer_items();
        } else if (blocknum == 2) {
            response_deadline = 10000;
            teststim = practice_items();
        } else if (blocknum == 3) {
            $("#label_top").html("");
            $("#label_right").html("");
            $("#label_left").html("");
            response_deadline = response_deadline_main;
            teststim = practice_items();
        } else if (blocknum == 5 || blocknum == 7) {
            teststim = practice_items();
            if (fillertype == "nofiller") {
                teststim = shuffle(JSON.parse(JSON.stringify(stim_base_unique[0])));
            } else {
                teststim = practice_items();
            }
        } else {
            crrnt_phase = 'main';
            if (fillertype == "nofiller") {
                teststim = standard_items();
            } else {
                teststim = main_items();
            }
        }
        rt_data_dict = {};
        show_blockstart();
    } else {
        ins = "";
        $("#div_cit_main").hide();
        lighten_bg();
        $("#div_outro_check").show();
    }
}

function show_blockstart() {
    if (practice_chances == 1) {
        $("#chances_id").html(
            "<b>Now you only have 1 single chance left to complete this practice round correctly!</b> Our minimum requirements are very low. If you understand the instructions clearly, and follow them carefully, you must be able to complete the task. Otherwise you will be disqualified from this experiment."
        );
    } else {
        $("#chances_id").html(
            "Make sure you clearly understand the instructions! You have only <b>" +
            practice_chances +
            " chances</b> to complete this practice round."
        );

    }
    $("#div_cit_main").hide();
    $("#div_cit_blockstart").show();
}

function runblock() {
    $("*").css("cursor", "none");
    $("#div_cit_blockstart").hide();
    $("#start_text").show();
    $("#div_cit_main").show();
    can_start = true;
}

$(document).ready(function() {
    $(document).keyup(function(es) {
        if (listn_end == true) {
            upcode = es.keyCode || es.which;
            if (upcode == keys_code) {
                press_dur = performance.now() - stim_start - rt_start;
                listn_end = false;
            }
        } else if (can_start === true) { //starting screen
            code = es.keyCode || es.which;
            if (code == 32) {
                //space
                can_start = false;
                $("#start_text").hide();
                next_trial();
            }
        }
    });
    $(document).keydown(function(e) {
        if (listen === true) {
            rt_start = now() - stim_start;
            if (rt_start < response_deadline) {
                keys_code = e.keyCode || e.which;
                if (keys_code == 69 || keys_code == 73) {
                    clearTimeout(response_window);
                    listen = false;
                    listn_end = true;
                    if (keys_code == 69) {
                        //nontarget
                        if (correct_key == "e") {
                            post_resp_hold();
                        } else if (correct_key == "i") {
                            flash_false();
                        }
                    } else if (keys_code == 73) {
                        //target
                        if (correct_key == "i") {
                            post_resp_hold();
                        } else if (correct_key == "e") {
                            flash_false();
                        }
                    }
                }
            }
        }
    });
});
