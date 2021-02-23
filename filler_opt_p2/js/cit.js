/*jshint esversion: 6 */


function remove_nonts(fullblck) {
    let filted = fullblck.filter((dct, indx) => {
        return dct.type != 'nontargflr' &&
            (dct.type != 'targetflr' || fullblck[indx + 1].type != 'target');
    });
    let comb_dicts = {};
    let ckey;
    filted.forEach((dct, indx) => {
        if (dct.type == 'targetflr') {
            ckey = filted[indx + 1].item;
            if (!(ckey in comb_dicts)) {
                comb_dicts[ckey] = [];
            }
            comb_dicts[ckey].push([dct.item, indx]);
        }
    });
    let placements = [];
    let tfills = [];
    let to_remove = [];
    shuffle(Object.keys(comb_dicts)).forEach((ckey, indx) => {
        let scores = [];
        comb_dicts[ckey].forEach((item, i) => {
            score = 0;
            score += tfills.filter(el => el === item[0]).length * 10;
            score += placements.filter(el => el === i).length;
            scores.push(score);
        });
        let mins = [0, 1, 2].filter(i => scores[i] == Math.min(...scores));
        let placement = shuffle(mins)[0];
        let chosn = comb_dicts[ckey][placement];
        placements.push(placement);
        tfills.push(chosn[0]);
        to_remove.push(chosn[1]);
    });
    let toret = filted.filter((dct, i) => {
        return !to_remove.includes(i);
    });
    return (toret);
}

function list_items(lst) {
    return ('<b>' + lst.sort(function(a, b) {
        return a.localeCompare(b);
    }).join("</b>, <b>") + '</b>');
}

let block_texts;

function set_block_texts() {
    let trefs = list_items(targetflr_words);
    let nontrefs = list_items(nontargflr_words);
    let change_hints;
    if (nontargfills == 'yes') {
        change_hints = ['In the third and last practice round, all items are present.',
            'During the rest of the test, including the upcoming block, the items ' + nontrefs + ' will not appear anymore.</br></br>'
        ];
    } else {
        change_hints = ['In the third and last practice round, both the items from the second round and the items to be categorized to the right side from the first round are present.',
            'During the rest of the test, including the upcoming block, items ' + nontrefs + ' will again appear. Same as in the very first practice round, press the left ("E") key whenever you see any of these items.</br></br>'
        ];
    }
    let targs = [
        list_items(the_targets[0]),
        list_items(the_targets[1]),
        list_items(the_targets[2]),
        list_items(the_targets[3])
    ];
    let nontargs = [
        list_items(the_nontargs[0]),
        list_items(the_nontargs[1]),
        list_items(the_nontargs[2]),
        list_items(the_nontargs[3])
    ];
    let numprac;
    let thetexts = [''];
    numprac = 'three';

    let intro = 'During the following test, various items will appear in the middle of the screen. You have to categorize each item by pressing the <i>key "E" on the left</i> or the <i>key "I" on the right</i>. ';
    let intro_end = 'There will be ' + numprac + ' short practice rounds. ';
    let inducers_instructions =
        '</br></br>Press the right ("I") key, when you see the following items: ' + trefs + '.</br></br>Press the left ("E") key when you see any other item. (These other items are: ' + nontrefs + '.)';
    let main_instruction = '</br></br>Press the right ("I") key when you see the following "target" item: ' +
        targs[0] +
        '.</br></br>Press the left ("E") key when you see any other item. These other items are: ' +
        nontargs[0] + '.';
    let main_end = '</br></br>In this practice round, you will have a lot of time to choose each response, but <b>you must respond to each item correctly</b>. If you choose an incorrect response (or not give response for over 10 seconds), you will have to repeat the practice round. ';
    thetexts.push(
        '<span id="feedback_id1">' + intro + intro_end + '</br></br>In the first practice round, you have to categorize two kinds of items. ' + inducers_instructions +
        '</br></br>There is a certain time limit for making each response. Please try to be both fast and accurate. In each of the two item categories, you need at least 80% correct responses in time, otherwise you have to repeat the practice round.</span>');
    thetexts.push("<span id='feedback_id2'>In the second practice round, you have to categorize the main test items. The aim of the entire test will be to show whether or not one of these main items is recognized by you. " + main_instruction + main_end + '</span>');
    thetexts.push(
        '<span id="feedback_id3">' + change_hints[0] + '<br><br>You again have to respond fast, but a certain number of mistakes is allowed. The task is the same. Press the right ("I") key, when you see any of the following items: ' + targs[0].replace('<br>', '') + ', ' + trefs + '. Press the left ("E") key when you see any other items.</span>');
    thetexts.push(
        'Now the test begins. The task is the same. Press the right ("I") key, when you see any of the following items: ' + targs[0].replace('<br>', '') + ', ' + trefs + '. Press the left ("E") key when you see any other words.<br><br>Try to be both fast and accurate.');

    [5, 7, 9, 11, 12, 13, 15].forEach((bnum, i) => {
        if (i != 0) {
            repped = ', once again,';
        } else {
            repped = '';
        }
        pnum = (i + 1) % 4;
        thetexts.push(
            '<span id="feedback_id' + bnum + '">Now' + repped + ' the main words will be different. Press the right ("I") key when you see the following "target" item: ' +
            targs[pnum] +
            '.</br></br>Press the left ("E") key when you see any of the following items: ' +
            nontargs[pnum] + '.</br></br>To make sure you understood this, in the following short practice round <b>you must respond to each item correctly</b>, otherwise you have to repeat this round.</span>');
        if (bnum == 11) {
            thetexts.push(
                change_hints[1] +
                ' Otherwise, the task is the same. Press the right ("I") key, when you see any of the following items: ' + targs[pnum].replace('<br>', '') + ', ' + trefs + '. Press the left ("E") key when you see any other item.');
        } else {
            thetexts.push(
                'Alright. The next block starts. The task is the same. Press the right ("I") key, when you see any of the following items: ' + targs[pnum].replace('<br>', '') + ', ' + trefs + '. Press the left ("E") key when you see any other item.');
        }
    });
    block_texts = thetexts;
}

function show_again() {
    $('#infotext').html(block_texts[blocknum]);
    $("#showfull_id").hide();
}

// stimulus sequence generation

function inducer_items() {
    console.log('inducer_items()');
    var blck_itms_temp = JSON.parse(JSON.stringify(targetflrs.concat(nontargflrs, targetflrs, nontargflrs))); // inducers x2
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
            'item': '-',
            'type': '-'
        }]; // dummy dict to the end
        var stim_dicts_f_d = stim_dicts_f.concat(dummy_dict);
        stim_dicts_f_d.forEach(function(f_item, f_index) {
            if (!diginto_dict(stim_dicts_f, f_index, 'item', 4).includes(dict_item.item)) {
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

function practice_items(current_base) {
    console.log('practice_items()');
    let blck_itms_temp;
    if (nontargfills == 'no') {
        blck_itms_temp = shuffle(targetflrs).slice(0, 2);
    } else {
        blck_itms_temp = JSON.parse(JSON.stringify(targetflrs.concat(nontargflrs)));
    }
    blck_itms_temp = blck_itms_temp.concat(JSON.parse(JSON.stringify(current_base)));
    blck_itms_temp = shuffle(blck_itms_temp);
    if (nontargfills == 'no') {
        return blck_itms_temp;
    }
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
            'item': '-',
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
        if (prev_last == item_order_temp[0].item) { // if the last one of the previous block is the first of this one
            var cutout = item_order_temp.splice(0, 1)[0]; // cut the element at index 'from'
            item_order_temp.splice(randomdigit(1, 5), 0, cutout);
        }
        item_order.push(JSON.parse(JSON.stringify(item_order_temp))); // make this the item order for this trial sequence
        prev_last = item_order_temp[item_order_temp.length - 1].item;
    }
    return (item_order);
}

function inducer_randomized() { // 6 possible inducer orders
    var targetflrs_perm = shuffle(permute(targetflrs)); // 3 x 2 = 6 arrangements
    var nontarg_temp = JSON.parse(JSON.stringify(nontargflrs));
    nontarg_temp = shuffle(nontarg_temp);
    var nontargflrs_perm1 = permute(nontarg_temp.slice(0, 3)); // 3 x 2 = 6
    var nontargflrs_perm2 = permute(nontarg_temp.slice(3, 6)); // 3 x 2 = 6
    var nontargflrs_perm = [];
    for (var i = 0; i < 3; i++) { // 6/2 = 3
        nontargflrs_perm.push(nontargflrs_perm1.shift().concat(nontargflrs_perm2.shift()));
        nontargflrs_perm.push(nontargflrs_perm2.shift().concat(nontargflrs_perm1.shift()));
    }
    nontargflrs_perm = shuffle(nontargflrs_perm); // another 6 arrangements
    var inducer_lists = [];
    zip(targetflrs_perm, nontargflrs_perm).forEach(function(perm_pair) {
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
    // First we want to assign which items get an inducer. We want each item to get an inducer in half (9) of the trials. In addition, we want half of the items in one trial sequence (3) to have an inducer. Thus we make 9 permutations of yes/no.
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
    var item_assignment = {};
    block_stim_base.forEach(function(dct, indx) { //assign the yes/nos to the items
        item_assignment[dct.item] = blck_fin.map(a => a[indx]); // combine them to create an inducer assignment for all 18 trial sequences and assign them to the dict
    });
    //  We then need to decide which inducer is shown thus we make a list
    var inducer_lists = inducer_randomized(); // randomize 6 lists of inducer items
    var inducer_per_main = {};
    block_stim_base.forEach(function(dct) {
        inducer_per_main[dct.item] = inducer_lists.shift();
    });
    // now insert the inducers
    var final_item_order = [];
    rndmz_details(block_stim_base).forEach(function(trial_seq, t_indx) { // trial sequence represents the order in which the x amount of items are presented within one sequence (n=6) of trials
        var final_temp = [];
        trial_seq.forEach(function(item, i_indx) { // item represents each individual item (or trial)
            if (item_assignment[item.item][t_indx] == "y") { // check if the item should get an inducer
                // pick the right inducer
                // then we should delete this element
                final_temp.push(inducer_per_main[item.item].shift()); // append the inducer to our item order
            }
            final_temp.push(item); // append the item to our item order
        });
        final_item_order.push(JSON.parse(JSON.stringify(final_temp))); // create final item order list
    });
    return ([].concat.apply([], final_item_order));
}

let seconds = 0;

function main_items() {
    console.log('main_items()');
    let newstims = stim_bases.shift();
    let to_ret;
    if (seconds == 0) {
        // prepare normal full block
        let fullblock = add_inducers(newstims);
        // remove nontargs if so set
        if (nontargfills == 'no') {
            fullblock = remove_nonts(fullblock);
        }
        // prep two separate blocks
        let halfat = Math.ceil(fullblock.length / 2);
        if (fullblock[halfat - 1].cat == "filler") {
            if (Math.random() < 0.5) {
                halfat++;
            } else {
                halfat--;
            }
        }
        to_ret = JSON.parse(JSON.stringify(fullblock.slice(0, halfat)));
        seconds = JSON.parse(JSON.stringify(fullblock.slice(halfat, fullblock.length)));
    } else {
        to_ret = replac_stims(seconds, newstims);
        seconds = 0;
    }
    return to_ret;
}

function replac_stims(blckitems, stm_base) {
    let stm_dict = {};
    stm_base.forEach(dct => {
        stm_dict[dct.type] = dct;
    });
    blckitems.forEach((dct, ind) => {
        if (dct.cat != "filler") {
            blckitems[ind] = stm_dict[dct.type];
        }
    });
    return (blckitems);
}

// the task

let teststim,
    tooslow,
    incorrect,
    block_trialnum,
    rt_data_dict,
    trial_stim,
    keys_code,
    can_start;
let practice_repeated = {};

var cit_data = ["subject_id", "phase", "nontargfills", "block_number", "trial_number", "stimulus_shown", "category", "stim_type", "response_key", "rt_start", "incorrect", "too_slow", "press_duration", "isi", "date_in_ms"].join('\t') + '\n';

var correct_key = "none";
var blocknum = 1;
var rt_start = 99999;
var press_dur = 99999;
var stim_start = 0;
let listen = false;
let listn_end = false;

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
let no_prac_fail = true;

function prac_fail() {
    if (crrnt_phase === 'practice_strict') {
        teststim = [];
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
    if (trial_stim.type == "target" || trial_stim.type == "targetflr") {
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
        document.getElementById('stimulus').textContent = text_to_show;
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

let isfirst = false;

let correct_feed = '% correct';
let accrep_feed = '<b>You will have to repeat this practice round due to a wrong response (or too much waiting).</b>';
let acc_feed = {
    start: "<b>You will have to repeat this practice round because of too few correct responses ",
    targs: 'to the items to be categorized with the right ("I") key. Please pay more attention to these items!<br>',
    nontargs: 'to the items to be categorized with the left ("E") key. Please pay more attention to these items!<br>',
    allits: "to all the item types. Please pay more attention!<br>"
};

function practice_eval(min_ratio) {
    if (!(blocknum in practice_repeated)) {
        practice_repeated[blocknum] = 0;
    }
    var is_valid = true;
    var types_failed = [];
    if (blocknum == 3) {
        if (isfirst == false) {
            isfirst = true;
        } else {
            return true;
        }
    }
    if (crrnt_phase === 'practice_strict') {
        is_valid = no_prac_fail;
        if (is_valid === false) {
            $("#feedback_id" + blocknum).html(accrep_feed);
            $("#showfull_id").show();
        }
        no_prac_fail = true;
    } else {
        let targfail = 0,
            nontargfail = 0;
        for (var it_type in rt_data_dict) {
            var rts_correct = $.grep(rt_data_dict[it_type], function(rt_item) {
                return rt_item > 150;
            });
            corr_ratio = rts_correct.length / rt_data_dict[it_type].length;
            if (corr_ratio < min_ratio) {
                is_valid = false;
                if (it_type.startsWith('targ')) {
                    targfail++;
                } else {
                    nontargfail++;
                }
                types_failed.push(it_type);
            }
        }
        if (is_valid == false) {
            if (targfail > 0 && nontargfail > 0) {
                $("#feedback_id" + blocknum).html(acc_feed.start + acc_feed.allits);
            } else if (targfail > 0) {
                $("#feedback_id" + blocknum).html(acc_feed.start + acc_feed.targs);
            } else {
                $("#feedback_id" + blocknum).html(acc_feed.start + acc_feed.nontargs);
            }
            $("#showfull_id").show();
        }
    }
    return is_valid;
}

let text_to_show;

function next_trial() {
    if (teststim.length > 0) {
        tooslow = 0;
        incorrect = 0;
        rt_start = 99999;
        press_dur = 99999;
        keys_code = "";
        trial_stim = teststim[0];
        block_trialnum++;
        text_to_show = trial_stim.item;
        isi();
    } else {
        $("#stimulus").text("");
        if ((crrnt_phase === 'main') || (blocknum == 1 && practice_eval(0.8)) ||
            (blocknum != 1 && practice_eval(0.6))) {
            blocknum++;
            $("#infotext").html(block_texts[blocknum]);
            $("#showfull_id").hide();
            nextblock();
        } else {
            practice_repeated[blocknum] += 1;
            nextblock();
        }
    }
}

function add_response() {
    var curr_type;
    if (
        ["targetflr", "nontargflr", "target"].indexOf(trial_stim.type) >= 0
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
    if (crrnt_phase == 'main' &&
        rt_start >= 150 &&
        tooslow === 0 &&
        rt_start <= response_deadline_main &&
        incorrect === 0 &&
        curr_type == "main_item") {
        if (trial_stim.type === 'probe') {
            rts_prob[testnum].push(rt_start);
        } else if (trial_stim.type.startsWith('irrel')) {
            rts_irr[testnum].push(rt_start);
        }
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
        crrnt_phase,
        nontargfills,
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

let crrnt_phase;
let testnum = 0;

function nextblock() {
    $("*").css("cursor", "auto");
    if (stim_bases.length > 0) {
        crrnt_phase = 'practice';
        block_trialnum = 0;
        if (blocknum == 1) {
            response_deadline = response_deadline_main;
            teststim = inducer_items();
        } else if (blocknum == 2) {
            response_deadline = 10000;
            crrnt_phase = 'practice_strict';
            teststim = shuffle(stim_base_unique[0]);
        } else if (blocknum == 3) {
            response_deadline = response_deadline_main;
            teststim = practice_items(stim_base_unique[0]);
        } else if (blocknum % 2 == 1) {
            response_deadline = 10000;
            crrnt_phase = 'practice_strict';
            teststim = shuffle(stim_bases[0]);
        } else {
            if (stim_bases.length == 4) {
                let revd = {
                    'yes': 'no',
                    'no': 'yes'
                };
                nontargfills = revd[nontargfills];
            }
            response_deadline = response_deadline_main;
            crrnt_phase = 'main';
            teststim = main_items();
        }
        rt_data_dict = {};
        show_blockstart();
    } else {
        $("#div_cit_main").hide();
        $("html").css("background", "#d1d1d1");
        $("#scale_div").show();
    }
}

function show_blockstart() {
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
