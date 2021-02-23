/*jshint esversion: 6 */

let targetflr_words, nontargflr_words;
let item_bases, item_cats,
    the_targets, the_probes, the_nontargs, nontargs_cpy,
    targetflrs, nontargflrs;
let fillertype, nontargfills;

// 0: Nowak; verbal
// 1: Nowak; signal
// 2: Nowak; mixed
// 3: Koch; verbal
// 4: Koch; signal
// 5: Koch; mixed

function shuff_chars(strng) {
    let arr = strng.split("");
    shuffed = shuffle(arr);
    return shuffed.join("");
}

let nonverbtype = 'none';

// Relevant, Recognized, Crucial, Meaningful, Significant, Familiar, Vital, Important, Critical, Pivotal, Pertinent, Salient, Central
// Trivial, Unrelated, Moot

function prep_conds() {
    let targetverb = ['Meaningful', 'Crucial', 'Recognized'];
    let nontargverb = ['Unimportant', 'Insignificant', 'Other', 'Random', 'Unfamiliar', 'Irrelevant'];
    if (condition == 0 || condition == 3) {
        fillertype = 'verbal';
        targetflr_words = targetverb;
        nontargflr_words = nontargverb;
    } else if (condition == 1 || condition == 4) {
        fillertype = 'signal';
        targetflr_words = [];
        nontargflr_words = [];
        if (Math.random() < 0.5) {
            nonverbtype = 'numbers';
            shuffle(targetverb).forEach((wrd, idx) => {
                targetflr_words.push(['1', '2', '3'][idx].repeat(wrd.length));
            });
            shuffle(nontargverb).forEach((wrd, idx) => {
                nontargflr_words.push(['4', '5', '6', '7', '8', '9'][idx].repeat(wrd.length));
            });
        } else {
            nonverbtype = 'arrows';
            let syms_t = '＞>⧽❯⟩)}⦔'.repeat(4); // sorted: ")>}❯⟩⦔⧽＞"
            let syms_nt = '＜<⧼❮⟨({⦓'.repeat(4); // sorted: "(<{❮⟨⦓⧼＜"
            let t_starts = shuff_chars('＞>⧽');
            let nt_starts = shuff_chars('＜<⧼❮⟨({⦓');
            window.nums_nontarg = [syms_nt, syms_nt, syms_nt, syms_nt, syms_nt, syms_nt];
            window.nums_target = [syms_t, syms_t, syms_t];
            shuffle(targetverb).forEach((wrd, idx) => {
                targetflr_words.push(t_starts[idx] +
                    shuff_chars(nums_target[idx].slice(0, wrd.length - 1)));
            });
            shuffle(nontargverb).forEach((wrd, idx) => {
                nontargflr_words.push(nt_starts[idx] +
                    shuff_chars(nums_nontarg[idx].slice(0, wrd.length - 1)));
            });
        }
    } else {
        fillertype = 'mixed';
        targetflr_words = targetverb;
        nontargflr_words = [];
        if (Math.random() < 0.5) {
            nonverbtype = 'numbers';
            shuffle(nontargverb).forEach((wrd, idx) => {
                nontargflr_words.push(['4', '5', '6', '7', '8', '9'][idx].repeat(wrd.length));
            });
        } else {
            nonverbtype = 'arrows';
            let syms_nt = '＜<⧼❮⟨({⦓'.repeat(4); // sorted: "(<{❮⟨⦓⧼＜"
            let nt_starts = shuff_chars('＜<⧼❮⟨({⦓');
            window.nums_nontarg = [syms_nt, syms_nt, syms_nt, syms_nt, syms_nt, syms_nt];
            shuffle(nontargverb).forEach((wrd, idx) => {
                nontargflr_words.push(nt_starts[idx] +
                    shuff_chars(nums_nontarg[idx].slice(0, wrd.length - 1)));
            });
        }
    }
    if (subcond == 6 || subcond == 8) {
        nontargfills = 'yes';
    } else {
        nontargfills = 'no';
    }
    if (subcond == 6 || subcond == 7) {
        item_cats = ['banks', 'nicks', 'names', 'pins'];
    } else {
        item_cats = ['names', 'pins', 'banks', 'nicks'];
    }
    create_stim_base();
}

let stim_bases;
let all_probes = [];

function create_stim_base() {
    let words_lists = [
        [],
        [],
        [],
        []
    ];
    stim_bases = [
        [],
        [],
        [],
        []
    ];
    the_targets = [
        [],
        [],
        [],
        []
    ];
    the_nontargs = [
        [],
        [],
        [],
        []
    ];
    all_rts = {
        "probe": [],
        "irrelevant1": [],
        "irrelevant2": [],
        "irrelevant3": [],
        "irrelevant4": [],
        "targetflr": [],
        "nontargflr": [],
        "target": []
    };
    item_cats.forEach((itemskey, num) => {
        itemlist = item_bases[itemskey];
        itemlist.forEach((item, indx) => {
            let tempdict = {
                'item': item,
                'cat': itemskey
            };
            if (0 === indx) {
                tempdict.type = "probe";
                the_nontargs[num].push(item);
                words_lists[num].push(item);
                all_probes.push(item);
            } else if (1 === indx) {
                tempdict.type = "target";
                the_targets[num].push(item);
            } else {
                tempdict.type = "irrelevant" + (indx - 1);
                the_nontargs[num].push(item);
                words_lists[num].push(item);
            }
            stim_bases[num].push(JSON.parse(JSON.stringify(tempdict)));
        });
        words_lists[num] = words_lists[num].sort(function(a, b) {
            return a.localeCompare(b);
        });
        words_lists[num].push("I don't know.");
    });

    words_lists.forEach((wlist, ind) => {
        let radopts = "";
        wlist.forEach(function(word) {
            radopts += word + ' <input type="radio" name="pch' + (testnum + 1) + ind + '" value="' + word + '"><br>';
        });
        $("#div_pcheck" + (testnum + 1) + ind).append(radopts);
    });

    targetflrs = [];
    nontargflrs = [];
    targetflr_words.forEach((ref_item, num) => {
        targetflrs.push({
            'item': ref_item,
            'type': 'targetflr',
            'cat': 'filler'
        });
    });
    nontargflr_words.forEach((ref_item, num) => {
        nontargflrs.push({
            'item': ref_item,
            'type': 'nontargflr',
            'cat': 'filler'
        });
    });
    window.stim_base_unique = JSON.parse(JSON.stringify(stim_bases));
    stim_bases = stim_bases.concat(JSON.parse(JSON.stringify(stim_bases)));
    set_block_texts();
    $("#infotext").html(block_texts[blocknum]);
}


let pchosen1;

function final_probe_check() {
    pchosen1 = [
        $("input[name=pch10]:checked").val(),
        $("input[name=pch11]:checked").val(),
        $("input[name=pch12]:checked").val(),
        $("input[name=pch13]:checked").val()
    ];
    let len = pchosen1.filter((v) => {
        return v !== undefined;
    }).length;
    if (len < 4) {
        alert('Please select an option in each of the 4 columns!');
    } else {
        $('#div_outro_check').hide();
        end_task();
        $('#div_outro_end').show();
        window.scrollTo(0, 0);
    }
}
