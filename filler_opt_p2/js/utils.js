/*jshint esversion: 6 */

// translation
var tranlate_count = 0;

function detect_transl() {
    setInterval(function() {
        if (
            $("html").hasClass("translated-ltr") ||
            $("html").hasClass("translated-rtl")
        ) {
            if (tranlate_count == 0) {
                alert(
                    "First warning: as written in the instructions, translation in this experiment is detected and will result in the invalidation of your test. Please turn it off immediately."
                );
            } else if (tranlate_count == 1) {
                alert(
                    "Second and last warning: translation of this site was detected for the second time. If it is detected one more time, the test will end and your data will be erased. Please turn it off immediately or (if you don't know how to turn it off) begin the experiment in a new window of your browser."
                );
                tranlate_count++;
            } else if (tranlate_count > 1) {
                alert(
                    "Due to continual or repeated use of automatic translation function, the experiment ends here."
                );
                window.location = end_url;
            }
            if ($("html").hasClass("notranslate") === false) {
                $("html").addClass("notranslate");
            }
            tranlate_count++;
        }
    }, 10000);
}

function inse(str, index, count, add) {
    var ar = str.split('');
    ar.splice(index, count, add);
    return ar.join('');
}

//only allow number in input field
function validate(evt) {
    var theEvent = evt || window.event;
    var key = theEvent.keyCode || theEvent.which;
    key = String.fromCharCode(key);
    var regex = /[0-9]/;
    if (!regex.test(key)) {
        theEvent.returnValue = false;
        if (theEvent.preventDefault) theEvent.preventDefault();
    }
}

// detect mobile device
function detectmob() {
    var is_valid = true;
    if (
        navigator.userAgent.match(/Android/i) ||
        navigator.userAgent.match(/webOS/i) ||
        navigator.userAgent.match(/iPhone/i) ||
        navigator.userAgent.match(/iPad/i) ||
        navigator.userAgent.match(/iPod/i) ||
        navigator.userAgent.match(/BlackBerry/i) ||
        navigator.userAgent.match(/Windows Phone/i)
    ) {
        alert(
            "You are using a smartphone or tablet. In this phase there is no mobile version of the test available. \nUnfortunately you cannot do this experiment on your mobile device. \nStart the experiment again with a normal webbrowser on your computer."
        );
        is_valid = false;
        window.location = end_url;
    }
    return is_valid;
}

//do nothing
function noop() {
    $.noop();
}

// change div - if good to go. Two optional function to include in execution (default does nothing)
function change_div(
    current,
    next,
    good_to_go = true,
    onchange_function1 = noop,
    onchange_function2 = noop
) {
    if (good_to_go === true) {
        onchange_function1();
        onchange_function2();
        var toHide = "#" + current.parentNode.id;
        $(toHide).hide();
        $(next).show();
    }
}

//after forms, check if all filled in
function validate_form(form_class) {
    var is_valid = true;
    $(form_class).each(function() {
        if ($(this).val() === "") is_valid = false;
    });
    if (is_valid === false) {
        alert("Please fill in all fields.");
    }
    return is_valid;
}

function evl(inp) {
    if (blocknum > 4 && $("#prob1check").val() != "") {
        return inp;
    } else {
        return "";
    }
}

function open_fulls() {
    try {
        let elem = document.documentElement;
        if (elem.requestFullscreen) {
            elem.requestFullscreen();
        } else if (elem.mozRequestFullScreen) {
            /* Firefox */
            elem.mozRequestFullScreen();
        } else if (elem.webkitRequestFullscreen) {
            /* Chrome, Safari and Opera */
            elem.webkitRequestFullscreen();
        } else if (elem.msRequestFullscreen) {
            /* IE/Edge */
            elem.msRequestFullscreen();
        }
    } catch (err) {
        console.log(err);
    }
}

/* Close fullscreen */
function close_fulls() {
    try {
        let elem = document.documentElement;
        if (document.exitFullscreen) {
            document.exitFullscreen();
        } else if (document.mozCancelFullScreen) {
            /* Firefox */
            document.mozCancelFullScreen();
        } else if (document.webkitExitFullscreen) {
            /* Chrome, Safari and Opera */
            document.webkitExitFullscreen();
        } else if (document.msExitFullscreen) {
            /* IE/Edge */
            document.msExitFullscreen();
        }
    } catch (err) {
        console.log(err);
    }
}

function selectable_bg() {
    $("html").css({
        "-webkit-user-select": "text" /* Chrome/Safari/Opera */ ,
        "-moz-user-select": "text" /* Firefox */ ,
        "-ms-user-select": "text" /* IE/Edge */ ,
        "-khtml-user-select": "text" /* Konqueror */ ,
        "user-select": "text" /* non-prefixed version */
    });
}

function no_select_bg() {
    $("html").css({
        "-webkit-user-select": "none" /* Chrome/Safari/Opera */ ,
        "-moz-user-select": "none" /* Firefox */ ,
        "-ms-user-select": "none" /* IE/Edge */ ,
        "-khtml-user-select": "none" /* Konqueror */ ,
        "user-select": "none" /* non-prefixed version */
    });
}

//calculate sum
function sum(array_to_sum) {
    var sum = 0;
    array_to_sum.forEach(function(item) {
        sum += item;
    });
    return sum;
}
//calculate mean
function mean(array_to_avg) {
    var mean = sum(array_to_avg) / array_to_avg.length;
    return mean;
}

//calculate sd
function sd(array_for_sd) {
    var m = mean(array_for_sd);
    return Math.sqrt(array_for_sd.reduce(function(sq, n) {
        return sq + Math.pow(n - m, 2);
    }, 0) / (array_for_sd.length - 1));
}

function sd_pooled(var1, var2) {
    let n1 = var1.length;
    let n2 = var2.length;
    let nom = (n1 - 1) * (Math.pow(sd(var1), 2)) + (n2 - 1) * (Math.pow(sd(var2), 2));
    return Math.sqrt(nom / (n1 + n2 - 2));
}

function d_pool(var1, var2) {
    return (mean(var1) - mean(var2)) / sd_pooled(var1, var2);
}

function randomdigit(min, max) {
    return Math.floor(Math.random() * (max - min + 1)) + min;
}

// timing
let now = function() {
    var performance = window.performance || {};
    performance.now = (function() {
        return (
            performance.now ||
            performance.webkitNow ||
            performance.msNow ||
            performance.oNow ||
            performance.mozNow ||
            function() {
                return new Date().getTime();
            }
        );
    })();
    return performance.now();
};
window.requestAnimationFrame = window.requestAnimationFrame || window.mozRequestAnimationFrame || window.webkitRequestAnimationFrame || window.msRequestAnimationFrame;

//shuffle
function shuffle(arr) {
    let array = JSON.parse(JSON.stringify(arr));
    var newarr = [];
    var currentIndex = array.length,
        temporaryValue,
        randomIndex;
    while (0 !== currentIndex) {
        randomIndex = Math.floor(Math.random() * currentIndex);
        currentIndex -= 1;
        temporaryValue = array[currentIndex];
        array[currentIndex] = array[randomIndex];
        newarr[currentIndex] = array[randomIndex];
        array[randomIndex] = temporaryValue;
    }
    return newarr;
}
//shuffle string

function str_shuffle(strng, count = 0) {
    let original = strng;
    let array = strng.split("");
    var newarr = [];
    var currentIndex = array.length,
        temporaryValue,
        randomIndex;
    while (0 !== currentIndex) {
        randomIndex = Math.floor(Math.random() * currentIndex);
        currentIndex -= 1;
        temporaryValue = array[currentIndex];
        array[currentIndex] = array[randomIndex];
        newarr[currentIndex] = array[randomIndex];
        array[randomIndex] = temporaryValue;
    }
    newword = newarr.join("");
    if (count < 10 && similar_text(original, newword, true) >= 80) {
        newword = str_shuffle(strng, count + 1);
    } else if (count < 99 && similar_text(original, newword, true) >= 85) {
        newword = str_shuffle(strng, count + 1);
    }
    return newword;
}


function similar_text(first, second, percent) {
    // Programming Classics: Implementing the World's Best Algorithms by Oliver (ISBN 0-131-00413-1)
    // https://www.php.net/manual/en/function.similar-text.php
    if (first === null || second === null || typeof first === 'undefined' || typeof second === 'undefined') {
        return 0;
    }

    first += '';
    second += '';

    var pos1 = 0,
        pos2 = 0,
        max = 0,
        firstLength = first.length,
        secondLength = second.length,
        p, q, l, sum;

    max = 0;

    for (p = 0; p < firstLength; p++) {
        for (q = 0; q < secondLength; q++) {
            for (l = 0;
                (p + l < firstLength) && (q + l < secondLength) && (first.charAt(p + l) === second.charAt(q + l)); l++)
            ;
            if (l > max) {
                max = l;
                pos1 = p;
                pos2 = q;
            }
        }
    }

    sum = max;

    if (sum) {
        if (pos1 && pos2) {
            sum += this.similar_text(first.substr(0, pos1), second.substr(0, pos2));
        }

        if ((pos1 + max < firstLength) && (pos2 + max < secondLength)) {
            sum += this.similar_text(first.substr(pos1 + max, firstLength - pos1 - max), second.substr(pos2 + max,
                secondLength - pos2 - max));
        }
    }

    if (!percent) {
        return sum;
    } else {
        return (sum * 200) / (firstLength + secondLength);
    }
}

// random choice from array
function rchoice(array) {
    return array[Math.floor(array.length * Math.random())];
}

// pythonic range (integers from START until before END)
function range(start, end) {
    var r = [];
    for (var i = start; i < end; i++) {
        r.push(i);
    }
    return r;
}

function permute(to_permutate) {
    var length = to_permutate.length,
        result = [to_permutate.slice()],
        c = new Array(length).fill(0),
        i = 1,
        k, p;
    while (i < length) {
        if (c[i] < i) {
            k = i % 2 && c[i];
            p = to_permutate[i];
            to_permutate[i] = to_permutate[k];
            to_permutate[k] = p;
            ++c[i];
            i = 1;
            result.push(to_permutate.slice());
        } else {
            c[i] = 0;
            ++i;
        }
    }
    return result;
}

let zip = (...rows) => [...rows[0]].map((_, c) => rows.map(row => row[c]));

function dl_as_file() {
    filename_to_dl = f_name;
    data_to_dl = cit_data;
    let blobx = new Blob([data_to_dl], {
        type: 'text/plain'
    });
    let elemx = window.document.createElement('a');
    elemx.href = window.URL.createObjectURL(blobx);
    elemx.download = filename_to_dl;
    document.body.appendChild(elemx);
    elemx.click();
    document.body.removeChild(elemx);
}

function copy_to_clip() {
    element = $('<textarea>').appendTo('body').val(cit_data).select();
    document.execCommand("Copy");
    element.remove();
}

function neat_date() {
    var m = new Date();
    return m.getFullYear() + "" +
        ("0" + (m.getMonth() + 1)).slice(-2) + "" +
        ("0" + m.getDate()).slice(-2) + "" +
        ("0" + m.getHours()).slice(-2) + "" +
        ("0" + m.getMinutes()).slice(-2) + "" +
        ("0" + m.getSeconds()).slice(-2);
}

function consent_agreed() {
    basic_times.consented = neat_date();
    open_fulls();
    detect_transl();
    no_select_bg();
    $("html").css("background", bg_color);
    $("#div_intro_ready").hide();
    $("#div_item_rec").show();
    prepits();
}

let its_dict = {};
let its_count = 0;

function prepits() {
    for (var i = 0; i < 6; i++) {
        its_dict[item_bases.banks[i]] = [item_bases.pins[i],
            item_bases.names[i], item_bases.nicks[i]
        ];
    }
    next_its();
}

function okits(elem) {
    elem.style.display = 'none';
    $("#its_rec").show();
}

function hovd(el) {
    if (el.style.fontWeight != 'bold') {
        its_count++;
        el.style.fontWeight = 'bold';
        if (its_count > 3) {
            document.getElementById("rec_but").disabled = false;
        }
    }
}

function next_its() {
    its_count = 0;
    document.getElementById("rec_but").disabled = true;
    if (Object.keys(its_dict).length === 0) {
        $("#div_item_rec").hide();
        nextblock();
    } else {
        let ky = Object.keys(its_dict).sort()[0];
        let tops = shuffle([randomdigit(8, 40), randomdigit(60, 83),
            randomdigit(8, 40), randomdigit(60, 83)
        ]);
        let newgrp = '<span onmouseover="hovd(this);" style="position: absolute;left: 5%;top:' +
            tops[0] + '%">' + ky + '</span>' +
            '<span onmouseover="hovd(this);" style="position: absolute;left: 43%;top:' +
            tops[1] + '%">' + its_dict[ky][0] + '</span>' +
            '<span onmouseover="hovd(this);" style="position: absolute;left: 58%;top:' +
            tops[2] + '%">' + its_dict[ky][1] + '</span>' +
            '<span onmouseover="hovd(this);" style="position: absolute;left: 80%;top:' +
            tops[3] + '%">' + its_dict[ky][2] + '</span>';
        $("#its_group").html(newgrp);
        delete its_dict[ky];
    }
}

let rts_prob = [
    [],
    []
];
let rts_irr = [
    [],
    []
];

let attcount = 0;

function attchecks() {
    document.getElementById("attcheck_id").classList.remove("slider_hide_thumb");
    attcount++;
    if (attcount > 2) {
        $("#attdiv").html("<br><b>Good! Thanks for paying attention! You can continue.</b><br><br>");
    }
}

// end task
function end_task() {
    selectable_bg();
    close_fulls();
    basic_times.finished = neat_date();
    let dcit = [
        d_pool(rts_prob[0], rts_irr[0])
    ];
    let pcorr1 = pchosen1.filter(word => all_probes.indexOf(word) !== -1).length;
    var duration_full = Math.round((now() - basic_times.loaded_now) / 600) / 100;
    var practice_all_reps = Object.values(practice_repeated).reduce((a, b) => a + b);
    let real_rate = 'NA';
    let anx_rate = 'NA';
    let excit_rate = 'NA';


    if (!$("#realism_id").hasClass("slider_hide_thumb")) {
        real_rate = document.getElementById("realism_id").value;
    }
    if (!$("#anx_id").hasClass("slider_hide_thumb")) {
        anx_rate = document.getElementById("anx_id").value;
    }
    if (!$("#excit_id").hasClass("slider_hide_thumb")) {
        excit_rate = document.getElementById("excit_id").value;
    }
    var dems = ['subject_id',
            'filler_type',
            'condition',
            'subcond',
            'browser_name',
            'browser_version',
            'practice_all_reps',
            'probe_selected',
            'probe_correct',
            'dcit',
            'nonverbals',
            'amount',
            'full_dur',
            'r_realism',
            'r_anxiety',
            'r_excitement',
            'attention',
            'userid'
        ].join('/') +
        '\t' + [subj_id,
            fillertype,
            condition,
            subcond,
            $.browser.name,
            $.browser.version,
            practice_all_reps,
            pchosen1.join('|'),
            pcorr1,
            dcit[0].toFixed(5),
            nonverbtype,
            stolsum,
            duration_full,
            real_rate,
            anx_rate,
            excit_rate,
            attcount,
            userid
        ].join('/');
    cit_data +=
        "Loaded " +
        basic_times.loaded +
        " Consented " +
        basic_times.consented +
        " Finished " +
        basic_times.finished +
        "\nRepetitions " +
        Object.values(practice_repeated).join('/') +
        "\ndems\t" + dems;
    store_end();
}

function store_end() {
    f_name =
        experiment_title +
        "_" +
        condition + subcond +
        "_" +
        subj_id +
        ".txt";
    $.post(
            "php/store_finish.php", {
                filename_post: f_name,
                results_post: cit_data,
                sid_post: subj_id
            },
            function(resp) {
                if (resp.startsWith("Fail") || resp.startsWith("Warning")) {
                    alert(resp);
                    $('#div_end_error').show();
                } else {
                    $("#passw_display").html('<a href=' + resp + ' target="_blank">' + resp + '</a>');
                }
            }
        )
        .fail(function(xhr, status, error) {
            console.log(error);
            $('#div_end_error').show();
            $("#passw_display").html("<i>(server connection failed)</i>");
        });
}
