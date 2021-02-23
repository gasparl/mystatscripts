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
        $("#mobile_warn").show();
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


//background changes
function darken_bg() {
    $("#html_id").css("background", bg_color);
}

function lighten_bg() {
    $("#html_id").css("background", "#666666");
}

// copy-pasting
function detect_copy_paste() {
    $("html").bind("copy paste drop", function(e) {
        alert("Please do not copy or paste anything on this page.");
        e.preventDefault();
    });
}

function selectable_bg() {
    $("#html_id").css({
        "-webkit-user-select": "text" /* Chrome/Safari/Opera */ ,
        "-moz-user-select": "text" /* Firefox */ ,
        "-ms-user-select": "text" /* IE/Edge */ ,
        "-khtml-user-select": "text" /* Konqueror */ ,
        "user-select": "text" /* non-prefixed version */
    });
    $("html").unbind("copy paste drop");
}

function no_select_bg() {
    detect_copy_paste();
    $("#html_id").css({
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

function randomdigit(min, max) {
    return Math.floor(Math.random() * (max - min + 1)) + min;
}

// timing
var now = function() {
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
function shuffle(array) {
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

var zip = (...rows) => [...rows[0]].map((_, c) => rows.map(row => row[c]));

function dl_as_file() {
    filename_to_dl = f_name;
    data_to_dl = cit_data;
    var blobx = new Blob([data_to_dl], {
        type: 'text/plain'
    });
    var elemx = window.document.createElement('a');
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

// check eligiblity, save start
function starter() {
    $('#div_intro_consent').text("Loading. Please wait.");
    basic_times.consented = neat_date();
    let stolnsum = 10000 + Math.round(Math.random() * Math.pow(10, randomdigit(3, 6) + 2));
    $.post("php/starter.php", {
            sid_post: userid,
            cond_post: condition,
            condplus_post: condition.toString() + randomdigit(6, 9),
            sum_post: stolnsum
        }, function(resp) {
            $('#div_intro_consent').hide();
            console.log(resp);
            resp = resp.replace("?pid=", "?pid=" + userid);
            $('#div_intro_ready').html(resp);
            $('#div_intro_ready').show();
        })
        .fail(function(xhr, status, error) {
            $('#div_intro_consent').hide();
            console.log("Connection failed.");
            console.log(error);
            $('#div_start_error').show();
        });
}

function pidpass(pid) {
    return pid.substring(5, 12).split("").reverse().join("");
}

function checkpass() {
    if ($('#rcode_id').val().substring(4) == '#np36kt57i' + pidpass(userid)) {
        crime_done();
    } else {
        $('#passfeed').text("Wrong password.");
        setTimeout(function() {
            $('#passfeed').text("");
        }, 3000);
    }
}

function crime_done() {
    fetch('./php/verif.php', {
            method: 'post',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({
                id_post: userid,
                pass_post: $('#rcode_id').val().substring(0, 4)
            })
        })
        .then(function(response) {
            return response.text();
        })
        .then(function(response) {
            console.log(response);
            if (response.startsWith("http")) {
                $('#div_intro_ready').hide();
                $("#passw_display").html('<a href=' + response + ' target="_blank">' + response + '</a>');
                $('#div_outro_end').show();
            } else
            if (response.startsWith("Wrong pass")) {
                $('#passfeed').text("Wrong password.");
                setTimeout(function() {
                    $('#passfeed').text("");
                }, 3000);
            } else {
                $('#passfeed').text(response);
                setTimeout(function() {
                    $('#passfeed').text("");
                }, 5000);
            }
        })
        .catch(function(error) {
            console.log('Request failed: ', error);
            document.getElementById("sub").innerHTML = 'Connection failed. Please check whether your internet is working. If the problem persists, please contact gaspar.lukacs@univie.ac.at, following error: ' + error;
        });
}

function consent_refused(div_id) {
    change_div(div_id, "#div_refused");
}

function create_fillers() {
    let animat = shuffle(['bear', 'cat', 'fox', 'mouse', 'rabbit', 'rat', 'sheep', 'tiger', 'turtle', 'wolf']);
    let inanim = shuffle(['bed', 'cabinet', 'chair', 'couch', 'desk', 'dresser', 'lamp', 'sofa', 'stool', 'table']);
    let neuts = shuffle([animat, inanim]);
    window.neut_nontarg = neuts[0].slice(0, 6).sort();
    window.neut_target = neuts[1].slice(0, 3).sort();
    let fillnums = ['1', '2', '3', '4', '5', '6', '7', '8', '9'];
    if (Math.random() >= 0.5) {
        fillnums.reverse();
    }
    window.nums_nontarg = fillnums.slice(0, 6);
    window.nums_target = fillnums.slice(6, 9);
    neut_nontarg.forEach((wrd, idx) => {
        nums_nontarg[idx] = nums_nontarg[idx].repeat(wrd.length);
    });
    neut_target.forEach((wrd, idx) => {
        nums_target[idx] = nums_target[idx].repeat(wrd.length);
    });
}

var fillertype;

function set_fillers(fill_type) {
    fillertype = fill_type;
    if (fill_type === "numbers") {
        window.nontargref_words = nums_nontarg;
        window.targetref_words = nums_target;
    } else if (fill_type === "neutral") {
        window.nontargref_words = neut_nontarg;
        window.targetref_words = neut_target;
    }
    window.targetrefs = [];
    window.nontargrefs = [];
    targetref_words.forEach(function(ref_word) {
        targetrefs.push({
            'word': ref_word,
            'type': 'targetref',
            'cat': 'filler'
        });
    });
    nontargref_words.forEach(function(ref_word) {
        nontargrefs.push({
            'word': ref_word,
            'type': 'nontargref',
            'cat': 'filler'
        });
    });
}


// end task
function end_task(full_validity = "") {
    selectable_bg();
    basic_times.finished = neat_date();
    var duration_full = Math.round((now() - basic_times.loaded_now) / 600) / 100;
    var practice_all_reps = Object.values(practice_repeated).reduce((a, b) => a + b);
    var dems = ['subject_id',
            'condition',
            'gender',
            'age',
            'handed',
            'edu',
            'lg',
            'country',
            'browser_name',
            'browser_version',
            'practice_all_reps',
            'full_dur',
            'failed_final',
            'failed_nums'
        ].join('/') +
        '\t' + [subj_id,
            condition,
            $("#gender").val(),
            $("#age").val(),
            $("#handedness").val(),
            $("#education").val(),
            $("#mothertongue").val(),
            $("#countries").val(),
            $.browser.name,
            $.browser.version,
            practice_all_reps,
            duration_full,
            failed_final,
            num_of_failed_fin
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
    store_end(full_validity);
}

function store_end() {
    let condition;
    f_name =
        full_validity +
        experiment_title +
        "_" +
        subj_id +
        ".txt";
    $.post(
            "php/store_finish.php", {
                filename_post: f_name,
                results_post: cit_data,
                sid_post: subj_id,
                cond_post: condition
            },
            function(resp) {
                if (resp.startsWith("Fail") || resp.startsWith("Warning")) {
                    alert(resp);
                    $('#div_end_error').show();
                } else if (full_validity == "") {
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

function store_basics() {
    //declare unique id
    window.subj_id =
        rchoice("CDFGHJKLMNPQRSTVWXYZ") +
        rchoice("AEIOU") +
        rchoice("CDFGHJKLMNPQRSTVWXYZ") +
        "_" +
        $("#cf_id").val();
    $.post(
            "php/store_start.php", {
                dems_post: start_dems,
                exp_title_post: experiment_title
            },
            function(resp) {
                console.log("server responding...");
                console.log(resp);
            }
        )
        .fail(function(xhr, status, error) {
            console.log("Could not store basics. No connection.");
            console.log(error);
        });
}
