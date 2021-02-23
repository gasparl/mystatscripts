/*jshint esversion: 6 */

// set global cit vars
var response_deadline_main = 900;
var tooslow_delay = 300;
var false_delay = 300;
var raf_warmup = 100;
var actual_isi_delay_minmax = [300, 600];
var end_url = "https://www.figure-eight.com/";
const all_conditions = [0, 1, 2, 3, 4, 5];
// 0: Nowak; verbal
// 1: Nowak; signal
// 2: Nowak; mixed
// 3: Koch; verbal
// 4: Koch; signal
// 5: Koch; mixed

var subj_id;
var response_deadline, teststim, prac_teststim;
var basic_times = {};
var num_of_blocks = 8; // including practice, starting at 1
var bg_color = "#000000"; //#031116
var isi_delay_minmax = [actual_isi_delay_minmax[0] - raf_warmup, actual_isi_delay_minmax[1] - raf_warmup];
// so the real ISI will be isi_delay+isi_delay_minmax[0]+raf_warmup
var max_practice = 6;

$(document).ready(function() {
    basic_times.loaded = neat_date();
    basic_times.loaded_now = now();
    if (detectmob()) {
        chrome_check();
    }
});

function chrome_check() {
    var browser_name = $.browser.name;
    if (browser_name != "Chrome") {
        console.log("Detected browser: " + browser_name + ". This application should be run in Google Chrome.");
        alert("Your browser was detected to be " + browser_name + "! This test was optimized for and should be run in Google Chrome. Please make sure you use the appropriate browser.");
        $("#not_chrome_warn").html("<br><i>The test was designed for <b>Google Chrome</b>, but your browser was detected to be " + browser_name + ".<br><br>If you want to do this test, please use Google Chrome.</i><br><br>");
        $("#not_chrome_warn").show();
    } else {
        userid_check();
    }
}

function hasnumalph(myString) {
    return /\d/.test(myString) && /[a-zA-Z]/g.test(myString);
}

let userid, condition;
function userid_check() {
    let params = new URLSearchParams(location.search);
    userid = params.get('PROLIFIC_PID');
    condition = parseInt(params.get('c'));
    if (userid != null && userid.length > 20 &&
        userid.length < 30 && hasnumalph(userid) &&
        all_conditions.includes(condition)) {
        $("#pay_info").html("Completed and valid participation in <b>both parts</b> of the experiments will be rewarded with altogether 3.00 GBP gross (Prolific ID: " + userid + '). Note that if you do not participate in Part 2, you do not get paid for Part 1 either: you have to either "Return" it or it will be "Rejected".<br></i>');
        $("#div_intro_consent").show(); // div_cit_main // div_intro_consent div_outro_end div_end_screen
    } else {
        $("#not_pid_warn").show();
    }
}
