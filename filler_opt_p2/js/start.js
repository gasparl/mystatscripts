/*jshint esversion: 6 */

const experiment_title = "fill_opt";
// set global cit vars
const response_deadline_main = 1000;
const tooslow_delay = 500;
const false_delay = 500;
const raf_warmup = 100;
const actual_isi_delay_minmax = [500, 800];
const end_url = "https://www.figure-eight.com/";
const basic_times = {};
const num_of_blocks = 10; // including practice, starting at 1
const bg_color = "#000000"; //#031116
const isi_delay_minmax = [actual_isi_delay_minmax[0] - raf_warmup, actual_isi_delay_minmax[1] - raf_warmup];
// so the real ISI will be isi_delay+isi_delay_minmax[0]+raf_warmup
const subj_id =
    rchoice("CDFGHJKLMNPQRSTVWXYZ") +
    rchoice("AEIOU") +
    rchoice("CDFGHJKLMNPQRSTVWXYZ") +
    "_" +
    neat_date();
let condition, subcond;
let response_deadline;

const all_conditions = [0, 1, 2, 3, 4, 5];
// 0: Nowak; verbal
// 1: Nowak; signal
// 2: Nowak; mixed
// 3: Koch; verbal
// 4: Koch; signal
// 5: Koch; mixed

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

let userid;

function userid_check() {
    let params = new URLSearchParams(location.search);
    userid = params.get('PROLIFIC_PID');
    if (userid != null && userid.length > 20 &&
        userid.length < 30 && hasnumalph(userid)) {
        starter();
    } else {
        $("#not_pid_warn").show();
    }
}

let stolsum;
function starter() {
    fetch("./php/starter.php", {
            method: 'post',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({
                id_post: userid
            })
        })
        .then(function(response) {
            return response.text();
        })
        .then(function(response) {
            console.log(response);
            if (response.startsWith("yep")) {
                condition = parseInt(response.substring(3, 4));
                subcond = parseInt(response.substring(4, 5));
                let respinfo = response.split("_");
                stolsum = respinfo[1];
                item_bases = JSON.parse(respinfo[2]);
                prep_conds();
            } else {
                $('#div_intro_ready').html(response);
            }
            $('#div_intro_ready').show();
        })
        .catch(function(error) {
            console.log("Connection failed.");
            console.log(error);
            $('#div_start_error').show();
        });
}
