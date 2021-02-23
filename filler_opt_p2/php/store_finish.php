<?php

ini_set("display_errors", 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

$file_name=('../../filler_opt_results/' . $_POST['filename_post']);
$subject_results = $_POST['results_post'];
$sid = $_POST['sid_post'];

$outcome = file_put_contents($file_name, $subject_results, FILE_APPEND);

if (strlen($sid) < 9 or strlen($subject_results) < 5000 or
    substr($file_name, -4) !== ".txt") {
    echo 'Failed. Data not correct. If you believe this is an error, contact gaspar.lukacs@univie.ac.at';
} else if (is_file($file_name) === FALSE) {
    echo "Failed to save file " . $file_name . "! Please do not close this page, but contact gaspar.lukacs@univie.ac.at! (" . $outcome . ")";
} else if ($outcome < 5000) {
    echo "Failed to save full file! Please do not close this page, but contact gaspar.lukacs@univie.ac.at! (" . $file_name . ")";
} else {
    echo "https://app.prolific.co/submissions/complete?cc=6B044E77";
}
