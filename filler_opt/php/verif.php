<?php

ini_set("display_errors", 1);
error_reporting(E_ALL);

header('Access-Control-Allow-Origin: *');
header("Access-Control-Allow-Methods: POST");
header("Access-Control-Allow-Headers: Content-Type");

function test_input($data) {
    $data = trim($data);
    $data = stripslashes($data);
    $data = htmlspecialchars($data);
    return $data;
}

$postdata = file_get_contents("php://input");
$request   = json_decode($postdata);
$sid     = test_input($request->id_post);
$thepw     = test_input($request->pass_post);

$con = mysqli_connect("lukacsg89.mysql.univie.ac.at","lukacsg89","choice12","lukacsg89");

if (mysqli_connect_errno()) {
    echo "Failed to connect to MySQL! Contact gaspar.lukacs@univie.ac.at, also copying the following error message: \n (verification) " . mysqli_connect_error() ;
    exit();
}

$sql_id ="SELECT crime_done, group_code FROM filler_opt_data WHERE subject_id = '$sid' ORDER BY all_data_id DESC LIMIT 1;";

$id_data = mysqli_query($con, $sql_id) or die(mysqli_error($con));

if (mysqli_num_rows($id_data) > 0) {
    $datdict = mysqli_fetch_assoc($id_data);
    $cond = $datdict['group_code'];
    if ($cond > 2) {
        $act_pin = "5288";
    } else {
        $act_pin = "4377";
    }
    if ($datdict['crime_done'] == 1) {
        if ($thepw == $act_pin) {
            echo "https://app.prolific.co/submissions/complete?cc=7C67471F";
        } else {
            echo "Wrong password: wrong code.";
        }
    } else {
        echo "Wrong password: no crime.";
    }
} else {
    echo "Wrong password: no ID.";
}

mysqli_close($con);
