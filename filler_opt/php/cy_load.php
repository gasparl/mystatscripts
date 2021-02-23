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

$con = mysqli_connect("lukacsg89.mysql.univie.ac.at","lukacsg89","choice12","lukacsg89");

if (mysqli_connect_errno()) {
    echo "Failed to connect to MySQL! Contact gaspar.lukacs@univie.ac.at, also copying the following error message: \n (Starter) " . mysqli_connect_error() ;
    exit();
}

$sql_id ="SELECT crime_done, group_code FROM filler_opt_data WHERE subject_id = '$sid' ORDER BY all_data_id DESC LIMIT 1;";

$id_data = mysqli_query($con, $sql_id) or die(mysqli_error($con));

if (mysqli_num_rows($id_data) > 0) {
    $datdict = mysqli_fetch_assoc($id_data);
    if ($datdict['crime_done'] == 0) {
        $cond = $datdict['group_code'];
        if ($cond > 2) {
            $e_name = "Tim Koch";
            $e_user = "kocht57";
            $e_pass = "KochTim57";
        } else {
            $e_name = "Paul Nowak";
            $e_user = "nowakp36";
            $e_pass = "NowakPaul36";
        }
        echo 'We hacked ' . $e_name . '\'s email account and have his login info, but to hide our tracks we want someone else to access his account via the webmail server of the University of Vienna (access at: https://www.univie.ac.at/ZID/webmail/). The username is "' . $e_user . '", the password is "' . $e_pass . '". Search messages for any credit card information, e.g. look for "credit card" or "card number".<br><br>';
    } else {
        die("Done already.");
    }
} else {
    die("No ID.");
}

mysqli_close($con);
