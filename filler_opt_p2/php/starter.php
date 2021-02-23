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
    echo "Failed to connect to MySQL! Contact gaspar.lukacs@univie.ac.at, also copying the following error message: \n (verification) " . mysqli_connect_error() ;
    exit();
}

$sql_id ="SELECT crime_done, group_code, conds, stol_sum, ROUND(TIMESTAMPDIFF(MINUTE, crime_date, NOW())/60, 2) as tdiff FROM filler_opt_data WHERE subject_id = '$sid' ORDER BY all_data_id DESC LIMIT 1;";

$id_data = mysqli_query($con, $sql_id) or die(mysqli_error($con));

if (mysqli_num_rows($id_data) > 0) {
    $datdict = mysqli_fetch_assoc($id_data);
    if ($datdict['crime_done'] == 1) {
        $t_diff = $datdict['tdiff'];
        if ($datdict['group_code'] > 2) {
            $items = '{ "banks": [ "Phoenix Community Trust", "Citizen Union Finances", "Vertex Corporation Banks", "Goldward Credit Union", "Springwell Bank Group", "Elysium Holding Company" ], "pins": ["5288", "7399", "6911", "8622", "9566", "4377" ], "names": [ "Phil Jenks", "Tim Howe", "Ray Snell", "Neil Rand", "Gene Falk", "Ralph Croft" ], "nicks": [ "kocht57", "langen92", "mullf29", "kugerh83", "hornm64", "bohml56" ] }';
        } else {
            $items = '{ "banks": [ "Elysium Holding Company", "Citadel Syndicate Group", "Zenith National Holdings", "Vanguard Savings Bank", "Bulwarks Credit Union", "Phoenix Community Trust" ], "pins": ["4377", "6422", "1799", "2955", "7866", "5288" ], "names": [ "Dale Spence", "Wayne Bryant", "Glenn Platt", "Walt Rusk", "Tod Ames", "Earl Dade" ], "nicks": [ "nowakp36", "huberm94", "mullerf27", "schrobh84", "kugele41", "bohmej58" ] }';
        }
        if ($t_diff < 20) {
            echo "Too soon.";
        } else if ($t_diff > 28) {
            echo "Too late.";
        } else {
            $sql="UPDATE filler_opt_data SET time_diff = '$t_diff', finished = CURRENT_TIME() WHERE subject_id = '$sid' ORDER BY all_data_id DESC LIMIT 1;";
            if (!mysqli_multi_query($con,$sql))  {
                echo 'Failed to save data! Please do not close this page, but contact gaspar.lukacs@univie.ac.at, also copying the following error message: \n (save) ' . mysqli_error($con);
            } else {
                echo "yep" . $datdict['conds'] . "_" . $datdict['stol_sum'] . "_" . $items;
            }
        }
    } else {
        echo "No corresponding completed Part 1 submission found.";
    }
} else {
    echo "No corresponding Part 1 submission found.";
}

mysqli_close($con);
