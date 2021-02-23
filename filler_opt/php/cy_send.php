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
$p_name1 = test_input($request->post_name1);
$p_name2 = test_input($request->post_name2);
$p_bank = test_input($request->post_bank);
$p_cardnum = test_input($request->post_cardnum);
$p_date1 = test_input($request->post_date1);
$p_date2 = test_input($request->post_date2);
$p_pin = test_input($request->post_pin);


$con = mysqli_connect("lukacsg89.mysql.univie.ac.at","lukacsg89","choice12","lukacsg89");

if (mysqli_connect_errno()) {
    echo "Failed to connect to MySQL! Contact gaspar.lukacs@univie.ac.at, also copying the following error message: \n (Starter) " . mysqli_connect_error() ;
    exit();
}

$sql_id ="SELECT crime_done, group_code, stol_sum FROM filler_opt_data WHERE subject_id = '$sid' ORDER BY all_data_id DESC LIMIT 1;";

$id_data = mysqli_query($con, $sql_id) or die(mysqli_error($con));

if (mysqli_num_rows($id_data) > 0) {
    $datdict = mysqli_fetch_assoc($id_data);
    if ($datdict['crime_done'] == 0) {
        $cond = $datdict['group_code'];
        $ssum = number_format($datdict['stol_sum'] / 100, 2);
        if ($cond > 2) {
            $act_name1 = "Phil";
            $act_name2 = "Jenks";
            $act_bank = "Phoenix Community Trust";
            $act_cardnum = "5185210716148193";
            $act_date1 = "05";
            $act_date2 = "23";
            $act_pin = "5288";
            $dets = '{ "bnk": "b_Pho.png", "cnum": "8193", "pin": "5288", "cname": "Phil Jenks", "msg": "via kocht57", "type": "c_mc.png", "stolsum" : "' . $ssum . '" }';
        } else {
            $act_name1 = "Dale";
            $act_name2 = "Spence";
            $act_bank = "Elysium Holding Company";
            $act_cardnum = "4716906231502282";
            $act_date1 = "04";
            $act_date2 = "25";
            $act_pin = "4377";
            $dets = '{ "bnk": "b_Ely.png", "cnum": "2282", "pin": "4377", "cname": "Dale Spence", "msg": "via nowakp36", "type": "c_vi.png", "stolsum" : "' . $ssum . '" }';
        }

        similar_text(strtolower($p_bank), strtolower($act_bank), $perc_b);
        if (strtolower($p_name1) == strtolower($act_name1) &&
            strtolower($p_name2) == strtolower($act_name2) &&
            $perc_b > 90 &&
            $p_cardnum == $act_cardnum &&
            $p_date1 == $act_date1 &&
            $p_date2 == $act_date2 &&
            $p_pin == $act_pin) {
            $sql="UPDATE filler_opt_data SET crime_done = 1, crime_date = CURRENT_TIME() WHERE subject_id = '$sid' ORDER BY all_data_id DESC LIMIT 1;";
            if (!mysqli_multi_query($con,$sql))  {
                echo 'Failed to save data! Please do not close this page, but contact gaspar.lukacs@univie.ac.at, also copying the following error message: \n (save) ' . mysqli_error($con);
            } else {
                echo 'Thank you. With your help, we managed to withdraw ' . $ssum . ' U.S. dollars from the account of ' . $act_name1 . ' ' . $act_name2 . ', via ' . $act_bank . ', card number ' . $act_cardnum . ' (PIN: ' . $act_pin . '). To collect your reward, use the following password: ' . $act_pin . 'PSWD. As soon as you have copied this password, close this page and never access it again._%_' . $dets;
            }
        } else {
            echo "invalid";
        }
    } else {
        die("This credit card has already been accessed.");
    }
} else {
    die("Information could not be verified.");
}

mysqli_close($con);
