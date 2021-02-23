<?php

ini_set("display_errors", 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

if (!empty($_SERVER['HTTP_CLIENT_IP'])) {
  $ip=$_SERVER['HTTP_CLIENT_IP'];
} elseif (!empty($_SERVER['HTTP_X_FORWARDED_FOR'])) {
  $ip=$_SERVER['HTTP_X_FORWARDED_FOR'];
} else {
  $ip=$_SERVER['REMOTE_ADDR'];
}

$thisid = $_POST['sid_post'];
$subj_group = $_POST['cond_post'];
$subj_conds = $_POST['condplus_post'];
$stol_sum = $_POST['sum_post'];

$con = mysqli_connect("lukacsg89.mysql.univie.ac.at","lukacsg89","choice12","lukacsg89");

if (mysqli_connect_errno()) {
    echo "Failed to connect to MySQL! Contact gaspar.lukacs@univie.ac.at, also copying the following error message: \n (Starter) " . mysqli_connect_error() ;
    exit();
}
$sql_id ="SELECT subject_id FROM filler_opt_data;";

$id_data = $con->query($sql_id);
if (($id_data) && ($id_data->num_rows > 0)) {
    while ($row = mysqli_fetch_array($id_data)){
        $all_ids[] = $row["subject_id"];
    }
} else {
    $all_ids = array();
}

if (in_array($thisid, $all_ids))  {
    echo "It seems that you have started this test once already.<br><br>If you believe this is a mistake, write us an email.";
} else {
    $sql = "INSERT INTO `filler_opt_data`(all_data_id, subject_id, group_code, conds, stol_sum, ip) VALUES (NULL, '$thisid', '$subj_group', '$subj_conds', '$stol_sum', '$ip');";
    if (!mysqli_multi_query($con, $sql)) {
        echo 'Failed to save data! Please do not close this page, but contact gaspar.lukacs@univie.ac.at, also copying the following error message: \n (Starter) ' . mysqli_error($con);
    } else {
        echo '<br><br>Please right-click <a id="link_id" target="_blank" href="https://np36kt57.github.io/?pid=">THIS LINK</a> and open it in a new tab or new window. (The link expires after a single use!) Then follow the instructions.<br><br>
        Reward password: <input id="rcode_id" type="text" size="20"> <span id="passfeed" style="color:red"></span><br><br><button class="intro_button" onclick="checkpass();">SUBMIT</button><br>';
    }
}

mysqli_close($con);
