/*jshint esversion: 6 */

var countrs = ["Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria", "Burkina Faso", "Burma", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Canada", "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros", "Congo", "Costa Rica", "Croatia", "Cuba", "Cyprus", "Czech Republic", "Denmark", "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Ethiopia", "Fiji", "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada", "Guatemala", "Guinea", "Guinea Bissau", "Guyana", "Haiti", "Vatican City", "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy", "Ivory Coast", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Macedonia", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Namibia", "Nauru", "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Korea", "Norway", "Oman", "Pakistan", "Palau", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Romania", "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent", "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Korea", "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Swaziland", "Sweden", "Switzerland", "Syria", "Tajikistan", "Tanzania", "Thailand", "Timor Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "United States", "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe"];
var animls = ["Alligator", "Alpaca", "Ant", "Antelope", "Badger", "Bat", "Bear", "Beaver", "Bee", "Bison", "Buffalo", "Butterfly", "Camel", "Cat", "Caterpillar", "Cheetah", "Chicken", "Chimpanzee", "Cobra", "Crab", "Crane", "Crocodile", "Crow", "Deer", "Dog", "Dolphin", "Donkey", "Dove", "Duck", "Dugong", "Eagle", "Elephant", "Emu", "Falcon", "Ferret", "Flamingo", "Fly", "Fox", "Frog", "Gazelle", "Giraffe", "Gnu", "Goat", "Goose", "Gorilla", "Grasshopper", "Hamster", "Hawk", "Hedgehog", "Herring", "Hippopotamus", "Horse", "Hummingbird", "Hyena", "Jackal", "Jaguar", "Jellyfish", "Kangaroo", "Kiwi", "Koala", "Lemur", "Leopard", "Lion", "Llama", "Lobster", "Locust", "Magpie", "Mammoth", "Mole", "Mongoose", "Monkey", "Moose", "Mouse", "Mosquito", "Octopus", "Opossum", "Ostrich", "Owl", "Oyster", "Panther", "Parrot", "Panda", "Pelican", "Penguin", "Pheasant", "Pig", "Pigeon", "Porcupine", "Porpoise", "Rabbit", "Raccoon", "Ram", "Rat", "Raven", "Reindeer", "Rhinoceros", "Salamander", "Salmon", "Seahorse", "Seal", "Shark", "Sheep", "Sloth", "Snail", "Snake", "Spider", "Squirrel", "Swan", "Tapir", "Tiger", "Toad", "Turkey", "Walrus", "Wasp", "Weasel", "Whale", "Wolf", "Wolverine", "Wombat", "Yak", "Zebra"];
var nums = range(1, 32);

var countrs_orig = $.extend(true, [], countrs);

countrs.forEach(function(item, index) {
    countrs[index] = item.toLowerCase();
});
animls.forEach(function(item, index) {
    animls[index] = item.toLowerCase();
});

var items_base1 = [
    countrs.sort(), ["january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december"], nums, animls.sort()
];

var categories_base = ["countries", "months", "days", "animals"];
//var categories = [ "countries", "dates", "animals" ];
var categories = ["countries"];

$(document).ready(function() {
    categories_base.forEach(function(categ, index) { //fills up the selection options for the categories
        var dropChoices = '';
        var catAll = items_base1[index];
        catAll.forEach(function(word) {
            dropChoices += '<option value="' + word + '">' + word + '</option>';
        });
        ['#', '#tcheck_', '#fcheck_'].forEach(function(pre_id, index) {
            categ_id = pre_id + categ;
            $(categ_id).append(dropChoices);
        });
    });
});
var true_details, items_base2;

function prune() {
    //given the probe (in each of the categories), selects 8 additional items, 5 of which will later be irrelevants. None with same starting letter, and with length closest possible to the probe.
    var true_details_base = [
        $("#countries")
        .val()
        .toLowerCase(),
        'may',
        '11',
        'cat'
    ];
    true_details = [
        true_details_base[0],
        [true_details_base[1], true_details_base[2]].join(" "),
        true_details_base[3]
    ];
    var items_base2_temp = [];
    true_details_base.forEach(function(probe, index) {
        var container = items_base1[index],
            temps;
        var final8 = [probe];
        var maxdif = 0;
        if (probe[0] > -1) {
            final8.push.apply(final8, [0, 0, 0, 0, 0, 0, 0]);
        } else {
            container = $.grep(container, function(n) {
                return probe != n;
            });
            container = $.grep(container, function(n) { // filter if same starting character
                return probe[0] != n[0];
            });
            if (index === 0) {
                if (/\s/.test(probe)) {
                    container = $.grep(container, function(n) {
                        return /\s/.test(n);
                    });
                } else {
                    container = $.grep(container, function(n) {
                        return !/\s/.test(n);
                    });
                }
            }
            while (final8.length < 9 && maxdif < 99) {
                temps = $.grep(container, function(n) {
                    return Math.abs(probe.length - n.length) <= maxdif;
                });
                if (temps.length > 0) {
                    final8.push(rchoice(temps));
                    container = $.grep(container, function(n) {
                        return final8[final8.length - 1] !== n;
                    });
                    if (index === 0 || index === 3) {
                        container = $.grep(container, function(n) {
                            return final8[final8.length - 1][0] !== n[0];
                        });
                    }
                } else {
                    maxdif++;
                }
            }
        }
        items_base2_temp.push(final8);
    });
    var days = range(1, 32);
    var day;
    var days_to_filt1 = [true_details_base[2]];
    items_base2_temp[1].forEach(function(month, index) {
        if (index > 0) {
            var days_to_filt2 = days_to_filt1;
            if (month == "february") {
                days_to_filt2 = days_to_filt1.concat([29, 30, 31]);
            } else {
                if (
                    $.inArray(month, [
                        "april",
                        "june",
                        "september",
                        "november"
                    ]) > -1
                ) {
                    days_to_filt2.push(31);
                }
            }
            var dys_temp = $.grep(days, function(a) {
                return $.inArray(a, days_to_filt2) == -1;
            });
            day = rchoice(dys_temp);
        } else {
            day = items_base2_temp[2][0];
        }
        items_base2_temp[2][index] = [month, day].join(" ");
        days_to_filt1.push(day);
    });
    items_base2_temp.splice(1, 1);
    items_base2 = items_base2_temp.slice(0, 1);
}

function select_meaningful() {
    window.countC0 = 0;
    window.words_to_filter = [
        []
    ];
    items_base2.forEach(function(categ, index1) {
        column = categ.slice(1, 9);
        column.forEach(function(word, ind) {
            column[ind] = word.toLowerCase();
        });
        column.sort();
        column.splice(randomdigit(1, 6), 0, "None");
        column.forEach(function(word, index2) {
            var id_full = ["#wo", index1, index2].join("");
            $(id_full).text(word);
        });
    });
    $(".words0").click(function() {
        var this_word = $(this).text();
        if (this_word == "None") {
            if ($(this).hasClass("turnedon")) {
                $(this).removeClass("turnedon");
                countC0 = 0;
            } else {
                if (countC0 === 0) {
                    $(this).addClass("turnedon");
                    countC0 = 9;
                }
            }
        } else {
            if ($(this).hasClass("turnedon")) {
                $(this).removeClass("turnedon");
                words_to_filter[0] = $.grep(words_to_filter[0], function(a) {
                    return a != this_word;
                });
                countC0--;
            } else {
                if (countC0 < 2) {
                    $(this).addClass("turnedon");
                    words_to_filter[0].push(this_word);
                    countC0++;
                }
            }
        }
    });
}


//validate selection of meaningful items
function check_selected() {
    var is_valid = true;
    if (countC0 === 0) {
        is_valid = false;
        alert(
            'Please select at least one country, or the option "None".'
        );
    }
    return is_valid;
}

var stim_base_6, the_targets = [],
    the_probes = [],
    the_nontargs = [];

function create_stim_base() {
    //creates all stimuli (a 6-item group - 1probe,1target,4irrelevants - for each of 4 different categories) from the given item and probe words
    var stim_base_temp = [
        []
    ];
    items_base2.forEach(function(categ, index) {
        var filtered_words = $.grep(categ, function(a) {
            return $.inArray(a, words_to_filter[index]) == -1;
        });
        var words_array = [];
        words_array = [filtered_words[0]].concat(
            shuffle(filtered_words.slice(1, 6))
        ); // for GUILTY
        words_array.forEach(function(word, num) {
            stim_base_temp[index].push({
                word: word,
                cat: categories[index]
            });
            if (0 === num) {
                stim_base_temp[index][num].type = "probe";
                the_probes.push(stim_base_temp[index][num].word);
                the_nontargs.push(stim_base_temp[index][num].word);
            } else if (1 == num) {
                stim_base_temp[index][num].type = "target";
                the_targets.push(stim_base_temp[index][num].word);
            } else {
                stim_base_temp[index][num].type = "irrelevant" + (num - 1);
                the_nontargs.push(stim_base_temp[index][num].word);
            }
        });
    });
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
    // if (condition == 0 || condition == 1) {
    //     window.stim_base = [stim_base_temp[0], stim_base_temp[1], stim_base_temp[0], stim_base_temp[1]]; // country, date
    // } else {
    //     window.stim_base = [stim_base_temp[1], stim_base_temp[0], stim_base_temp[1], stim_base_temp[0]]; // date, country
    // }
    window.stim_base = [stim_base_temp[0], stim_base_temp[0], stim_base_temp[0], stim_base_temp[0]]; // country, date
    window.stim_base_origcopy = JSON.parse(JSON.stringify(stim_base));

    window.stim_base_unique = JSON.parse(JSON.stringify(stim_base_temp));
    set_cit_type();
    set_block_texts();
}

function target_check() {
    if (
        $("#tcheck_countries")
        .val()
        .toUpperCase() != the_targets[0].toUpperCase()
    ) {
        alert("Wrong! Please check the detail more carefully!");
        $("#div_target_check").hide();
        $("#to_hide_id").hide();
        $("#div_target_intro").show();
        $("#tcheck_countries").val('');
        return false;
    } else {
        return true;
    }
}

var num_of_failed_fin = 11,
    failed_final = 0;

function final_probe_check() {
    if (validate_form(".form_check")) {
        var multip = 1 + 9 * failed_final;
        if (
            $("#fcheck_countries")
            .val()
            .toUpperCase() != the_probes[0].toUpperCase()
        ) {
            num_of_failed_fin += 1 * multip;
        }
        if (
            $("#fcheck_countries")
            .val()
            .toUpperCase() != the_probes[0].toUpperCase()
        ) {
            if (failed_final == 0) {
                failed_final = 1;
                alert("Wrong!");
                $("#final_check_feedback").html(
                    "<b>Your selection was not correct! It is crucial that you select this detail correctly. Please take your time and make sure you understand what you need to select.</b><br>"
                );
                return false;
            } else {
                failed_final = 2;
                return true;
            }
        } else {
            return true;
        }
    } else {
        return false;
    }
}
