#!perl

use 5.010001;
use strict;
use warnings;

use Text::Table::More qw/generate_table/;

my %table1 = (
    rows => [
        [{text=>'colspan=2,rowspan=2', colspan=>2, rowspan=>2}, {text=>'cell'}, {text=>'cell'}],
        [{text=>'colspan=2,rowspan=2', colspan=>2, rowspan=>2}],
        [{text=>'cell',tpad=>2}, {text=>'cell'}],
    ],
    separate_rows => 1,
    align => 'middle',
    valign => 'middle',
    lpad => 0,
    rpad => 0,
    tpad=>0,
    bpad=>0,
    #pad_char=>'x',
);

my %table2 = (
    rows => [
        [{text=>'colspan=3,rowspan=3', colspan=>3, rowspan=>3}, {text=>'cell1'}, {text=>'cell2'},{text=>'cell3'}],
        [{text=>'cell4'}, {text=>'cell5'},{text=>'cell6'}],
        [{text=>'colspan=3,rowspan=3', colspan=>3, rowspan=>3}],
        [{text=>'cell7'}, {text=>'cell8'},{text=>'cell9'}],
        [{text=>'cell10'}, {text=>'cell11'},{text=>'cell12'}],
    ],
    separate_rows => 1,
    align => 'middle',
    valign => 'middle',
    hpad => 0,
);


binmode(STDOUT, ":utf8");
say "Set BORDER_STYLE environment to see different border style, e.g. UTF8::SingleLineBold";
print generate_table(%table1);
say "";
print generate_table(%table2) if 0;
