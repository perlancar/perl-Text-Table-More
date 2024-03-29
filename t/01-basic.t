#!perl

use 5.010001;
use strict;
use utf8;
use warnings;
#use Test::Differences;
use Test::More 0.98;
#use Test::Needs;

use Text::Table::More qw(generate_table);

#binmode STDOUT, ":encoding(UTF-8)";
#binmode STDERR, ":encoding(UTF-8)";
#binmode STDOUT, ":utf8";
#binmode STDERR, ":utf8";

my @tests = (
    {
        name => 'empty',
        rows => [],
        result => '',
    },

    {
        name => '1x1',
        rows => [["A"]],
        result => <<'_',
.---.
| A |
`---'
_
    },

    {
        name => '1x2',
        rows => [["A","BBB"]],
        result => <<'_',
.---+-----.
| A | BBB |
`---+-----'
_
    },

    {
        name => '2x1',
        rows => [["A"],["BBB"]],
        result => <<'_',
.-----.
| A   |
| BBB |
`-----'
_
    },

    {
        name => '2x2',
        rows => [["A","BBB"], ["CC","D"]],
        result => <<'_',
.----+-----.
| A  | BBB |
| CC | D   |
`----+-----'
_
    },

    {
        name => '2x2 + header_row',
        rows => [["A","BBB"], ["CC","D"]],
        args => {header_row=>1},
        result => <<'_',
.----+-----.
| A  | BBB |
+====+=====+
| CC | D   |
`----+-----'
_
    },

    {
        name => '3x2 + header_row',
        rows => [["A","BBB"], ["CC","D"], ["F","GG"]],
        args => {header_row=>1},
        result => <<'_',
.----+-----.
| A  | BBB |
+====+=====+
| CC | D   |
| F  | GG  |
`----+-----'
_
    },

    {
        name => '3x2 + header_row + separate_rows',
        rows => [["A","BBB"], ["CC","D"], ["F","GG"]],
        args => {header_row=>1, separate_rows=>1},
        result => <<'_',
.----+-----.
| A  | BBB |
+====+=====+
| CC | D   |
+----+-----+
| F  | GG  |
`----+-----'
_
    },

    {
        name => 'multirow header',
        rows => [
            # header
            [{text=>'header1', rowspan=>2}, {text=>'header2',colspan=>2}, {text=>'header3',rowspan=>2}],
            ['header2a', 'header2b'],

            # data
            [1,2,3,4],
        ],
        args => {header_row=>2},
        result => <<'_',
.---------+---------------------+---------.
| header1 | header2             | header3 |
|         +----------+----------+         |
|         | header2a | header2b |         |
| 1       | 2        | 3        | 4       |
`---------+----------+----------+---------'
_
    },

    {
        name => 'row attr: bottom_border',
        rows => [["A","BBB"], ["CC","D"], ["E","FF"],["G","H"]],
        args => {
            header_row=>1,
            row_attrs => [
                [2, {bottom_border=>1}],
            ],
        },
        result => <<'_',
.----+-----.
| A  | BBB |
+====+=====+
| CC | D   |
| E  | FF  |
+----+-----+
| G  | H   |
`----+-----'
_
    },

    {
        name => 'rowspan',
        rows => [
            ["A0","B0"],
            [{text=>"A12",rowspan=>2},"B1"],
            ["B2"],
            ["A3", {text=>"B34",rowspan=>2}],
            ["A4"],
        ],
        args => {header_row=>1, separate_rows=>1},
        result => <<'_',
.-----+-----.
| A0  | B0  |
+=====+=====+
| A12 | B1  |
|     +-----+
|     | B2  |
+-----+-----+
| A3  | B34 |
+-----+     |
| A4  |     |
`-----+-----'
_
    },

    {
        name => 'row height: rowspan longer',
        rows => [["A0","B0"], [{text=>"A1L1\nL2\nL3\nL4",rowspan=>2},"B1"], ["B2"]],
        args => {header_row=>1, separate_rows=>1},
        result => <<'_',
.------+----.
| A0   | B0 |
+======+====+
| A1L1 | B1 |
| L2   |    |
| L3   +----+
| L4   | B2 |
`------+----'
_
    },

    {
        name => 'colspan',
        rows => [
            [{text=>"AB0",colspan=>2},"C0"],
            ["A1", {text=>"BC1",colspan=>2}],
            [{text=>"AB2",colspan=>2},"C2"],
            ["A3","B3","C3"],
        ],
        args => {header_row=>1, separate_rows=>1},
        result => <<'_',
.---------+----.
| AB0     | C0 |
+====+====+====+
| A1 | BC1     |
+----+----+----+
| AB2     | C2 |
+----+----+----+
| A3 | B3 | C3 |
`----+----+----'
_
    },

    {
        name => 'rowcolspan 1',
        rows => [
            ["A0","B0","C0"],
            [{text=>"AB12L1\nL2\nL3",rowspan=>2,colspan=>2},"C1"],
            ["C2"],
            ["A3","B3","C3"],
        ],
        args => {header_row=>1, separate_rows=>1},
        result => <<'_',
.----+----+----.
| A0 | B0 | C0 |
+====+====+====+
| AB12L1  | C1 |
| L2      +----+
| L3      | C2 |
+----+----+----+
| A3 | B3 | C3 |
`----+----+----'
_
    },

    {
        name => 'rowcolspan 2',
        rows => [
            ["A0","B0","C0"],
            ["A1","B1","C1"],
            ["A2", {text=>"BC23L1\nL2\nL3",rowspan=>2,colspan=>2}],
            ["A3"],
        ],
        args => {header_row=>1, separate_rows=>1},
        result => <<'_',
.----+----+----.
| A0 | B0 | C0 |
+====+====+====+
| A1 | B1 | C1 |
+----+----+----+
| A2 | BC23L1  |
+----+ L2      |
| A3 | L3      |
`----+---------'
_
    },

    {
        name => 'align',
        rows => [
            [{text=>"A0", align=>'middle'},"B0","C0"], # cell attrs (hash cell)
            ["A1L1\nL2","B1","C1"],
            ["A2", {text=>"BC23L1\nL2\nL3",rowspan=>2,colspan=>2}],
            ["A3"],
        ],
        args => {
            align => 'right', # table arg
            header_row=>1,
            separate_rows=>1,
            row_attrs => [
                [1, {align=>'left'}], # row attrs
            ],
            col_attrs => [
                [1, {align=>'left'}], # col attrs
            ],
            cell_attrs => [
                [2, 0, {align=>'left'}], # cell attrs
            ],
        },
        result => <<'_',
.------+----+----.
|  A0  | B0 | C0 |
+======+====+====+
| A1L1 | B1 | C1 |
| L2   |    |    |
+------+----+----+
| A2   | BC23L1  |
+------+ L2      |
|   A3 | L3      |
`------+---------'
_
    },

    {
        test_if => sub {
            return 0 unless eval { require "Text/ANSI/Util.pm"; 1 };
            return 0 unless eval { require "Text/WideChar/Util.pm"; 1 };
        },
        name => 'wide_char & color',
        rows => [
            [{text=>"⻅", align=>'middle'},"B0","C0"], # cell attrs (hash cell)
            ["A1L1\nL2","B1","C1"],
            ["A2", {text=>"\e[31mBC23L1\nL2⻅\nL3\e[0m",rowspan=>2,colspan=>2}],
            ["A3"],
        ],
        args => {
            align => 'right', # table arg
            header_row=>1,
            separate_rows=>1,
            row_attrs => [
                [1, {align=>'left'}], # row attrs
            ],
            col_attrs => [
                [1, {align=>'left'}], # col attrs
            ],
            cell_attrs => [
                [2, 0, {align=>'left'}], # cell attrs
            ],
            wide_char => 1,
            color => 1,
        },
        result => <<"_",
.------+----+----.
|  ⻅  | B0 | C0 |
+======+====+====+
| A1L1 | B1 | C1 |
|\e[0m L2   |    |    |
+------+----+----+
| A2   | \e[31mBC23L1 \e[0m |
+------+\e[31m L2⻅ \e[0m   |
|   A3 |\e[31m L3\e[0m      |
`------+---------'
_
    },

    {
        name => 'valign',
        rows => [
            [{text=>"A0", valign=>'middle'},"B0L1\nL2\nL3","C0"], # cell attrs in cell (A0)
            ["A1L1\nL2\nL3","B1","C1"],
            ["A2L1\nL2\nL3", {text=>"BC23L1\nL2\nL3",rowspan=>2,colspan=>2}],
            ["A3"],
        ],
        args => {
            valign => 'bottom', # table arg (e.g. C0)
            header_row=>1,
            separate_rows=>1,
            row_attrs => [
                [1, {valign=>'top'}], # row attrs (B1)
            ],
            col_attrs => [
                [1, {valign=>'top'}], # col attrs (BC23)
            ],
            cell_attrs => [
                [1, 2, {valign=>'middle'}], # cell attrs (C1)
            ],
        },
        result => <<'_',
.------+------+----.
|      | B0L1 |    |
| A0   | L2   |    |
|      | L3   | C0 |
+======+======+====+
| A1L1 | B1   |    |
| L2   |      | C1 |
| L3   |      |    |
+------+------+----+
| A2L1 | BC23L1    |
| L2   | L2        |
| L3   | L3        |
+------+           |
| A3   |           |
`------+-----------'
_
    },

    {
        name => 'lpad=0, rpad=0, vpad=>1',
        rows => [["A","BBB"], ["CC","D"]],
        args => {lpad=>0, rpad=>0, vpad=>1},
        result => <<'_',
.--+---.
|  |   |
|A |BBB|
|  |   |
|  |   |
|CC|D  |
|  |   |
`--+---'
_
    },
    {
        name => 'lpad=2, rpad=>3',
        rows => [["A","BBB"], ["CC","D"]],
        args => {lpad=>2, rpad=>3},
        result => <<'_',
.-------+--------.
|  A    |  BBB   |
|  CC   |  D     |
`-------+--------'
_
    },
    {
        name => 'tpad=1, bpad=>2, hpad=>0',
        rows => [["A","BBB"], ["CC","D"]],
        args => {tpad=>1, bpad=>2, hpad=>0},
        result => <<'_',
.--+---.
|  |   |
|A |BBB|
|  |   |
|  |   |
|  |   |
|CC|D  |
|  |   |
|  |   |
`--+---'
_
    },
    {
        name => 'cell attr: tpad,bpad,lpad,rpad',
        rows => [[{text=>"A",lpad=>2},{text=>"BBB", rpad=>0}], [{text=>"CC",tpad=>1},{text=>"D",bpad=>3}]],
        args => {},
        result => <<'_',
.----+----.
|  A | BBB|
|    | D  |
| CC |    |
|    |    |
|    |    |
`----+----'
_
    },
    # XXX per-cell attr: {h,v}pad
    # XXX per-row  attr: {h,v,t,b,l,r}pad
    # XXX per-col  attr: {h,v,t,b,l,r}pad
);
my @include_tests = (); #("wide_char & color"); # e.g. ("1x1")
my $border_style;# = "UTF8::SingleLineBoldHeader"; # force border style

for my $test (@tests) {
    if (@include_tests) {
        next unless grep { $_ eq $test->{name} } @include_tests;
    }
    if ($test->{test_if}) {
        unless ($test->{test_if}->()) {
            diag "Test $test->{name} skipped (test_if returns false)";
            next;
        }
    }
    subtest $test->{name} => sub {
        my $res = generate_table(
            rows => $test->{rows},
            ($test->{args} ? %{$test->{args}} : ()),
            ($border_style ? (border_style=>$border_style) : ()),
        );
        is($res, $test->{result}) or diag "expected:\n$test->{result}\nresult:\n$res";
        #eq_or_diff($res, $test->{result}); # sometimes show output as one line, not very useful
        #use DDC; is($res, $test->{result}) or diag "expected:\n".DDC::dump($test->{result})."\nresult:\n".DDC::dump($res);
    };
}

done_testing;
