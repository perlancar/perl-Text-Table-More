package Text::Table::More;

use 5.010001;
use strict;
use warnings;
#use utf8;

# AUTHORITY
# DATE
# DIST
# VERSION

# see Module::Features for more details on this
our %FEATURES = (
    set_v => {
        TextTable => 1,
    },

    features => {
        PerlTrove => {
            "Development Status" => "4 - Beta",
            "Environment" => "Console",
            # Framework
            "Intended Audience" => ["Developers"],
            "License" => "OSI Approved :: Artistic License",
            # Natural Language
            # Operating System
            "Programming Language" => "Perl",
            "Topic" => ["Software Development :: Libraries :: Perl Modules", "Utilities"],
            # Typing
        },

        TextTable => {
            can_align_cell_containing_wide_character => 1,
            can_align_cell_containing_color_code     => 1,
            can_align_cell_containing_newline        => 1,
            can_use_box_character                    => 1,
            can_customize_border                     => 1,
            can_halign                               => 1,
            can_halign_individual_row                => 1,
            can_halign_individual_column             => 1,
            can_halign_individual_cell               => 1,
            can_valign                               => 1,
            can_valign_individual_row                => 1,
            can_valign_individual_column             => 1,
            can_valign_individual_cell               => 1,
            can_rowspan                              => 1,
            can_colspan                              => 1,
            can_color                                => 0,
            can_color_theme                          => 0,
            can_set_cell_height                      => 0,
            can_set_cell_height_of_individual_row    => 0,
            can_set_cell_width                       => 0,
            can_set_cell_width_of_individual_column  => 0,
            speed                                    => 'slow',
            can_hpad                                 => 1,
            can_hpad_individual_row                  => 1,
            can_hpad_individual_column               => 1,
            can_hpad_individual_cell                 => 1,
            can_vpad                                 => 1,
            can_vpad_individual_row                  => 1,
            can_vpad_individual_column               => 1,
            can_vpad_individual_cell                 => 1,
        },
    },
);

use List::AllUtils qw(first firstidx max);

use Exporter qw(import);
our @EXPORT_OK = qw/ generate_table /;

our $_split_lines_func;
our $_pad_func;
our $_length_height_func;

# consts
sub IDX_EXPTABLE_CELL_ROWSPAN()         {0} # number of rowspan, only defined for the rowspan head
sub IDX_EXPTABLE_CELL_COLSPAN()         {1} # number of colspan, only defined for the colspan head
sub IDX_EXPTABLE_CELL_WIDTH()           {2} # visual width. this does not include the cell padding.
sub IDX_EXPTABLE_CELL_HEIGHT()          {3} # visual height. this does not include row separator.
sub IDX_EXPTABLE_CELL_ORIG()            {4} # str/hash
sub IDX_EXPTABLE_CELL_IS_ROWSPAN_TAIL() {5} # whether this cell is tail of a rowspan
sub IDX_EXPTABLE_CELL_IS_COLSPAN_TAIL() {6} # whether this cell is tail of a colspan
sub IDX_EXPTABLE_CELL_ORIG_ROWNUM()     {7} #
sub IDX_EXPTABLE_CELL_ORIG_COLNUM()     {8} #
sub IDX_EXPTABLE_CELL_TEXT()            {9} # cell text (modified: padded)

# whether an exptable cell is the head (1st cell) or tail (the rest) of a
# rowspan/colspan. these should be macros if possible, for speed.
sub _exptable_cell_is_rowspan_tail { defined($_[0]) &&  $_[0][IDX_EXPTABLE_CELL_IS_ROWSPAN_TAIL] }
sub _exptable_cell_is_colspan_tail { defined($_[0]) &&  $_[0][IDX_EXPTABLE_CELL_IS_COLSPAN_TAIL] }
sub _exptable_cell_is_tail         { defined($_[0]) && ($_[0][IDX_EXPTABLE_CELL_IS_ROWSPAN_TAIL] || $_[0][IDX_EXPTABLE_CELL_IS_COLSPAN_TAIL]) }
sub _exptable_cell_is_rowspan_head { defined($_[0]) && !$_[0][IDX_EXPTABLE_CELL_IS_ROWSPAN_TAIL] }
sub _exptable_cell_is_colspan_head { defined($_[0]) && !$_[0][IDX_EXPTABLE_CELL_IS_COLSPAN_TAIL] }
sub _exptable_cell_is_head         { defined($_[0]) && defined $_[0][IDX_EXPTABLE_CELL_ORIG] }

sub _divide_int_to_n_ints {
    my ($int, $n) = @_;
    my $subtot = 0;
    my $int_subtot = 0;
    my $prev_int_subtot = 0;
    my @ints;
    for (1..$n) {
        $subtot += $int/$n;
        $int_subtot = sprintf "%.0f", $subtot;
        push @ints, $int_subtot - $prev_int_subtot;
        $prev_int_subtot = $int_subtot;
    }
    @ints;
}

sub _vpad {
    my ($lines, $num_lines, $width, $which, $pad_char) = @_;
    return $lines if @$lines >= $num_lines; # we don't do truncate
    my @vpadded_lines;
    my $pad_line = $pad_char x $width;
    if ($which =~ /^b/) { # bottom padding
        push @vpadded_lines, @$lines;
        push @vpadded_lines, $pad_line for @$lines+1 .. $num_lines;
    } elsif ($which =~ /^t/) { # top padding
        push @vpadded_lines, $pad_line for @$lines+1 .. $num_lines;
        push @vpadded_lines, @$lines;
    } else { # center padding
        my $p  = $num_lines - @$lines;
        my $p1 = int($p/2);
        my $p2 = $p - $p1;
        push @vpadded_lines, $pad_line for 1..$p1;
        push @vpadded_lines, @$lines;
        push @vpadded_lines, $pad_line for 1..$p2;
    }
    \@vpadded_lines;
}

sub _get_attr {
    my ($attr_name, $y, $x, $cell_value, $table_args) = @_;

  CELL_ATTRS_FROM_CELL_VALUE: {
        last unless ref $cell_value eq 'HASH';
        my $attr_val = $cell_value->{$attr_name};
        return $attr_val if defined $attr_val;
    }

  CELL_ATTRS_FROM_CELL_ATTRS_ARG:
    {
        last unless defined $x && defined $y;
        my $cell_attrs = $table_args->{cell_attrs};
        last unless $cell_attrs;
        for my $entry (@$cell_attrs) {
            next unless $entry->[0] == $y && $entry->[1] == $x;
            my $attr_val = $entry->[2]{$attr_name};
            return $attr_val if defined $attr_val;
        }
    }

  COL_ATTRS:
    {
        last unless defined $x;
        my $col_attrs = $table_args->{col_attrs};
        last unless $col_attrs;
        for my $entry (@$col_attrs) {
            next unless $entry->[0] == $x;
            my $attr_val = $entry->[1]{$attr_name};
            return $attr_val if defined $attr_val;
        }
    }

  ROW_ATTRS:
    {
        last unless defined $y;
        my $row_attrs = $table_args->{row_attrs};
        last unless $row_attrs;
        for my $entry (@$row_attrs) {
            next unless $entry->[0] == $y;
            my $attr_val = $entry->[1]{$attr_name};
            return $attr_val if defined $attr_val;
        }
    }

  TABLE_ARGS:
    {
        my $attr_val = $table_args->{$attr_name};
        return $attr_val if defined $attr_val;
    }

    undef;
}

sub _get_exptable_cell_lines {
    my ($table_args, $exptable, $row_heights, $column_widths,
        $bottom_borders, $intercol_width, $y, $x) = @_;

    my $exptable_cell = $exptable->[$y][$x];
    my $cell     = $exptable_cell->[IDX_EXPTABLE_CELL_ORIG];
    my $text     = $exptable_cell->[IDX_EXPTABLE_CELL_TEXT];
    my $align    = _get_attr('align', $y, $x, $cell, $table_args) // 'left';
    my $valign   = _get_attr('valign', $y, $x, $cell, $table_args) // 'top';
    my $pad      = $align eq 'left' ? 'r' : $align eq 'right' ? 'l' : 'c';
    my $vpad     = $valign eq 'top' ? 'b' : $valign eq 'bottom' ? 't' : 'c';
    my $pad_char = $table_args->{pad_char};
    my $height = 0;
    my $width  = 0;
    for my $ic (1..$exptable_cell->[IDX_EXPTABLE_CELL_COLSPAN]) {
        $width += $column_widths->[$x+$ic-1];
        $width += $intercol_width if $ic > 1;
    }
    for my $ir (1..$exptable_cell->[IDX_EXPTABLE_CELL_ROWSPAN]) {
        $height += $row_heights->[$y+$ir-1];
        $height++ if $bottom_borders->[$y+$ir-2] && $ir > 1;
    }

    my @datalines = map { $_pad_func->($_, $width, $pad, $pad_char, 'truncate') }
        ($_split_lines_func->($text));
    _vpad(\@datalines, $height, $width, $vpad, $pad_char);
}

sub generate_table {
    require Module::Load::Util;
    require Text::NonWideChar::Util;

    my %args = @_;
    $args{header_row} //= 0; my $header_row = $args{header_row};
    $args{pad_char} //= ' ';
    $args{hpad} //= 1;
    $args{vpad} //= 0;

    my $rows = $args{rows} or die "Please specify rows";
    my $bs_name = $args{border_style} //
        $ENV{PERL_TEXT_TABLE_MORE_BORDER_STYLE} //
        $ENV{BORDER_STYLE} //
        'ASCII::SingleLineDoubleAfterHeader';
    my $cell_attrs = $args{cell_attrs} // [];

    my $bs_obj = Module::Load::Util::instantiate_class_with_optional_args({ns_prefix=>"BorderStyle"}, $bs_name);

  DETERMINE_CODES: {
        my $color = $args{color};
        my $wide_char = $args{wide_char};

        # split_lines
        if ($color) {
            require Text::ANSI::Util;
            $_split_lines_func = sub { Text::ANSI::Util::ta_add_color_resets(split /\R/, $_[0]) };
        } else {
            $_split_lines_func = sub { split /\R/, $_[0] };
        }

        # pad & length_height
        if ($color) {
            if ($wide_char) {
                require Text::ANSI::WideUtil;
                $_pad_func           = \&Text::ANSI::WideUtil::ta_mbpad;
                $_length_height_func = \&Text::ANSI::WideUtil::ta_mbswidth_height;
            } else {
                require Text::ANSI::Util;
                $_pad_func           = \&Text::ANSI::Util::ta_pad;
                $_length_height_func = \&Text::ANSI::Util::ta_length_height;
            }
        } else {
            if ($wide_char) {
                require Text::WideChar::Util;
                $_pad_func           = \&Text::WideChar::Util::mbpad;
                $_length_height_func = \&Text::WideChar::Util::mbswidth_height;
            } else {
                require String::Pad;
                require Text::NonWideChar::Util;
                $_pad_func           = \&String::Pad::pad;
                $_length_height_func = \&Text::NonWideChar::Util::length_height;
            }
        }
    }

    # XXX when we allow cell attrs right_border and left_border, this will
    # become array too like $exptable_bottom_borders.
    my $intercol_width = length($bs_obj->get_border_char(char=>'v_i'));

    my $exptable = []; # [ [[$orig_rowidx,$orig_colidx,$rowspan,$colspan,...], ...], [[...], ...], ... ]
    my $exptable_bottom_borders = []; # idx=exptable rownum, val=bool
    my $M = 0; # number of rows in the exptable
    my $N = 0; # number of columns in the exptable
  CONSTRUCT_EXPTABLE: {
        # 1. the first step is to construct a 2D array we call "exptable" (short
        # for expanded table), which is like the original table but with all the
        # spanning rows/columns split into the smaller boxes so it's easier to
        # draw later. for example, a table cell with colspan=2 will become 2
        # exptable cells. an m-row x n-column table will become M-row x N-column
        # exptable, where M>=m, N>=n.

        my $rownum;

        # 1a. first substep: construct exptable and calculate everything except
        # each exptable cell's width and height, because this will require
        # information from the previous substeps.

        $rownum = -1;
        for my $row (@$rows) {
            $rownum++;
            my $colnum = -1;
            my $separator_type = do {
                my $cmp = $header_row-1 <=> $rownum;
                # 0=none, 2=separator between header/data, 4=separator between
                # data rows, 8=separator between header rows. this is from
                # BorderStyle standard.
                $cmp==0 ? 2 : $cmp==1 ? 8 : 4;
            };
            $exptable->[$rownum] //= [];
            push @{ $exptable->[$rownum] }, undef
                if (@{ $exptable->[$rownum] } == 0 ||
                defined($exptable->[$rownum][-1]));
            #use DDC; say "D:exptable->[$rownum] = ", DDC::dump($exptable->[$rownum]);
            my $exptable_colnum = firstidx {!defined} @{ $exptable->[$rownum] };
            #say "D:rownum=$rownum, exptable_colnum=$exptable_colnum";
            if ($exptable_colnum == -1) { $exptable_colnum = 0 }
            $exptable_bottom_borders->[$rownum] //= $args{separate_rows} ? $separator_type : 0;

            for my $cell (@$row) {
                $colnum++;
                my $text;

                my $rowspan = 1;
                my $colspan = 1;
                if (ref $cell eq 'HASH') {
                    $text = $cell->{text};
                    $rowspan = $cell->{rowspan} if $cell->{rowspan};
                    $colspan = $cell->{colspan} if $cell->{colspan};
                } else {
                    $text = $cell;
                    my $el;
                    $el = first {$_->[0] == $rownum && $_->[1] == $colnum && $_->[2]{rowspan}} @$cell_attrs;
                    $rowspan = $el->[2]{rowspan} if $el;
                    $el = first {$_->[0] == $rownum && $_->[1] == $colnum && $_->[2]{colspan}} @$cell_attrs;
                    $colspan = $el->[2]{colspan} if $el;
                }

                my @widths;
                my @heights;
              ROW:
                for my $ir (1..$rowspan) {
                    for my $ic (1..$colspan) {
                        my $exptable_cell;
                        $exptable->[$rownum+$ir-1][$exptable_colnum+$ic-1] = $exptable_cell = [];

                        $exptable_cell->[IDX_EXPTABLE_CELL_ORIG_ROWNUM] = $rownum;
                        $exptable_cell->[IDX_EXPTABLE_CELL_ORIG_COLNUM] = $colnum;

                        if ($ir == 1 && $ic == 1) {
                            $exptable_cell->[IDX_EXPTABLE_CELL_ROWSPAN]     = $rowspan;
                            $exptable_cell->[IDX_EXPTABLE_CELL_COLSPAN]     = $colspan;
                            $exptable_cell->[IDX_EXPTABLE_CELL_ORIG]        = $cell;
                        } else {
                            $exptable_cell->[IDX_EXPTABLE_CELL_IS_ROWSPAN_TAIL] = 1 if $ir > 1;
                            $exptable_cell->[IDX_EXPTABLE_CELL_IS_COLSPAN_TAIL] = 1 if $ic > 1;
                        }
                        #use DDC; dd $exptable; say ''; # debug
                    }

                    # determine whether we should draw bottom border of each row
                    if ($rownum+$ir-1 == 0 && $header_row > 0) {
                        $exptable_bottom_borders->[0] = $separator_type;
                    } else {
                        my $val;
                        $val = _get_attr('bottom_border', $rownum+$ir-1, 0, $cell, \%args);     $exptable_bottom_borders->[$rownum+$ir-1] = $separator_type if $val;
                        $val = _get_attr('top_border'   , $rownum+$ir-1, 0, $cell, \%args);     $exptable_bottom_borders->[$rownum+$ir-2] = $separator_type if $val;
                        $val = _get_attr('bottom_border', $rownum+$ir-1, undef, undef, \%args); $exptable_bottom_borders->[$rownum+$ir-1] = $separator_type if $val;
                        $val = _get_attr('top_border'   , $rownum+$ir-1, undef, undef, \%args); $exptable_bottom_borders->[$rownum+$ir-2] = $separator_type if $val;
                    }

                    $M = $rownum+$ir if $M < $rownum+$ir;
                }

                $exptable_colnum += $colspan;
                $exptable_colnum++ while defined $exptable->[$rownum][$exptable_colnum];

            } # for a row
            $N = $exptable_colnum if $N < $exptable_colnum;
        } # for rows

        # 1b. calculate the heigth and width of each exptable cell (as required
        # by the text, or specified width/height when we allow cell attrs width,
        # height)

        for my $exptable_rownum (0..$M-1) {
            for my $exptable_colnum (0..$N-1) {
                my $exptable_cell = $exptable->[$exptable_rownum][$exptable_colnum];
                next if _exptable_cell_is_tail($exptable_cell);
                my $rowspan = $exptable_cell->[IDX_EXPTABLE_CELL_ROWSPAN];
                my $colspan = $exptable_cell->[IDX_EXPTABLE_CELL_COLSPAN];
                my $cell = $exptable_cell->[IDX_EXPTABLE_CELL_ORIG];
                my $text = ref $cell eq 'HASH' ? $cell->{text} : $cell;

                # pad the text, put in exptable text
                my $hpad = _get_attr('hpad', $exptable_rownum, $exptable_colnum, $cell, \%args);
                my $lpad = _get_attr('lpad', $exptable_rownum, $exptable_colnum, $cell, \%args) // $hpad;
                my $rpad = _get_attr('rpad', $exptable_rownum, $exptable_colnum, $cell, \%args) // $hpad;
                my $vpad = _get_attr('vpad', $exptable_rownum, $exptable_colnum, $cell, \%args);
                my $tpad = _get_attr('tpad', $exptable_rownum, $exptable_colnum, $cell, \%args) // $vpad;
                my $bpad = _get_attr('bpad', $exptable_rownum, $exptable_colnum, $cell, \%args) // $vpad;
                my $pad_char = $args{pad_char};
                if ($lpad > 0) { my $p = $pad_char x $lpad; $text =~ s/^/$p/gm }
                if ($rpad > 0) { my $p = $pad_char x $rpad; $text =~ s/$/$p/gm }
                if ($tpad > 0) { $text = ("\n" x $tpad) . $text }
                if ($bpad > 0) { $text = $text . ("\n$pad_char" x $bpad) }
                $exptable_cell->[IDX_EXPTABLE_CELL_TEXT] = $text;

                my $lh = $_length_height_func->($text);
                #use DDC; dd $text;
                #use DDC; say "D:length_height[$exptable_rownum,$exptable_colnum] = (".DDC::dump($text)."): ".DDC::dump($lh);
                my $tot_intercol_widths = ($colspan-1) * $intercol_width;
                my $tot_interrow_heights = 0; for (1..$rowspan-1) { $tot_interrow_heights++ if $exptable_bottom_borders->[$exptable_rownum+$_-1] }
                #say "D:tot_intercol_widths=$tot_intercol_widths";
                #say "D:to_interrow_heights=$tot_interrow_heights";
                my @heights = _divide_int_to_n_ints(max(0, $lh->[1] - $tot_interrow_heights), $rowspan);
                my @widths  = _divide_int_to_n_ints(max(0, $lh->[0] - $tot_intercol_widths ), $colspan);
                #use DDC; say "D:split widths:", DDC::dump(\@widths), ", split heights:", DDC::dump(\@heights);
                for my $ir (1..$rowspan) {
                    for my $ic (1..$colspan) {
                        $exptable->[$exptable_rownum+$ir-1][$exptable_colnum+$ic-1][IDX_EXPTABLE_CELL_HEIGHT]  = $heights[$ir-1];
                        $exptable->[$exptable_rownum+$ir-1][$exptable_colnum+$ic-1][IDX_EXPTABLE_CELL_WIDTH]   = $widths [$ic-1];
                    }
                }
            }
        } # for rows

    } # CONSTRUCT_EXPTABLE
    #use DDC; dd $exptable; # debug
    #print "D: exptable size: $M x $N (HxW)\n"; # debug
    #use DDC; print "bottom borders: "; dd $exptable_bottom_borders; # debug

  OPTIMIZE_EXPTABLE: {
        # TODO

        # 2. we reduce extraneous columns and rows if there are colspan that are
        # too many. for example, if all exptable cells in column 1 has colspan=2
        # (or one row has colspan=2 and another row has colspan=3), we might as
        # remove 1 column because the extra column span doesn't have any
        # content. same case for extraneous row spans.

        # 2a. remove extra undefs. skip this. doesn't make a difference.
        #for my $exptable_row (@{ $exptable }) {
        #    splice @$exptable_row, $N if @$exptable_row > $N;
        #}

        1;
    } # OPTIMIZE_EXPTABLE
    #use DDC; dd $exptable; # debug

    my $exptable_column_widths  = []; # idx=exptable colnum
    my $exptable_row_heights    = []; # idx=exptable rownum
  DETERMINE_SIZE_OF_EACH_EXPTABLE_COLUMN_AND_ROW: {
        # 3. before we draw the exptable, we need to determine the width and
        # height of each exptable column and row.
        #use DDC;
        for my $ir (0..$M-1) {
            my $exptable_row = $exptable->[$ir];
            $exptable_row_heights->[$ir] = max(
                1, map {$_->[IDX_EXPTABLE_CELL_HEIGHT] // 0} @$exptable_row);
        }

        for my $ic (0..$N-1) {
            $exptable_column_widths->[$ic] = max(
                1, map {$exptable->[$_][$ic] ? $exptable->[$_][$ic][IDX_EXPTABLE_CELL_WIDTH] : 0} 0..$M-1);
        }
    } # DETERMINE_SIZE_OF_EACH_EXPTABLE_COLUMN_AND_ROW
    #use DDC; print "column widths: "; dd $exptable_column_widths; # debug
    #use DDC; print "row heights: "; dd $exptable_row_heights; # debug

    # each elem is an arrayref containing characters to render a line of the
    # table, e.g. for element [0] the row is all borders. for element [1]:
    # [$left_border_str, $exptable_cell_content1, $border_between_col,
    # $exptable_cell_content2, ...]. all will be joined together with "\n" to
    # form the final rendered table.
    my @buf;

  DRAW_EXPTABLE: {
        # 4. finally we draw the (exp)table.

        my $y = 0;

        for my $ir (0..$M-1) {
          DRAW_TOP_BORDER:
            {
                last unless $ir == 0;
                my $row_is_header = $header_row > $exptable->[$ir][0][IDX_EXPTABLE_CELL_ORIG_ROWNUM];
                my %gbcargs = (for_header_row=>$row_is_header ? 1:0);
                my $b_topleft    = $bs_obj->get_border_char(char=>'rd_t', %gbcargs);
                my $b_topline    = $bs_obj->get_border_char(char=>'h_t', %gbcargs);
                my $b_topbetwcol = $bs_obj->get_border_char(char=>'hd_t', %gbcargs);
                my $b_topright   = $bs_obj->get_border_char(char=>'ld_t', %gbcargs);
                last unless length $b_topleft || length $b_topline || length $b_topbetwcol || length $b_topright;
                $buf[$y][0] = $b_topleft;
                for my $ic (0..$N-1) {
                    my $cell_right = $ic < $N-1 ? $exptable->[$ir][$ic+1] : undef;
                    my $cell_right_has_content = defined $cell_right && _exptable_cell_is_head($cell_right);
                    $buf[$y][$ic*4+2] = $bs_obj->get_border_char(char=>'h_t', repeat=>$exptable_column_widths->[$ic], %gbcargs); # +1, +2, +3
                    $buf[$y][$ic*4+4] = $ic == $N-1 ? $b_topright : ($cell_right_has_content ? $b_topbetwcol : $b_topline);
                }
                $y++;
            } # DRAW_TOP_BORDER

            # DRAW_DATA_OR_HEADER_ROW
            {
                # draw leftmost border, which we always do.
                for my $i (1 .. $exptable_row_heights->[$ir]) {
                    my $row_is_header = $header_row > $exptable->[$ir][0][IDX_EXPTABLE_CELL_ORIG_ROWNUM];
                    my %gbcargs = (for_header_row=>$row_is_header ? 1:0);
                    $buf[$y+$i-1][0] = $bs_obj->get_border_char(char=>'v_l', %gbcargs);
                }

                my $lines;
                for my $ic (0..$N-1) {
                    my $cell = $exptable->[$ir][$ic];

                    my $row_is_header = $header_row > $cell->[IDX_EXPTABLE_CELL_ORIG_ROWNUM];
                    my %gbcargs = (for_header_row=>$row_is_header ? 1:0);

                    # draw cell content. also possibly draw border between
                    # cells. we don't draw border inside a row/colspan.
                    if (_exptable_cell_is_head($cell)) {
                        $lines = _get_exptable_cell_lines(
                            \%args, $exptable, $exptable_row_heights, $exptable_column_widths,
                            $exptable_bottom_borders, $intercol_width, $ir, $ic);
                        for my $i (0..$#{$lines}) {
                            $buf[$y+$i][$ic*4+0] = $bs_obj->get_border_char(char=>'v_i', %gbcargs);
                            $buf[$y+$i][$ic*4+1] = "";
                            $buf[$y+$i][$ic*4+2] = $lines->[$i];
                            $buf[$y+$i][$ic*4+3] = "";
                        }
                        #use DDC; say "D: Drawing exptable_cell($ir,$ic): ", DDC::dump($lines);
                    }

                    # draw rightmost border, which we always do.
                    if ($ic == $N-1) {
                        for my $i (1 .. $exptable_row_heights->[$ir]) {
                            $buf[$y+$i-1][$ic*4+4] = $bs_obj->get_border_char(char=>'v_r', %gbcargs);
                        }
                    }

                }
            } # DRAW_DATA_OR_HEADER_ROW
            $y += $exptable_row_heights->[$ir];

          DRAW_ROW_SEPARATOR:
            {
                last unless $ir < $M-1;
                last unless $exptable_bottom_borders->[$ir];

                my $_rownum = $exptable->[$ir][0][IDX_EXPTABLE_CELL_ORIG_ROWNUM];
                my %gbcargs;
                if ($_rownum == $header_row-1) {
                    $gbcargs{for_header_data_separator} = 1;
                } elsif ($_rownum < $header_row-1) {
                    $gbcargs{for_header_row} = 1;
                }

                my $b_betwrowleft    = $bs_obj->get_border_char(char=>'rv_l', %gbcargs);
                my $b_betwrowline    = $bs_obj->get_border_char(char=>'v_i', %gbcargs);
                my $b_betwrowbetwcol = $bs_obj->get_border_char(char=>'hv_i', %gbcargs);
                my $b_betwrowright   = $bs_obj->get_border_char(char=>'lv_r', %gbcargs);
                last unless length $b_betwrowleft || length $b_betwrowline || length $b_betwrowbetwcol || length $b_betwrowright;
                my $b_betwrowbetwcol_notop = $bs_obj->get_border_char(char=>'hd_i', %gbcargs);
                my $b_betwrowbetwcol_nobot = $bs_obj->get_border_char(char=>'hu_i', %gbcargs);
                my $b_betwrowbetwcol_noleft  = $bs_obj->get_border_char(char=>'rv_i', %gbcargs);
                my $b_betwrowbetwcol_noright = $bs_obj->get_border_char(char=>'lv_i', %gbcargs);
                my $b_dataorheaderrowleft    = $bs_obj->get_border_char(char=>'v_l', %gbcargs);
                my $b_dataorheaderrowbetwcol = $bs_obj->get_border_char(char=>'v_i', %gbcargs);
                my $b_dataorheaderrowright   = $bs_obj->get_border_char(char=>'v_r', %gbcargs);
                for my $ic (0..$N-1) {
                    my $cell_right       = $ic < $N-1 ? $exptable->[$ir][$ic+1] : undef;
                    my $cell_bottom      = $ir < $M-1 ? $exptable->[$ir+1][$ic] : undef;
                    my $cell_rightbottom = $ir < $M-1 && $ic < $N-1 ? $exptable->[$ir+1][$ic+1] : undef;

                    # leftmost border
                    if ($ic == 0) {
                        $buf[$y][0] = _exptable_cell_is_rowspan_tail($cell_bottom) ? $b_dataorheaderrowleft : $b_betwrowleft;
                    }

                    # along the width of cell content
                    if (_exptable_cell_is_rowspan_head($cell_bottom)) {
                        $buf[$y][$ic*4+2] = $bs_obj->get_border_char(char=>'h_i', repeat=>$exptable_column_widths->[$ic], %gbcargs);
                    }

                    my $char;
                    if ($ic == $N-1) {
                        # rightmost
                        if (_exptable_cell_is_rowspan_tail($cell_bottom)) {
                            $char = $b_dataorheaderrowright;
                        } else {
                            $char = $b_betwrowright;
                        }
                    } else {
                        # between cells
                        if (_exptable_cell_is_colspan_tail($cell_right)) {
                            if (_exptable_cell_is_colspan_tail($cell_rightbottom)) {
                                if (_exptable_cell_is_rowspan_tail($cell_bottom)) {
                                    $char = "";
                                } else {
                                    $char = $b_betwrowline;
                                }
                            } else {
                                $char = $b_betwrowbetwcol_notop;
                            }
                        } else {
                            if (_exptable_cell_is_colspan_tail($cell_rightbottom)) {
                                $char = $b_betwrowbetwcol_nobot;
                            } else {
                                if (_exptable_cell_is_rowspan_tail($cell_bottom)) {
                                    if (_exptable_cell_is_rowspan_tail($cell_rightbottom)) {
                                        $char = $b_dataorheaderrowbetwcol;
                                    } else {
                                        $char = $b_betwrowbetwcol_noleft;
                                    }
                                } elsif (_exptable_cell_is_rowspan_tail($cell_rightbottom)) {
                                    $char = $b_betwrowbetwcol_noright;
                                } else {
                                    $char = $b_betwrowbetwcol;
                                }
                            }
                        }
                    }
                    $buf[$y][$ic*4+4] = $char;

                }
                $y++;
            } # DRAW_ROW_SEPARATOR

          DRAW_BOTTOM_BORDER:
            {
                last unless $ir == $M-1;
                my $row_is_header = $header_row > $exptable->[$ir][0][IDX_EXPTABLE_CELL_ORIG_ROWNUM];
                my %gbcargs = (for_header_row=>$row_is_header ? 1:0);
                my $b_botleft    = $bs_obj->get_border_char(char=>'ru_b');
                my $b_botline    = $bs_obj->get_border_char(char=>'h_b');
                my $b_botbetwcol = $bs_obj->get_border_char(char=>'hu_b');
                my $b_botright   = $bs_obj->get_border_char(char=>'lu_b');
                last unless length $b_botleft || length $b_botline || length $b_botbetwcol || length $b_botright;
                $buf[$y][0] = $b_botleft;
                for my $ic (0..$N-1) {
                    my $cell_right = $ic < $N-1 ? $exptable->[$ir][$ic+1] : undef;
                    $buf[$y][$ic*4+2] = $bs_obj->get_border_char(char=>'h_b', repeat=>$exptable_column_widths->[$ic]);
                    $buf[$y][$ic*4+4] = $ic == $N-1 ? $b_botright : (_exptable_cell_is_colspan_tail($cell_right) ? $b_botline : $b_botbetwcol);
                }
                $y++;
            } # DRAW_BOTTOM_BORDER

        }
    } # DRAW_EXPTABLE

    for my $row (@buf) { for (@$row) { $_ = "" if !defined($_) } } # debug. remove undef to "" to save dump width
    #use DDC; dd \@buf;
    join "", (map { my $linebuf = $_; join("", grep {defined} @$linebuf)."\n" } @buf);
}

# Back-compat: 'table' is an alias for 'generate_table', but isn't exported
{
    no warnings 'once';
    *table = \&generate_table;
}

1;
# ABSTRACT: Generate text table with simple interface and many options

=encoding utf8

=for Pod::Coverage ^(.+)$

=head1 SYNOPSIS

# EXAMPLE: share/examples/emmy.pl

will output something like:

# COMMAND: perl -Ilib share/examples/emmy.pl

If you set the C<border_style> argument to C<"UTF8::SingleLineBoldHeader">:

 print generate_table(
     rows => $rows,
     border_style => "UTF8::SingleLineBoldHeader",
     ...
 );

then the output will be something like:

# COMMAND: perl -Ilib share/examples/emmy.pl UTF8::SingleLineBoldHeader


=head1 DESCRIPTION

This module uses the simple interface of L<Text::Table::Tiny> (0.04) with
support for more formatting options like column/row spans, border style,
per-row/column/cell align/valign.

Keywords: rowspan, colspan.


=head1 FUNCTIONS

=head2 generate_table

Usage:

 my $table_str = generate_table(%args);

Arguments:

=over

=item * rows

Array of arrayrefs (of strings or hashrefs). Required. Each array element is a
row of cells. A cell can be a string like C<"foo"> specifying only the text
(equivalent to C<< {text=>"foo"} >>) or a hashref which allows you to specify a
cell's text (C<text>) as well as attributes like C<rowspan> (int, >= 1),
C<colspan> (int, >= 1), etc. See L</PER-CELL ATTRIBUTES> for the list of known
per-cell attributes.

Currently, C<top_border> and C<bottom_border> needs to be specified for the
first column of a row and will take effect for the whole row.

Alternatively, you can also specify cell attributes using L</cell_attrs>
argument.

=item * header_row

Int. Optional. Default 0. Number of rows that are header. Note that in
Text::Table::Tiny, this option is a boolean. We use integer to support multirow
header.

=item * border_style

Str. Optional. Uses default from the environment variable
L</PERL_TEXT_TABLE_MORE_BORDER_STYLE>, or environment variable L</BORDER_STYLE>,
or C<ASCII::SingleLineDoubleAfterHeader>. This is Perl module under the
L<BorderStyle> namespace, without the namespace prefix. To see how a border
style looks like, you can use the CLI L<show-border-style> from
L<App::BorderStyleUtils>.

=item * align

String. Value is either C<"left">, C<"middle">, C<"right">. Specify horizontal
text alignment of cells. Overriden by overridden by per-row, per-column, or
per-cell attribute of the same name.

=item * valign

String. Value is either C<"top">, C<"middle">, C<"bottom">. Specify vertical
text alignment of cells. Overriden by overridden by per-row, per-column, or
per-cell attribute of the same name.

=item * row_attrs

Array of records. Optional. Specify per-row attributes. Each record is a
2-element arrayref: C<< [$row_idx, \%attrs] >>. C<$row_idx> is zero-based. See
L</PER-ROW ATTRIBUTES> for the list of known attributes.

=item * col_attrs

Array of records. Optional. Specify per-column attributes. Each record is a
2-element arrayref: C<< [$col_idx, \%attrs] >>. C<$col_idx> is zero-based. See
L</PER-COLUMN ATTRIBUTES> for the list of known attributes.

=item * cell_attrs

Array of records. Optional. Specify per-cell attributes. Each record is a
3-element arrayref: C<< [$row_idx, $col_idx, \%attrs] >>. C<$row_idx> and
C<$col_idx> are zero-based. See L</PER-CELL ATTRIBUTES> for the list of known
attributes.

Alternatively, you can specify a cell's attribute in the L</rows> argument
directly, by specifying a cell as hashref.

=item * lpad

Integer. Optional. Set number of padding characters to add at the left side of
table cells. Overrides C<hpad>. Overridden by per-row/per-column/per-cell
padding attributes. See also C<rpad>.

=item * rpad

Integer. Optional. Set number of padding characters to add at the right side of
table cells. Overrides C<hpad>. Overridden by per-row/per-column/per-cell
padding attributes. See also C<rpad>.

=item * hpad

Integer. Optional. Set number of padding characters to add at the left and right
sides of table cells. Overridden by C<lpad> for left side, and C<rpad> for right
side. Overridden by per-row/per-column/per-cell padding attributes. See also
C<vpad>.

Default is 1.

=item * tpad

Integer. Optional. Set number of padding lines to add at the top side of table
cells. Overrides C<vpad>. Overridden by per-row/per-column/per-cell padding
attributes. See also C<bpad>.

=item * bpad

Integer. Optional. Set number of padding lines to add at the bottom side of
table cells. Overrides C<vpad>. Overridden by per-row/per-column/per-cell
padding attributes. See also C<tpad>.

=item * vpad

Integer. Optional. Set number of padding lines to add at the top and bottom
sides of table cells. Overridden by C<tpad> for top side, and C<bpad> for bottom
side. Overridden by per-row/per-column/per-cell padding attributes. See also
C<hpad>.

Default is 0.

=item * pad_char

String. Optional. Must be one character long. Default is C< > (space character).

=item * separate_rows

Boolean. Optional. Default 0. If set to true, will add a separator between data
rows. Equivalent to setting C<bottom_border> or C<top_border> attribute to true
for each row.

=item * wide_char

Boolean. Optional. Default false. Turn on wide character support. Cells that
contain wide Unicode characters will still be properly aligned. Note that this
requires optional prereq L<Text::WideChar::Util> or L<Text::ANSI::WideUtil>.

=item * color

Boolean. Optional. Default false. Turn on color support. Cells that contain ANSI
color codes will still be properly aligned. Note that this requires optional
prereq L<Text::ANSI::Util> or L<Text::ANSI::WideUtil>.

=back


=head1 PER-ROW ATTRIBUTES

=over

=item * align

String. Value is either C<"left">, C<"middle">, C<"right">. Specify text
alignment of cells. Override table argument, but is overridden by per-column or
per-cell attribute of the same name.

=item * valign

String. Value is either C<"top">, C<"middle">, C<"bottom">. Specify vertical
text alignment of cells. Override table argument, but is overridden by
per-column or per-cell attribute of the same name.

=item * bottom_border

Boolean.

=item * top_border

Boolean.

=item * lpad

Integer.

=item * rpad

Integer.

=item * hpad

Integer.

=item * tpad

Integer.

=item * bpad

Integer.

=item * vpad

Integer.


=head1 PER-COLUMN ATTRIBUTES

=item * align

String. Value is either C<"left">, C<"middle">, C<"right">. Specify text
alignment of cells. Override table argument and per-row attribute of the same
name, but is overridden by per-cell attribute of the same name.

=item * valign

String. Value is either C<"top">, C<"middle">, C<"bottom">. Specify vertical
text alignment of cells. Override table argument and per-row attribute of the
same name, but is overridden by per-cell attribute of the same name.

=item * lpad

Integer.

=item * rpad

Integer.

=item * hpad

Integer.

=item * tpad

Integer.

=item * bpad

Integer.

=item * vpad

Integer.


=head1 PER-CELL ATTRIBUTES

=item * align

String. Value is either C<"left">, C<"middle">, C<"right">. Override table
argument, per-row attribute, and per-column attribute of the same name.

=item * valign

String. Value is either C<"top">, C<"middle">, C<"bottom">. Specify vertical
text alignment of cells. Override table argument, per-row attribute, and
per-column attribute of the same name.

=item * colspan

Positive integer. Default 1.

=item * rowspan

Positive integer. Default 1.

=item * bottom_border.

Boolean. Currently the attribute of he leftmost cell is used.

=item * top_border.

Boolean. Currently the attribute of he leftmost cell is used.

=item * lpad

Integer.

=item * rpad

Integer.

=item * hpad

Integer.

=item * tpad

Integer.

=item * bpad

Integer.

=item * vpad

Integer.

=item * pad_char

String.


=head1 ENVIRONMENT

=head2 PERL_TEXT_TABLE_MORE_BORDER_STYLE

String. Used to set the default for the L</border_style> option. Has higher
precedence than L</BORDER_STYLE>.

=head2 BORDER_STYLE

String. Used to set the default for the L</border_style> option. Has lower
precedence than L</PERL_TEXT_TABLE_MORE_BORDER_STYLE>.


=head1 FAQ

=head2 Can I have multiple header rows?

Yes, by setting L</header_row> option to 2 or whatever number of header rows you
have. See example script F<multirow-header.pl> in this distribution.


=head1 SEE ALSO

L<Text::ANSITable> also offers lots of formatting options, but currently lacks
support for rowspan/colspan. It also uses an OO interface and has features I
never use: hiding rows and selecting display columns different from declared
columns. I currently plan to actively develop Text::Table::More instead of
Text::ANSITable, but we'll see.

L<Acme::CPANModules::TextTable> contains a comparison and benchmark for modules
that generate text table.

HTML E<lt>TABLEE<gt> element,
L<https://www.w3.org/TR/2014/REC-html5-20141028/tabular-data.html>,
L<https://www.w3.org/html/wiki/Elements/table>

=cut
