package Test::Script::Run;

use warnings;
use strict;
use Test::More;
use Test::Exception;
use IPC::Run3;
use File::Basename;
use File::Spec;

our $VERSION = '0.02';
use base 'Exporter';
our @EXPORT =
  qw/run_ok run_not_ok run_script run_output_matches run_output_matches_unordered/;
our @EXPORT_OK = qw/is_script_output last_script_stdout last_script_stderr
  last_script_exit_code/;
our %EXPORT_TAGS = ( all => [ @EXPORT, @EXPORT_OK ] );
my (
    $last_script_stdout,        $last_script_stderr,
    $last_script_exit_code,
);

=head2 run_script($script, $args, $stdout, $stderr)

Runs the script $script as a perl script, setting the @INC to the same as
our caller.

$script is the name of the script to be run (such as 'prophet'). $args is a
reference to an array of arguments to pass to the script. $stdout and $stderr
are both optional; if passed in, they will be passed to L<IPC::Run3>'s run3
subroutine as its $stdout and $stderr args.  Otherwise, this subroutine will
create scalar references to pass to run3 instead (which are treated as strings
for STDOUT/STDERR to be written to).

Returns run3's return value and, if no $stdout and $stderr were passed in, the
STDOUT and STDERR of the script that was run.

=cut

sub run_script {
    my $script = shift;
    my $args = shift || [];
    my ( $stdout, $stderr ) = @_;
    my ( $new_stdout, $new_stderr, $return_stdouterr );
    if ( !ref($stdout) && !ref($stderr) ) {
        ( $stdout, $stderr, $return_stdouterr ) =
          ( \$new_stdout, \$new_stderr, 1 );
    }
    my @cmd = _get_perl_cmd($script);

    my $ret = run3 [ @cmd, @$args ], undef, $stdout, $stderr;
    $last_script_exit_code = $? >> 8;
    if ( ref $stdout eq 'SCALAR' ) {
        $last_script_stdout = $$stdout;
    }

    if ( ref $stderr eq 'SCALAR' ) {
        $last_script_stderr = $$stderr;
    }

    return $return_stdouterr
      ? ( $ret, $last_script_stdout, $last_script_stderr )
      : $ret;
}

=head2 run_ok($script, $args, $msg)

Runs the script, checking that it didn't error out.

$script is the name of the script to be run (e.g. 'prophet'). $args
is an optional reference to an array of arguments to pass to the
script when it is run. $msg is an optional message to print with
the test. If $args is not specified, you can still pass in
a $msg.

Returns nothing of interest.

=cut

sub run_ok {
    return _run_ok( '==', @_ );
}

=head2 run_not_ok($script, $args, $msg)

opposite of run_ok

=cut

sub run_not_ok {
    return _run_ok( '!=', @_ );
}

sub _run_ok {
    my $cmp = shift || '==';    # the exit code
    my $script = shift;
    my $args;
    $args = shift if ( ref $_[0] eq 'ARRAY' );
    my $msg = (@_) ? shift : '';

    local $Test::Builder::Level = $Test::Builder::Level + 1;

    lives_and {
        local $Test::Builder::Level = $Test::Builder::Level + 1;
        my ( $ret, $stdout, $stderr ) = run_script( $script, $args );
        cmp_ok( $? >> 8, $cmp, 0, $msg );
    };
}

# _updir( $path )
#
# Strips off the filename in the given path and returns the absolute
# path of the remaining directory.

sub _updir {
    my $path = shift;
    my ( $file, $dir, undef ) = fileparse( File::Spec->rel2abs($path) );
    return $dir;
}

our $RUNCNT;

# _get_perl_cmd( $script )
# find the $script path

sub _get_perl_cmd {
    my $script = shift;
    my $base_dir;
    unless ( File::Spec->file_name_is_absolute($script) ) {
        my ( $tmp, $i ) = ( _updir($0), 0 );
        while ( !-d File::Spec->catdir( $tmp, 'bin' ) && $i++ < 10 ) {
            $tmp = _updir($tmp);
        }

        $base_dir = File::Spec->catdir( $tmp, 'bin' );
        die "couldn't find bin dir" unless -d $base_dir;
    }

    my @cmd = ( $^X, ( map { "-I$_" } @INC ) );
    push @cmd, '-MDevel::Cover' if $INC{'Devel/Cover.pm'};
    if ( $INC{'Devel/DProf.pm'} ) {
        push @cmd, '-d:DProf';
        $ENV{'PERL_DPROF_OUT_FILE_NAME'} = 'tmon.out.' . $$ . '.' . $RUNCNT++;
    }
    push @cmd, $base_dir ? File::Spec->catdir( $base_dir => $script ) : $script;
    return @cmd;
}

=head2 is_script_output($scriptname \@args, \@stdout_match, \@stderr_match, $msg)

Runs $scriptname, checking to see that its output matches.

$args is an array reference of args to pass to the script. $stdout_match and
$stderr_match are references to arrays of expected lines. $msg is a string
message to display with the test. $stderr_match and $msg are optional. (As is
$stdout_match if for some reason you expect your script to have no output at
all. But that would be silly, wouldn't it?)

Allows regex matches as well as string equality (lines in $stdout_match and
$stderr_match may be Regexp objects).

=cut

sub is_script_output {
    my ( $script, $args, $exp_stdout, $exp_stderr, $msg ) = @_;
    local $Test::Builder::Level = $Test::Builder::Level + 1;

    my $stdout_err = [];
    $exp_stderr ||= [];

    my $ret = run_script(
        $script, $args,
        _mk_cmp_closure( 'stdout', $exp_stdout, $stdout_err ),    # stdout
        _mk_cmp_closure( 'stderr', $exp_stderr, $stdout_err ),    # stderr
    );

    _check_cmp_closure_output( $script, $msg, $args, $exp_stdout, $stdout_err );
}

# =head2 _mk_cmp_closure($expected, $error)
# $expected is a reference to an array of expected output lines, and
# $error is an array reference for storing error messages.
# 
# Returns a subroutine that takes a line of output and compares it
# to the next line in $expected. You can, for example, pass this
# subroutine to L<IPC::Run3>::run3 and it will compare the output
# of the script being run to the expected output. After the script
# is done running, errors will be in $error.
# 
# If a line in $expected is a Regexp reference (made with e.g.
# qr/foo/), the subroutine will check for a regexp match rather
# than string equality.

sub _mk_cmp_closure {
    my ( $type, $exp, $err ) = @_;

    if ( $type eq 'stderr' ) {
        $last_script_stderr = '';
        my $line = 0;
        return sub {
            my $output = shift;
            ++$line;
            $last_script_stderr .= $output;
            __mk_cmp_closure()->( $exp, $err, $line, $output );
          }
    }
    else {
        $last_script_stdout = '';
        my $line = 0;
        return sub {
            my $output = shift;
            ++$line;
            $last_script_stdout .= $output;
            __mk_cmp_closure()->( $exp, $err, $line, $output );
          }
    }
}

sub __mk_cmp_closure {
    sub {
        my ( $exp, $err, $line, $output ) = @_;
        chomp $output;
        unless (@$exp) {
            push @$err, "$line: got $output";
            return;
        }
        my $item = shift @$exp;
        push @$err, "$line: got ($output), expect ($item)\n"
          unless ref($item) eq 'Regexp'
            ? ( $output =~ m/$item/ )
            : ( $output eq $item );
      }
}

# XXX note that this sub doesn't check to make sure we got
# all the errors we were expecting (there can be more lines
# in the expected stderr than the received stderr as long
# as they match up until the end of the received stderr --
# the same isn't true of stdout)
sub _check_cmp_closure_output {
    my ( $script, $msg, $args, $exp_stdout, $stdout_err ) = @_;

    for my $line (@$exp_stdout) {
        next if !defined $line;
        push @$stdout_err, "got nothing, expected: $line";
    }

    my $test_name = join( ' ', $msg ? "$msg:" : '', $script, @$args );
    is( scalar(@$stdout_err), 0, $test_name );

    if (@$stdout_err) {
        diag( "Different in line: " . join( "\n", @$stdout_err ) );
    }
}

=head2 run_output_matches($script, $args, $exp_stdout, $exp_stderr, $msg)

A wrapper around L<is_script_output> that also checks to make sure
the test runs without throwing an exception.

=cut

sub run_output_matches {
    my ( $script, $args, $expected, $stderr, $msg ) = @_;
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    lives_and {
        local $Test::Builder::Level = $Test::Builder::Level + 3;
        is_script_output( $script, $args, $expected, $stderr, $msg );
    };
}

=head2 run_output_matches_unordered($script, $args, $exp_stdout, $exp_stderr, $msg)

This subroutine has exactly the same functionality as run_output_matches, but
doesn't impose a line ordering when comparing the expected and received
outputs.

=cut

sub run_output_matches_unordered {
    my ( $cmd, $args, $stdout, $stderr, $msg ) = @_;
    $stderr ||= [];

    my ( $val, $out, $err ) = run_script( $cmd, $args );

    local $Test::Builder::Level = $Test::Builder::Level + 1;

    # Check if each line matches a line in the expected output and
    # delete that line if we have a match. If no match is found,
    # add an error.
    my $errors = [];
    my @lines = split /\n/, $out;
  OUTPUT: while ( my $line = shift @lines ) {
        for my $exp_line (@$stdout) {
            if (
                (
                    ref($exp_line) eq 'Regexp'
                    ? ( $line =~ m/$exp_line/ )
                    : ( $line eq $exp_line )
                )
              )
            {

                # remove the found element from the array of expected output
                $stdout = [ grep { $_ ne $exp_line } @$stdout ];
                next OUTPUT;
            }
        }

        # we didn't find a match
        push @$errors, "couldn't find match for ($line)\n";
    }

    # do the same for STDERR
    @lines = split /\n/, $err;
  ERROR: while ( my $line = shift @lines ) {
        for my $exp_line (@$stderr) {
            if (
                (
                    ref($exp_line) eq 'Regexp'
                    ? ( $line =~ m/$exp_line/ )
                    : ( $line eq $exp_line )
                )
              )
            {

                # remove the found element from the array of expected output
                $stderr = [ grep { $_ ne $exp_line } @$stderr ];
                next ERROR;
            }
        }

        # we didn't find a match
        push @$errors, "couldn't find match for ($line)\n";
    }

    # add any expected lines that we didn't find to the errors
    for my $exp_line ( @$stdout, @$stderr ) {
        push @$errors, "got nothing, expected: $exp_line";
    }

    my $test_name = join( ' ', $msg ? "$msg:" : '', $cmd, @$args );
    is( scalar(@$errors), 0, $test_name );

    if (@$errors) {
        diag( "Errors: " . join( "\n", @$errors ) );
    }
}

=head2 last_script_stdout

return last script's stdout

=cut

sub last_script_stdout        { $last_script_stdout }

=head2 last_script_stderr

return last script's stderr

=cut

sub last_script_stderr        { $last_script_stderr }

=head2 last_script_exit_code

return last script's exit code

=cut

sub last_script_exit_code     { $last_script_exit_code }


1;

__END__

=head1 NAME

Test::Script::Run - test the script with run

=head1 SYNOPSIS

    use Test::Script::Run;
    run_ok( 'app_name', [ app's args ], 'you_app runs ok' );
    my ( $return, $stdout, $stderr ) = run_script( 'app_name', [ app's args ] );
    run_output_matches(
        'app_name', [app's args],
        [ 'out line 1', 'out line 2' ],
        [ 'err line 1', 'err line 2' ],
        'run_output_matches'
    );
    run_output_matches_unordered(
        'app_name', [ app's args ],
        [ 'out line 2', 'out line 1' ],
        [ 'err line 2', 'err line 1' ],
        'run_output_matches_unordered'
    );

=head1 DESCRIPTION

This module exports some subs to help test and run scripts in your dist's 
bin/ directory, if the script path is not absolute.

Nearly all the essential code is stolen from Prophet::Test, we think subs like
those should live below C<Test::> namespace, that's why we packed them and
created this module.

=head1 DEPENDENCIES

L<Test::More>, L<Test::Exception>, L<IPC::Run3>, L<File::Basename>, L<File::Spec>

=head1 BUGS AND LIMITATIONS

No bugs have been reported.

=head1 AUTHOR

sunnavy  C<< <sunnavy@bestpractical.com> >>

=head1 LICENCE AND COPYRIGHT

Copyright 2009 Best Practical Solutions.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

