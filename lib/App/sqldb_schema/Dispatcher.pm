# Do not modify!
# This file is autogenerated and your changes will be overwritten.
package App::sqldb_schema::Dispatcher;
use Getopt::Long::Descriptive qw/describe_options prog_name/;
use strict;
use warnings;

our $VERSION = '0.11';

my $me = prog_name;

my $program = {
    'App::sqldb_schema' => {
        'opt_spec' => [
            [ 'username|u=s', 'DSN user name' ],
            [ 'password|p=s', 'DSN password' ],
            [ 'dbschema|d=s', 'Database Schema name' ]
        ],
        'arg_spec' => [
            [ 'dsn=s',     'DSN',             { 'required' => 1 } ],
            [ 'package=s', 'Package name',    { 'required' => 1 } ],
            [ 'outfile=s', 'output filename', { 'default'  => '-' } ]
        ],
        'name'          => 'sqldb_schema',
        'usage_desc'    => 'usage: %c [options] DSN PACKAGE [OUTFILE]',
        'order'         => 2147483647,
        'class'         => 'App::sqldb_schema',
        'abstract'      => 'generate a SQL::DB schema from a database',
        'require_order' => 0,
        'getopt_conf'   => [ 'permute' ]
    }
};

sub _commands {
    my $cmd = shift;
    require List::Util;

    my @commands =
      grep { $_->{class} =~ m/${cmd}::/ and not $_->{class} =~ m/${cmd}::.*:/ }
      values %$program;

    return unless @commands;

    my $max = 4 + List::Util::max( map { length $_->{name} } @commands );

    return
      map { sprintf( "    %-${max}s %s\n", $_->{name}, $_->{abstract} ) }
      sort { $a->{order} <=> $b->{order} } @commands;
}

sub _message {
    my ( $cmd, $usage, $abstract ) = @_;

    my $str = $usage->text;
    $str .= "\n($program->{$cmd}->{abstract})\n" if $abstract;

    my @arg_spec = @{ $program->{$cmd}->{arg_spec} };
    return unless @arg_spec;

    my @commands = _commands($cmd);

    if (@commands) {
        my $x = $arg_spec[0]->[0];
        $x =~ s/[\|=].*//;

        $str .= "\nValid values for " . ( uc $x ) . " include:\n";
        $str .= join( '', @commands );
    }
    return $str;
}

sub _usage {
    die _message(@_);
}

sub _help {
    print STDOUT _message( @_, 1 );
}

my $DEBUG = 0;
my %RAN   = ();

sub _dispatch {
    my $class = shift;
    if (@_) {
        @ARGV = @_;
    }

    my $cmd       = 'App::sqldb_schema';
    my @ORIG_ARGV = @ARGV;

    # Look for a subcommand
    while ( @ARGV && exists $program->{ $cmd . '::' . $ARGV[0] } ) {
        $cmd = $cmd . '::' . shift @ARGV;
    }

    my ( $opt, $usage ) = describe_options(
        $program->{$cmd}->{usage_desc},
        @{ $program->{$cmd}->{opt_spec} },
        { getopt_conf => $program->{$cmd}->{getopt_conf} },
    );

    if ( $opt->can('help') && $opt->help ) {
        return _help( $cmd, $usage );
    }

    my @arg_spec = @{ $program->{$cmd}->{arg_spec} };

    # Missing a required argument?
    my @narg_spec = @arg_spec;
    while ( scalar @narg_spec > scalar @ARGV ) {

        my $arg = pop @narg_spec;
        next unless ( exists $arg->[2]->{required} );

        _usage( $cmd, $usage );
        return;
    }

    # Now rebuild the whole command line that includes options and
    # arguments together.
    my @newargv;
    my @remainder;

    my $i = 0;
    while (@ARGV) {
        my $val = shift @ARGV;
        if ( !@arg_spec ) {
            @remainder = ( $val, @ARGV );
            last;
        }
        my $arg = shift @arg_spec;
        my $x   = $arg->[0];
        $x =~ s/[|=].*//;
        push( @newargv, '--' . $x, $val );
    }

    @ARGV = @newargv;

    my ( $new_opt, $new_usage ) = describe_options(
        $program->{$cmd}->{usage_desc},
        @{ $program->{$cmd}->{arg_spec} },
        map { [ $_->[0], $_->[1] ] }    # ignore 'require', 'default'
          @{ $program->{$cmd}->{opt_spec} },
    );

    while ( my ( $key, $val ) = each %$opt ) {
        $new_opt->{$key} = $val;
    }
    $opt = $new_opt;

    @ARGV = @remainder;

    if ( !$RAN{$cmd} ) {

        eval "require $cmd";    ## no critic
        die $@ if $@;

        ( my $plugin_file = $cmd . '.pm' ) =~ s!::!/!g;
        my $file = $INC{$plugin_file};

        if ( -M $file < -M __FILE__ ) {
            warn "warning: "
              . __PACKAGE__
              . " is out of date:\n    "
              . scalar localtime( ( stat(__FILE__) )[9] ) . " "
              . __FILE__
              . "\n    "
              . scalar localtime( ( stat($file) )[9] )
              . " $file\n";
        }

        # FIXME move this check into App::Dispatcher?
        if ( !$cmd->can('run') ) {
            _usage( $cmd, $usage );
            die "$cmd missing run() method or 'required' attribute on arg 1\n";
        }

        {
            no strict 'refs';    ## no critic
            *{ $cmd . '::opt' } = sub { $opt };
            *{ $cmd . '::usage' } = sub { _message( $cmd, $usage ) };
            *{ $cmd . '::dispatch' } = sub {
                shift;
                $DEBUG = 1 if $opt->can('debug_dispatcher');
                $class->_dispatch(@_);
            };
        }
        $RAN{$cmd}++;
    }

    return $cmd->run($opt);

}

sub run {
    my $class = shift;
    $class->_dispatch(@_);
}

1;
__END__


=head1 NAME

App::sqldb_schema::Dispatcher - Dispatcher for App::sqldb_schema
commands

=head1 SYNOPSIS

  use App::sqldb_schema::Dispatcher;
  App::sqldb_schema::Dispatcher->run;

=head1 DESCRIPTION

B<App::sqldb_schema::Dispatcher> provides option checking, argument
checking, and command dispatching for commands implemented under the
App::sqldb_schema::* namespace.

This class has a single method:

=over 4

=item run

Dispatch to a L<App::sqldb_schema> command based on the contents of
@ARGV.

=back

This module was automatically generated by L<App::Dispatcher>(3p).

=head1 SEE ALSO

L<App::Dispatcher>(3p), L<app-dispatcher>(1)

=head1 AUTHOR

Mark Lawrence E<lt>nomad@null.netE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2011 Mark Lawrence E<lt>nomad@null.netE<gt>

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

=cut

# vim: set tabstop=4 expandtab:
