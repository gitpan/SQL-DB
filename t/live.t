#!/usr/bin/perl
use strict;
use warnings;
use lib 't/lib';
use Test::More;
use Test::Database;
use SQL::DB;
use Benchmark qw(:all);

my $subs;

my @handles = Test::Database->handles(qw/ SQLite Pg mysql /);

sub run_tests {
    my $handle = shift;
    my ( $dsn, $user, $pass ) = $handle->connection_info;

    my $db = SQL::DB->new(
        dsn    => $dsn,
        dbuser => $user,
        dbpass => $pass,
    );

    isa_ok( $db, 'SQL::DB' );
}

if ( @handles > 1 ) {
    foreach my $h (@handles) {
        $subs->{ $h->dbd } = sub {
            diag( 'DSN: ' . $h->dsn );
            run_tests($h);
        };
    }

    my $results = timethese( 1, $subs, 'none' );

    #    diag cmpthese( $results );
}
else {
    run_tests( $handles[0] );
}

done_testing();

1;
