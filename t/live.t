#!/usr/bin/perl
use strict;
use warnings;
use lib 't/lib';
use Test::More;
use Test::Database;
use File::Temp qw/tempdir/;
use Cwd;
use SQL::DB;

my $cwd;
BEGIN { $cwd = getcwd }

my $subs;

my @handles = Test::Database->handles(qw/ SQLite Pg mysql /);

if (@handles) {
    plan tests => 1 * @handles;
}
else {
    plan skip_all => "No database handles to test with";
}

my $tempdir;
foreach my $handle (@handles) {
    chdir $cwd || die "chdir: $!";
    $tempdir = tempdir( CLEANUP => 1 );
    chdir $tempdir || die "chdir: $!";

    my ( $dsn, $user, $pass ) = $handle->connection_info;

    my $db = SQL::DB->new(
        dsn    => $dsn,
        dbuser => $user,
        dbpass => $pass,
    );

    isa_ok( $db, 'SQL::DB' );
}

done_testing();

# So that File::Temp doesn't complain if it can't remove $tempdir when
# $tempdir goes out of scope;
END {
    chdir $cwd;
}

1;
