#!/usr/bin/perl
use lib 't/lib';
use strict;
use warnings;
use DBD::SQLite;
use Test::More;
use Test::Exception;
use Test::Memory::Cycle;
use SQLDBTest;
use SQL::DB qw(define_tables count);

BEGIN {
    # If testing with Postgres or a good (as of yet unreleased) version
    # of DBD::SQLite then run the tests.
    if ($ENV{TEST_PG} or $DBD::SQLite::VERSION >= 1.15) {
        plan tests => 14;
    }
    # Check for using Debian's version which is patched properly
    elsif (-e '/usr/share/doc/libdbd-sqlite3-perl' and
        $INC{'DBD/SQLite.pm'} eq '/usr/lib/perl5/DBD/SQLite.pm') {
        plan tests => 14;
    }
    else {
        plan skip_all => 'due to DBD::SQLite Bug #30558';
    }
}

require_ok('t/TestLib.pm');

define_tables(TestLib->Artist);

my $db = SQLDBTest->new();
$db->test_connect;
$db->deploy;
#$db->dbh->do('PRAGMA parser_trace = ON;');
#$db->dbh->do('PRAGMA vdbe_trace = ON;');
#$db->dbh->do('PRAGMA vdbe_listing = ON;');
#$db->dbh->trace('5|ALL|SQL');

ok($db->create_seq('test'), "Sequence test created");


my $a1 = Artist->new(id => 1, name => 'artist1', ucname => 'ARTIST1');
my $a2 = Artist->new(id => 2, name => 'artist2', ucname => 'ARTIST2');
my $a3 = Artist->new(id => 3, name => 'artist3', ucname => 'ARTIST3');
my $a4 = Artist->new(id => 4, name => 'artist4', ucname => 'ARTIST4');
my $artists = $db->arow('artists');

my $res = $db->txn(sub {
    $db->insert($a1);
});
ok($res, 'transaction insert: '.$res);

is($db->fetch1(
    select => count($artists->id)->as('acount'),
    from   => $artists,
)->acount,1, 'select 1');

$res = $db->txn(sub {
    $db->insert($a1);
});
ok(!$res, "transaction insert duplicate:");

is($db->fetch1(
    select => count($artists->id)->as('acount'),
    from   => $artists,
)->acount,1, 'still select 1');

ok(!$db->txn(sub {
    $db->insert($a2);
    $db->insert($a1);
}), 'transaction insert duplicate with non-dup');

is($db->fetch1(
    select => count($artists->id)->as('acount'),
    from   => $artists,
)->acount,1, 'still select 1');

$res = $db->txn(sub {
    $db->insert($a2);
    $db->insert($a3);
});

ok($res, 'transaction insert 2 and 3 '. ($res ? '' : $res));

is($db->fetch1(
    select => count($artists->id)->as('acount'),
    from   => $artists,
)->acount,3, 'select 3');

$res = $db->txn(sub {
    $db->txn(sub {
        $db->insert($a4);
    });
});

ok($res, 'nested transaction insert 4'. ($res ? '' : $res));

memory_cycle_ok($db, 'memory cycle');

$res = $db->txn(sub {
    $db->insert($a4);
});
ok(!$res, "transaction insert duplicate 4");

my $subref = sub {
    $db->txn(sub{
        $db->insert($a4);
    });
};

#$SQL::DB::DEBUG=1;
$res = $db->txn($subref);

ok(!$res, 'nested transaction insert 4 again');