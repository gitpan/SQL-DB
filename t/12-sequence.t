use strict;
use warnings;
use Test::More;
use Test::Database;
use Cwd;
use File::Temp qw/tempdir/;
use SQL::DB;
use SQL::DBx::Sequence;
use SQL::DBx::Deploy;    # Remove this stuff

can_ok(
    'SQL::DB', qw/
      create_sequence
      nextval
      /
);

my $cwd;
BEGIN { $cwd = getcwd }

my $subs;

my @handles = Test::Database->handles(qw/ SQLite Pg mysql /);

if ( !@handles ) {
    plan skip_all => "No database handles to test with";
}

my $tempdir;
foreach my $handle (@handles) {
    chdir $cwd || die "chdir: $!";
    $tempdir = tempdir( CLEANUP => 1 );
    chdir $tempdir || die "chdir: $!";

    if ( $handle->dbd eq 'SQLite' ) {
        $handle->driver->drop_database( $handle->name );
        $handle->driver->drop_database( $handle->name . '.seq' );
    }

    my ( $dsn, $user, $pass ) = $handle->connection_info;

    my $db = SQL::DB->new(
        dsn      => $dsn,
        username => $user,
        password => $pass,
    );

    eval { $db->conn->dbh->do('DROP SEQUENCE seq_testseq'); };

    $db->create_sequence('testseq');

    my ( $id1, $id2, $id3, $id4 );

    $id1 = $db->nextval('testseq');
    ok $id1, 'nextval:' . $id1;

    $id2 = $db->nextval('testseq');
    ok $id2 > $id1, "$id2 > $id1";

    eval {
        $db->txn(
            sub {
                $id3 = $db->nextval('testseq');
                die;    # Force a ROLLBACK
            }
        );
    };

  TODO: {
        local $TODO = "SQLite Sequence Rollback"
          if ( $handle->dbd eq 'SQLite' );

        $id4 = $db->nextval('testseq');
        ok $id4 > $id3, "$id4 > $id3 after ROLLBACK";
    }

    if ( $handle->dbd eq 'SQLite' ) {
        my $rows =
          $db->conn->dbh->selectrow_array('SELECT count(seq) from seq.testseq');
        is $rows, 1, 'SQLite rows deleted';
    }
}

done_testing();

# So that File::Temp doesn't complain if it can't remove $tempdir when
# $tempdir goes out of scope;
END {
    chdir $cwd;
}

1;
