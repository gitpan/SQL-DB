package App::sqldb_schema;
use strict;
use warnings;
use Term::Prompt;
use File::Slurp qw/write_file/;
use Data::Dumper;
use Perl::Tidy qw/perltidy/;
use DBI;

our $VERSION = '0.19_9';

sub opt_spec {
    (
        [ "username|u=s", "DSN user name" ],
        [ "password|p=s", "DSN password" ],
        [ "dbschema|d=s", "Database Schema name" ],
    );
}

sub arg_spec {
    (
        [ "dsn=s",     "DSN",             { required => 1 }, ],
        [ "package=s", "Package name",    { required => 1 }, ],
        [ "outfile=s", "output filename", { default  => '-' }, ],
    );
}

sub run {
    my ( $class, $opt ) = @_;

    $opt->{dsn} = 'dbi:SQLite:dbname=' . $opt->{dsn} if -f $opt->{dsn};

    my ( $scheme, $driver, $attr_string, $attr_hash, $driver_dsn ) =
      DBI->parse_dsn( $opt->dsn )
      or die "Could not parse DSN: " . $opt->dsn;

    if ( $driver ne 'SQLite' and not $opt->{username} ) {
        $opt->{username} = prompt( 'x', 'Username:', '', '' );
    }

    if ( $driver ne 'SQLite' and not $opt->{password} ) {
        $opt->{password} = prompt( 'p', 'Password:', '', '' );
        print "\n";
    }

    if ( $driver eq 'SQLite' and !$opt->{dbschema} ) {
        $opt->{dbschema} = 'main';
    }

    if ( $driver eq 'Pg' and !$opt->{dbschema} ) {
        $opt->{dbschema} = 'public';
    }

    my $output = '';

    $output .= "# Generated by " . __PACKAGE__ . ' ';
    $output .= ( scalar localtime ) . "\n";
    $output .= "# from " . $opt->dsn . "\n";
    $output .= "package $opt->{package};\n";

    $output .= "use strict;\n";
    $output .= "use warnings;\n";
    $output .= "require SQL::DB::Schema;\n\n";

    my $dbh = DBI->connect( $opt->dsn, $opt->username, $opt->password )
      || die "Could not connect: " . DBI->errstr;

    my $sth =
      $dbh->table_info( '%', $opt->{dbschema}, '%',
        "'TABLE','VIEW','GLOBAL TEMPORARY','LOCAL TEMPORARY'" );

    local $Data::Dumper::Indent   = 0;
    local $Data::Dumper::Maxdepth = 0;

    my @columns;

    while ( my $table = $sth->fetchrow_arrayref ) {
        my $sth2 = $dbh->column_info( '%', '%', $table->[2], '%' );
        push( @columns, @{ $sth2->fetchall_arrayref } );
    }

    $output .= 'my ' . Dumper \@columns;

    $output .=
      qq[\nSQL::DB::Schema->new(name => '$opt->{package}')->define(\$VAR1);\n];

    $output .= "1;\n";

    my $tidy;

    perltidy(
        source      => \$output,
        destination => \$tidy,
        perltidyrc  => '/dev/null',
    );

    if ( $opt->outfile eq '-' ) {
        print $tidy;
    }
    else {
        write_file( $opt->outfile, $tidy );
    }
}

1;

# vim: set tabstop=4 expandtab:
