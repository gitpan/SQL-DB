package SQL::DB;
use strict;
use warnings;
use Moo;
use Carp qw/croak carp confess/;
use DBIx::Connector;
use SQL::DB::Expr qw/:all/;
use SQL::DB::Cursor;
use Sub::Install qw/install_sub/;

use constant SQL_FUNCTIONS => qw/
  table
  srow
  urow
  query
  AND
  OR
  sql_and
  sql_case
  sql_cast
  sql_coalesce
  sql_concat
  sql_count
  sql_exists
  sql_func
  sql_length
  sql_lower
  sql_max
  sql_min
  sql_or
  sql_sum
  sql_upper
  sql_values
  /;

use Sub::Exporter -setup => {
    exports => [SQL_FUNCTIONS],
    groups  => {

        #        default => [qw/ /],
        all     => [SQL_FUNCTIONS],
        default => [SQL_FUNCTIONS],
    },
};

our $VERSION = '0.19_7';

has 'debug'   => ( is => 'rw' );
has 'dry_run' => ( is => 'rw' );

has 'dsn' => ( is => 'rw', required => 1 );
has 'dbd' => ( is => 'rw', init_arg => undef );
has 'dbuser'  => ( is => 'rw' );
has 'dbpass'  => ( is => 'rw' );
has 'dbattrs' => ( is => 'rw', default => sub { {} } );
has 'conn' => ( is => 'rw', init_arg => undef );

has 'prepare_mode' => (
    is  => 'rw',
    isa => sub {
        die "prepare_mode must be 'prepare|prepare_cached'"
          unless $_[0] =~ m/^(prepare)|(prepare_cached)$/;
    },
    default => sub { 'prepare_cached' },
);

has '_current_timestamp' => ( is => 'rw', init_arg => undef );
has '_sqlite_seq_dbh'    => ( is => 'rw', init_arg => undef );

sub BUILD {
    my $self = shift;

    ( my $dsn = $self->dsn ) =~ /^dbi:(.*?):/;
    my $dbd = $1 || die "Invalid DSN: " . $self->dsn;
    $self->dbd($dbd);

    $self->dbattrs(
        {
            PrintError => 0,
            ChopBlanks => 1,
            $dbd eq 'Pg'     ? ( pg_enable_utf8    => 1 ) : (),
            $dbd eq 'SQLite' ? ( sqlite_unicode    => 1 ) : (),
            $dbd eq 'mysql'  ? ( mysql_enable_utf8 => 1 ) : (),
            %{ $self->dbattrs },
            RaiseError => 1,
            AutoCommit => 1,
            Callbacks  => {
                connected => sub {
                    my $h = shift;
                    if ( $dbd eq 'Pg' ) {
                        $h->do('SET client_min_messages = WARNING;');
                        $h->do("SET TIMEZONE TO 'UTC';");
                    }
                    elsif ( $dbd eq 'SQLite' ) {
                        $h->do('PRAGMA foreign_keys = ON;');
                    }
                    return;
                },
            }
        }
    );

    $self->conn(
        DBIx::Connector->new(
            $dsn, $self->dbuser, $self->dbpass, $self->dbattrs
        )
    );

    # Emulate sequence support for SQLite
    if ( $dbd eq 'SQLite' and $self->dsn !~ m/\.seq$/ ) {
        my $dsn = $self->dsn . '.seq';
        my $dbh = DBI->connect(
            $dsn, '', '',
            {
                RaiseError => 1,
                PrintError => 0,
            }
        );
        $self->_sqlite_seq_dbh($dbh);
    }

    $self->conn->mode('fixup');
    return $self;
}

sub connect {
    my $class   = shift;
    my $dsn     = shift;
    my $dbuser  = shift;
    my $dbpass  = shift;
    my $dbattrs = shift || {};
    return $class->new(
        dsn     => $dsn,
        dbuser  => $dbuser,
        dbpass  => $dbpass,
        dbattrs => $dbattrs,
    );
}

sub sth {
    my $self    = shift;
    my $prepare = $self->prepare_mode;
    my $query   = eval { query(@_) };

    if ( !defined $query ) {
        confess "Bad Query: $@";
    }

    warn $self->query_as_string( "$query", @{ $query->_bvalues } ) . "\n"
      if $self->debug;

    return if $self->dry_run;

    my $wantarray = wantarray;

    return $self->conn->run(
        sub {
            my $dbh = $_;
            my $sth = eval { $dbh->$prepare("$query") };
            if ($@) {
                die $@ if ( $self->debug );
                die $self->query_as_string( "$query", @{ $query->_bvalues } )
                  . "\n$@";
            }

            my $i = 0;
            foreach my $ref ( @{ $query->_btypes } ) {
                $i++;
                next unless $ref;
                my $type = $ref->{ $self->dbd } || $ref->{default};
                $sth->bind_param( $i, undef, eval "$type" );
                warn 'binding param ' . $i . ' with ' . $type
                  if ( $self->debug && $self->debug > 1 );
            }
            my $rv = $sth->execute( @{ $query->_bvalues } );
            warn "-- Result: $rv" if ( $self->debug );
            return $wantarray ? ( $sth, $rv ) : $sth;
        },
        catch => sub {
            die $_ if ( $self->debug );
            die $self->query_as_string( "$query", @{ $query->_bvalues } )
              . "\n$_";
        }
    );
}

sub txn {
    my $wantarray = wantarray;

    my $self          = shift;
    my $set_timestamp = !$self->_current_timestamp;

    if ($set_timestamp) {
        $self->_current_timestamp( $self->current_timestamp );
    }

    my @ret = $self->conn->txn(@_);

    if ($set_timestamp) {
        $self->_current_timestamp(undef);
    }

    return $wantarray ? @ret : $ret[0];
}

sub do {
    my $self = shift;
    my ( $sth, $rv ) = $self->sth(@_);
    $sth->finish();
    return $rv;
}

sub cursor {
    my $self  = shift;
    my $query = query(@_);
    my $sth   = $self->sth($query);

    return SQL::DB::Cursor->new(
        db    => $self,
        query => $query,
        sth   => $sth,
    );
}

sub fetch {
    my $self = shift;
    return $self->cursor(@_)->all;
}

sub fetch1 {
    my $self   = shift;
    my $cursor = $self->cursor(@_);
    my $first  = $cursor->next;

    $cursor->finish;
    return $first;
}

sub create_sequence {
    my $self = shift;
    my $name = shift;

    return if $self->dry_run;

    if ( $self->dbd eq 'SQLite' ) {
        $self->_sqlite_seq_dbh->do( 'CREATE TABLE sequence_' 
              . $name . ' ('
              . 'seq INTEGER PRIMARY KEY, mtime TIMESTAMP )' );
    }
    elsif ( $self->dbd eq 'Pg' ) {
        $self->conn->run(
            sub {
                $_->do( 'CREATE SEQUENCE seq_' . $name );
            },
            catch => sub {
                die $_;
            }
        );
    }
    else {
        die "Sequence support not implemented for " . $self->dbd;
    }
}

sub nextval {
    my $self = shift;
    my $name = shift;

    return if $self->dry_run;

    if ( $self->dbd eq 'SQLite' ) {
        warn 'INSERT INTO sequence_' 
          . $name
          . "(mtime) VALUES(CURRENT_TIMESTAMP)\n"
          if $self->debug;
        $self->_sqlite_seq_dbh->do( 'INSERT INTO sequence_' 
              . $name
              . '(mtime) VALUES(CURRENT_TIMESTAMP)' );
        return $self->_sqlite_seq_dbh->sqlite_last_insert_rowid();
    }
    elsif ( $self->dbd eq 'Pg' ) {
        warn "SELECT nextval('seq_" . $name . "')\n" if $self->debug;
        my $val = $self->conn->run(
            sub {
                $_->selectrow_array( "SELECT nextval('seq_" . $name . "')" );
            },
            catch => sub {
                die $_;
            }
        );
    }
    else {
        die "Sequence support not implemented for " . $self->dbd;
    }
}

sub current_timestamp {
    my $self = shift;
    return $self->_current_timestamp if $self->_current_timestamp;

    my ( $sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst ) = gmtime;
    $mon  += 1;
    $year += 1900;
    return sprintf( '%04d-%02d-%02d %02d:%02d:%02d',
        $year, $mon, $mday, $hour, $min, $sec );
}

sub query_as_string {
    my $self = shift;
    my $sql  = shift || confess 'usage: query_as_string($sql,@values)';
    my $dbh  = $self->conn->dbh;

    foreach (@_) {
        if ( defined($_) and $_ =~ /[^[:graph:][:space:]]/ ) {
            $sql =~ s/\?/*BINARY DATA*/;
        }
        else {
            my $quote;
            if ( defined $_ ) {
                $_ =~ s/\n.*/\.\.\./s;
                $quote = $dbh->quote("$_");
            }
            else {
                $quote = $dbh->quote(undef);
            }
            $sql =~ s/\?/$quote/;
        }
    }
    return $sql . ';';
}

# $db->insert_into('customers',
#     values => {cid => 1, name => 'Mark'}
# );
sub insert_into {
    my $self = shift;
    my $table = shift;
    shift;
    my $values = shift;

    my $urow = urow($table);

    my @cols = sort grep { $urow->can($_) } keys %$values;
    my @vals = map { $values->{$_} } @cols;

    @cols || croak 'insert_into requires columns/values';

    return $self->do(
        insert_into => SQL::DB::Expr->new(
            _txt => $table .'('. join(',',@cols) .')',
        ),
        sql_values( @vals )
    );
}

# $db->update('purchases',
#     set   => {pid => 2},
#     where => {cid => 1},
# );
sub update {
    my $self = shift;
    my $table = shift;
    shift;
    my $set = shift;
    shift;
    my $where = shift;

    if ( $self->debug ) {
        use Data::Dumper; $Data::Dumper::Indent = 1;
        $Data::Dumper::Maxdepth=2;
        warn Dumper( {table => $table, set => $set, where => $where});
    }

    my $urow = urow($table);
    my @updates = map { $urow->$_( $set->{$_} ) }
        grep { $urow->can($_) and !exists $where->{$_} } keys %$set;

    unless (@updates) {
        warn "Nothing to update for table: $table\n" if $self->debug;
        return;
    }

    my $expr = _expr_join(' AND ',
        map { $urow->$_ == $where->{$_} }
        grep { $urow->can($_) } keys %$where );

    $expr || croak 'update requires a valid where clause';
    return $self->do(
        update => $urow,
        set => \@updates,
        where => $expr,
    );
}

# $db->delete_from('purchases',
#    where => {cid => 1},
# );

sub delete_from {
    my $self = shift;
    my $table = shift;
    shift;
    my $where = shift;

    my $urow = urow($table);
    my $expr = _expr_join(' AND ',
        map { $urow->$_ == $where->{$_} } keys %$where );

    $expr || croak 'delete_from requires a where clause';
    return $self->do(
        delete_from => $urow,
        where => $expr,
    );
}

# my @objs = $db->select( ['pid','label],
#     from => 'customers',
#     where => {cid => 1},
# );
sub select {
    my $self = shift;
    my $list = shift;
    shift;
    my $table = shift;
    shift;
    my $where = shift;

    my $srow = srow($table);
    my @columns = map { $srow->$_ } @$list;

    @columns || croak 'select requires columns';

    my $expr = _expr_join(' AND ',
        map { $srow->$_ == $where->{$_} } keys %$where );

    return $self->fetch(
        select => \@columns,
        from => $srow,
        $expr ? (where => $expr) : (),
    ) if wantarray;

    return $self->fetch1(
        select => \@columns,
        from => $srow,
        $expr ? (where => $expr) : (),
    );
}

sub deploy {
    my $self = shift;
    my $ref  = shift;

    return $self->conn->txn(
        sub {
            my $dbh = $_;

            $dbh->do( '
            CREATE TABLE IF NOT EXISTS _sqldb (
                id INTEGER PRIMARY KEY,
                sql VARCHAR,
                perl VARCHAR
            )
        ' );

            my $latest_change_id = $self->conn->dbh->selectrow_array(
                'SELECT count(sql) FROM _sqldb');

            my $count = 1;
            foreach my $cmd (@$ref) {
                next unless ( $count > $latest_change_id );

                if ( defined $cmd->[0] ) {
                    $dbh->do($cmd);
                }
                elsif ( defined $cmd->[1] ) {
                    my $str = $cmd->[1];
                    eval $str;
                }
                else {
                    die "Require SQL or Perl";
                }
                $dbh->do( 'INSERT INTO _sqldb(sql) VALUES(?,?,?)',
                    undef, $count, @$cmd );

                $count++;
            }

        },
        catch => sub {
            die $_;
        }
    );
}


### CLASS FUNCTIONS ###

our @tables;

sub _getglob { no strict 'refs'; \*{ $_[0] } }

sub table {
    my ( $caller, $file, $line ) = caller;
    my $table = shift;

    my $srow = __PACKAGE__ . '::Srow::' . $table;
    my $urow = __PACKAGE__ . '::Urow::' . $table;

    {
        no strict 'refs';
        @{ *{ _getglob( $srow . '::ISA' ) }{ARRAY} } = ('SQL::DB::Expr');
        @{ *{ _getglob( $urow . '::ISA' ) }{ARRAY} } = ('SQL::DB::Expr');
    }

    my @columns;

    foreach my $def (@_) {
        my $col = $def;
        my $btype;

        if ( ref $def ) {
            $col = delete $def->{col};
            while ( my ( $key, $val ) = each %$def ) {
                if ( $key eq 'btype' ) {
                    $btype->{default} = $val;
                }
                elsif ( $key =~ s/^btype_(.*)/$1/ ) {
                    $btype->{$key} = $val;
                }
                else {
                    die "Unknown column parameter: $key";
                }
            }
        }

        install_sub(
            {
                code => sub {
                    my $row = shift;
                    SQL::DB::Expr->new(
                        _txt   => $row->_alias . '.' . $col,
                        _btype => $btype,
                    );
                },
                into => $srow,
                as   => $col,
            }
        );

        install_sub(
            {
                code => sub {
                    my $row = shift;    # but we don't care

                    if (@_) {
                        my $val = shift;
                        return SQL::DB::Expr->new(
                            _txt     => $col . ' = ?',
                            _btype   => $btype,
                            _bvalues => [$val],
                        );
                    }

                    return SQL::DB::Expr->new(
                        _txt   => $col,
                        _btype => $btype,
                    );
                },
                into => $urow,
                as   => $col,
            }
        );

        push( @columns, $col );
    }

    install_sub(
        {
            code => sub {
                my $row = shift;
                return map { $row->$_ } @columns;
            },
            into => $urow,
            as   => '_columns',
        }
    );

    install_sub(
        {
            code => sub {
                my $row = shift;
                return map { $row->$_ } @columns;
            },
            into => $srow,
            as   => '_columns',
        }
    );

    install_sub(
        {
            code => sub {
                return SQL::DB::Expr->new(
                    _txt => $table . '(' . join( ',', @_ ) . ') ', );
            },
            into => $caller,
            as   => 'sql_' . $table,
        }
    );

    push( @tables, $table );

}

sub srow {
    foreach my $try (@_) {
        croak "Table '$try' not defined" unless grep( /^$try$/, @tables );
    }

    my @ret;
    foreach my $name (@_) {
        my $srow = SQL::DB::Expr->new(
            _txt   => 'junk',
            _alias => $name,
        );
        bless $srow, __PACKAGE__ . '::Srow::' . $name;
        return $srow unless (wantarray);
        push( @ret, $srow );
    }
    return @ret;
}

sub urow {
    foreach my $try (@_) {
        die "Table '$try' not defined" unless grep( /^$try$/, @tables );
    }

    my @ret;
    foreach my $name (@_) {
        my $urow = SQL::DB::Expr->new(
            _txt   => $name,
        );
        bless $urow, __PACKAGE__ . '::Urow::' . $name;
        return $urow unless (wantarray);
        push( @ret, $urow );
    }
    return @ret;
}

sub query {
    return $_[0] if ( @_ == 1 and ref $_[0] eq 'SQL::DB::Expr' );
    my @statements;
    foreach my $item (@_) {
        if ( ref $item eq '' ) {
            ( my $tmp = $item ) =~ s/_/ /g;
            push( @statements, uc $tmp . "\n" );
        }
        elsif ( ref $item eq 'ARRAY' ) {
            push( @statements, '    ', _bexpr_join( ",\n    ", @$item ), "\n" );

         #FIXME
         #            push(@statements, '    ', query(",\n    ", @$item), "\n");
        }
        elsif ( $item->isa('SQL::DB::Expr') ) {
            push( @statements, '    ', $item, "\n" );
        }
    }

    my $e = _expr_join( '', @statements );
    $e->_txt( $e->_txt . "\n" ) unless ( $e->_txt =~ /\n$/ );
    return $e;
}

sub sql_and { _bexpr_join( ' AND ', @_ ) }

sub sql_case {
    @_ || croak 'case([$expr,] when => $expr, then => $val,[else...])';

    my @items = map {
        ( ref $_ eq '' && $_ =~ m/^((when)|(then)|(else))$/i )
          ? uc($_)
          : _bexpr($_)
    } @_;

    return _expr_join( ' ', 'CASE', @items, 'END' );
}

sub sql_coalesce { sql_func( 'COALESCE', @_ ) }

sub sql_cast { sql_func( 'CAST', @_ ) }

sub sql_concat { _expr_binary( '||', $_[0], $_[1] ) }

sub sql_count { sql_func( 'COUNT', @_ ) }

sub sql_exists { 'EXISTS(' . query(@_) . ')' }

sub sql_func {
    my $func = shift;
    return $func . '(' . _bexpr_join( ', ', @_ ) . ')';
}

sub sql_length { sql_func( 'LENGTH', @_ ) }

sub sql_lower { sql_func( 'LOWER', @_ ) }

sub sql_max { sql_func( 'MAX', @_ ) }

sub sql_min { sql_func( 'MIN', @_ ) }

sub sql_sum { sql_func( 'SUM', @_ ) }

sub sql_or { _bexpr_join( ' OR ', @_ ) }

sub sql_upper { sql_func( 'UPPER', @_ ) }

sub sql_values { sql_func( 'VALUES', @_ ) }

1;
__END__

=head1 NAME

SQL::DB - Perl/SQL database interface

=head1 VERSION

0.19_7. Development release.

=head1 SYNOPSIS

    use SQL::DB;
    my $db = SQL::DB->connect( $dsn, $dbuser, $dbpass );

    ### Basic Operations - thin wrappers around the main API

    $db->insert_into('purchases',
        values => {id => 1, count => 1},
    );

    $db->update('purchases',
        set   => {count => 2},
        where => {id => 1},
    );

    my $obj = $db->select( ['id','who','count'],
        from => 'customers',
        where => {id => 1},
    );
    # print $obj->id, $obj->who etc.

    $db->delete_from('purchases',
        where => {id => 1},
    );

    ### Advanced Operation - make the database do some real work

    # It is faster if we don't have to retrieve
    # this information from the DB
    table 'customers' => qw/ cid name surname age /;
    table 'products'  => qw/ pid label category /;
    table 'purchases' => qw/ pid cid /;

    # Works for both Pg and SQLite
    $db->create_sequence('purchases');
    my $id = $db->nextval('purchases');

    $db->txn( sub {

        my $purchases = srow('purchases');
        my $purchases2 = srow('purchases');


        $db->do(
            insert_into => sql_purchases('id','name'),
            select   => [ $id, $purchases->name ],
            from     => $purchases,
            where    => $purchases->id->in(
                select   => [ $purchases2->id ],
                from     => $purchases2,
                where    => ($purchases2->id != 1) .OR. (1),
            ),
            order_by => $purchases->name->desc,
            offset => 20,
            limit  => 5,
        );

        # Give everyone a birthday - calculated in the DB
        my $people = urow('people');
        $db->do(
            update => $people,
            set    => $people->age( $people->age + 1 ),
            where  => $people->dob == $today,
        );

        $db->do(
            delete_from => $people,
            where       => ($people->id > 1) .AND. ($people->name != 'Markb'),
        );

        $people = srow('people');

        my @people = $db->fetch1(
            select => [
                $people->id,
                $people->name,
            ],
            from => $people,
            where => $people->name->like('Mark%'),
        )

        # If you just want plain rows you can obtain the DBI
        # statement handle:
        my $sth = $db->sth( @query);
        map { print join(',',@$_) ."\n" } $sth->fetchall_arrayref;


    }, catch => sub {
        die "WTF: $_";
    });


=head1 DESCRIPTION

B<SQL::DB> is a Perl-to-SQL interface. By providing an interface that
is very close to real SQL, B<SQL::DB> give you unfettered access to the
power and flexibility of the underlying database. It aims to be a tool
for programmers who want their databases to work just as hard as their
Perl scripts.

B<SQL::DB> is capable of generating just about any kind of query,
including aggregate expressions, joins, nested selects, unions,
database-side operator invocations, and transactions. It has some minor
cross database support, keeping the amount of database-specific code
you have to write to a minimum.

Although rows can be retrieved from the database as simple objects,
B<SQL::DB> does not attempt to be an Object-Relational-Mapper al-la
L<DBIx::Class>.

there is This is nothing like a full-blown Object-Relational-Mapper (ORM) such
as L<DBIx::Class>in the The effort needed to use B<SQL::DB> is primarily related to learning
the query syntax, which is quite similar to SQL.

=head2 Abstract Rows and Expressions

B<SQL::DB> queries use abstract representations of table rows - objects
that can be thought of as matching I<any> row in a table.  Abstract
rows are obtained using the Schema->arow() method.  The objects
returned have methods that match the columns of a table, plus some
extra SQL syntax/sugar methods.

    my ( $cds, $tracks ) = $db->arow(qw/ cds tracks /);

The real power of B<SQL::DB> lies in the way that SQL expressions can
be constructed using these abstract columns. The abstract column
methods do not really act on the abstract row, but intead return an
expression object (L<SQL::DB::Expr>). Using Perl's overload feature
these objects can be combined and nested in Perl in a way that maps
very closely to they way the SQL would be written by hand.

    Perl Expression                     SQL Result
    ----------------------------------  ---------------------------
    $cds                                cds

    $cds->title                         cds.title

    $tracks->cd                         tracks.cd

    $tracks->id->func('count')          count(tracks.cd)

    $tracks->set_length(                SET tracks.length =
        $tracks->length + 10 )              tracks.length + 10

    ( $cds->id == 1 ) .OR.              cds.id = 1 OR
      $cds->title->like( '%Magic%' ) )      cds.title LIKE '%Magic%'

    ( $cds->id->is_not_null .AND.       cds.id IS NOT NULL AND
      ! $tracks->id->in( 1, 2, 5 ) )        NOT tracks.id IN (1,2,5)

Here is a summary of the default available expression operators and
methods. New expression subroutines can be generated in your own code -
see L<SQL::DB::Expr> for details.

    Perl            SQL             Applies to
    ---------       -------         ------------
    .AND.           AND             All Expressions
    .OR.            OR              All Expressions
    !               NOT             All Expressions
    .CONCAT.        || or CONCAT    All Expressions
    ==              =               All Expressions
    !=              !=              All Expressions

    like            LIKE            Column only
    in              IN              Column only
    not_in          NOT IN          Column only
    is_null         IS NULL         Column only
    is_not_null     IS NOT NULL     Column only
    asc             ASC             Column only
    desc            DESC            Column only
    count           COUNT           Column only
    min             MIN             Column only
    max             MAX             Column only
    func('x',...)   X(col,...)      Column only

=head2 Query Syntax

Here is a better example with multiple functions and multiple tables.
For each CD, show me the number of tracks, the length of the longest
track, and the total length of the CD in one query:

    track = $db->arow('tracks');
    @objs = $db->fetch(
        select   => [
            $track->cd->title,
            $track->id->func('count'),
            $track->length->func('max'),
            $track->length->func('sum')
        ],
        group_by  => [
            $track->cd->title,
        ],
    );

    foreach my $obj (@objs) {
        print 'Title: '            . $obj->title      ."\n";
        print 'Number of Tracks: ' . $obj->count_id   ."\n";
        print 'Longest Track: '    . $obj->max_length ."\n";
        print 'CD Length: '        . $obj->sum_length ."\n\n";
    }


=head2 Transactions, Savepoints, and Sequences

Emulated on systems that don't support native sequences. Based roughly
on the PostgreSQL api.


=head1 CONSTRUCTORS

=over 4

=item new( dsn => $dsn, ...)

The new() constructor requires at least a 'dsn' option, and most likely
you also want the 'dbuser' and 'dbpass' options as well. Also accepted
are 'dbattrs', 'debug', 'dry_run' and 'prepare_mode'. See ATTRIBUTES
below for the definitions.

=item connect($dsn,$dbuser,$dbpass,$dbattrs)

Similar to the L<DBI> connect() constructor.

=back

=head1 ATTRIBUTES

Unless specified all attributes can be read and set.

=over 4

=item debug <-> Bool

General debugging state (true/false). Debug messages are 'warn'ed.

=item dry_run <-> Bool

When set to false doesn't send any commands to the database.  I would
check the code to make sure I covered everything before you actually
rely on this.

=item dsn <-> Str

The L<DBI> connection string.

=item dbd -> Str

The L<DBD> driver name ('SQLite', 'mysql', 'Pg' etc) for the type of
database we are connected to. Derived from the 'dsn' attribute.

=item dbuser <-> Str

The L<DBI> connection username.

=item dbpass <-> Str

The L<DBI> connection password.

=item dbattrs <-> HASHREF

The L<DBI> connection attributes.

=item conn -> DBIx::Connector

The handle connecting us to the database.

=item prepare_mode -> Str

One of either 'prepare' or 'prepare_cached' (default). See L<DBI> for
details.

=back

=head1 METHODS

=over 4

=item BUILD

Documented here for completeness. This is used by the Moo object system
at instantiation time.

=item connect($dsn, $user, $pass, $attrs)

Connect to a database. The parameters are passed directly to
L<DBI>->connect. This method also informs the internal table/column
representations what type of database we are connected to, so they can
set their database-specific features accordingly. Returns the dbh.

=item create_table($name)

Creates the table $name and associated indexes and sequences in the
database.  Will warn and skip on any attempts to create tables that
already exist.

=item do(@query)

Constructs a L<SQL::DB::Query> object as defined by @query and runs
that query against the connected database.  Croaks if an error occurs.
This is the method to use for any statement that doesn't retrieve
values (eg INSERT, UPDATE and DELETE). Returns whatever value the
underlying L<DBI>->do call returns.  This method uses "prepare_cached"
to prepare the call to the database.

=item fetch(@query) -> SQL::DB::Cursor | @SQL::DB::Row

Constructs an L<SQL::DB::Query> object as defined by @query and runs
that query against the connected database.  Croaks if an error occurs.
This method should be used for SELECT-type statements that retrieve
rows. This method uses "prepare_cached" to prepare the call to the
database.

When called in array context returns a list of L<SQL::DB::Row> based
objects. The objects have accessors for each column in the query. Be
aware that this can consume large amounts of memory if there are lots
of rows retrieved.

When called in scalar context returns a query cursor
(L<SQL::DB::Cursor>) (with "next", "all" and "reset" methods) to
retrieve dynamically constructed objects one at a time.

=item fetch1(@query) -> SQL::DB::Row

Similar to fetch() but always returns only the first object from the
result set. All other rows (if any) can not be retrieved. You should
only use this method if you know/expect one result. This method uses
"prepare_cached" to prepare the call to the database.

=item query(@query)

Return an L<SQL::DB::Query> object as defined by @query. This method is
useful when creating nested SELECTs, UNIONs, or you can print the
returned object if you just want to see what the SQL looks like.

=item query_as_string($sql, @bind_values)

An internal function for pretty printing SQL queries by inserting the
bind values into the SQL itself. Returns a string.

=item current_timestamp

The current date and time (as a string) that remains fixed within a
transaction.

=item cursor( @query )

Runs a query and returns a L<SQL::DB::Cursor> object. You can call
next() and all() methods on this object to obtain data.

=item sth( @query )

Runs a query and returns a L<DBI::st> statement handle. You can call
fetchrow_array() and other L<DBI> method on this handle.

=item insert_into($row)

Insert $row into the database, where $row is an L<SQL::DB::Row> based
object returned from row() or fetch() methods.

=item update($row)

Update $row in the database, where $row is an L<SQL::DB::Row> based
object returned from row() or fetch() methods.

=item delete_from($row) -> Int

Delete $row from the database, where $row is an L<SQL::DB::Row> based
object returned from row() or fetch() methods.

=item select(\@columns, from => $table, where => \%args) -> Obj

Delete $row from the database, where $row is an L<SQL::DB::Row> based
object returned from row() or fetch() methods.

=item txn(&coderef)

Runs the code in &coderef as an SQL transaction. If &coderef does not
raise any exceptions then the transaction is commited, otherwise it is
rolled back.

This method can be called recursively, but any sub-transaction failure
will always result in the outer-most transaction also being rolled
back. For savepoint support see L<DBIx::Connector>.

=item create_sequence( $name )

Creates a sequence in the database. Takes the same arguments as
Sequences are emulated on SQLite, used directly for PostgreSQL, and are
unsupported for everything else.

    $db->create_sequence( 'myseq' );

=item nextval( $name )

Advance the sequence to its next value and return that value. If $count
is specified then a array of $count values are returned and the
sequence incremented appropriately.  Sequences are emulated on SQLite,
used directly for PostgreSQL, and are unsupported for everything else.

=item setval( $name, $value )

Unimplemented.

=item drop_sequence( $name )

Unimplemented.

=item deploy( $array_ref )

Deploy the [SQL,Perl] pairs contained in $array_ref to the database.
Only $array_ref elements greater than the previous deployment count
(stored in the _sqldb table) will be deployed.

=back

=head1 CLASS FUNCTIONS

All of the following functions can be exported on demand, or all at
once using the ':all' tag.

=over 4

=item table( $name, @columns )

Define a table in the database by table name and columns.

=item srow( $name, [...] ) -> SQL::DB::SRow::$name, ...

Returns an object (or objects in array context) representing any row of
table $name. This abstraction object is used for building SELECT
statements with the 'sth', 'fetch', 'fetch1', 'cursor' etc methods. The
returned object has a method for each column in the table.

=item urow( $name, [...] ) -> SQL::DB::URow::$name, ...

Returns an object (or objects in array context) representing any row of
table $name. This abstraction object is used for building UPDATE or
DELETE statements with the 'do' method. The returned object has a
method for each column in the table.

=item query( @statements )

Create a new L<SQL::DB::Expr> expression based on the given
@statements. Scalars are uppercased with '_' converted to spaces, and
ARRAYREF elements are joined together with ','.

=item sql_and

=item sql_case

=item sql_coalesce

=item sql_cast

=item sql_concat

=item sql_count

=item sql_exists

=item sql_func

=item sql_length

=item sql_lower

=item sql_max

=item sql_min

=item sql_sum

=item sql_or

=item sql_upper

=item sql_values

=back

=head1 COMPATABILITY

All SQL::DB releases have so far been DEVELOPMENT!

Version 0.19 was a complete rewrite based on Moo. Lots of things were
simplified, modules deleted, dependencies removed, etc. The API has
changed completely.

Version 0.13 changed the return type of the txn() method. Instead of a
2 value list indicating success/failure and error message, a single
L<Return::Value> object is returned intead.

=head1 SEE ALSO

L<DBIx::Connector>, L<SQL::DB::Expr>, L<SQL::DB::Cursor>,
L<SQL::DB::Schema>

=head1 SUPPORT

This distribution is still under development. Feedback, testing, bug
reports and patches are all welcome.

=over

=item Bug Reporting

    https://rt.cpan.org/Public/Bug/Report.html?Queue=SQL-DB

=item Source Code

    git clone git://github.com/mlawren/sql-db.git

=back

=head1 AUTHOR

Mark Lawrence E<lt>nomad@null.netE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2007-2011 Mark Lawrence <nomad@null.net>

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

=cut

# vim: set tabstop=4 expandtab:
