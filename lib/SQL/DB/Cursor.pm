package SQL::DB::Cursor;
use Moo;
use Sub::Install qw/install_sub/;
use Carp qw(croak);

our $VERSION = '0.97_1';

has 'sth' => (
    is       => 'ro',
    required => 1,
);

has 'class' => (
    is       => 'rw',
    writer   => '_class',
    init_arg => undef,
);

# '_done' is a bool set to true when there are no more rows to
# be returned.
has '_done' => (
    is       => 'rw',
    init_arg => undef,
);

my %classes;

sub BUILD {
    my $self = shift;
    return if $self->class;

    my @cols = map { $_ =~ s/\s+/_/g; $_ } @{ $self->sth->{NAME_lc} };
    my $class = 'SQL::DB::Row::' . join( '_', @cols );

    if ( !$classes{$class} ) {
        my $i = 0;
        my $x = eval $i;
        foreach my $col (@cols) {
            my $x = eval $i;
            install_sub(
                {
                    code => sub {
                        $_[0]->[$x] = $_[2] if @_ == 2;
                        return $_[0]->[$x];
                    },
                    into => $class,
                    as   => $col,
                }
            );
            $i++;
        }
        $classes{$class} = 1;
    }
    $self->class($class);
}

sub next {
    my $self = shift;
    return if ( $self->_done );

    my @values = $self->sth->fetchrow_array;

    if ( !@values ) {
        $self->finish;
        return;
    }
    return bless \@values, $self->class;
}

sub all {
    my $self = shift;
    my @all;
    while ( my @values = $self->sth->fetchrow_array ) {
        push( @all, bless \@values, $self->class );
    }
    $self->finish;
    return @all;
}

sub finish {
    my $self = shift;
    return if $self->_done;
    $self->sth->finish;
    $self->_done(1);
}

sub DESTROY {
    my $self = shift;
    $self->finish;
}

1;
__END__


=head1 NAME

SQL::DB::Cursor - SQL::DB database cursor

=head1 SYNOPSIS

  use SQL::DB::Cursor;

  my $cursor = SQL::DB::Cursor->new(
      sth => $sth,
  );

  while (my $row = $cursor->next) {
      print $row->column(), $row->some_other_column;
  }

  # Or if you want you can get everything at once:
  my @rows = $cursor->all;

=head1 DESCRIPTION

B<SQL::DB::Cursor> is a class for traversing over records retrieved
from a L<DBI> statement handle. Note that this is a Perl-side cursor,
completely unrelated to any database-side cursors you might have.

The objects returned by the 'next' and 'all' methods are simple
ARRAY(ref) based objects with one accessor/modifer method per column.
Method names are column names but with any spaces replaced by a '_'.

=head1 CONSTRUCTOR

=over 4

=item new(sth => $sth)

Returns a new cursor object. $sth must be a L<DBI> statement handle
that has already been 'executed'.

=back

=head1 ATTRIBUTES

=over 4

=item sth -> DBI::st

The L<DBI> statement handle. Read-only.

=item class -> Str

The class name into which retrieved rows are 'blessed'. This is
automatically calculated based on the retrieved column names.
Read-only.

=back

=head1 METHODS

=over 4

=item BUILD

Internal method.

=item next -> $row

Returns the next row from the statement handle as an object. Returns
undef when there is no more data.

=item all -> @rows

Returns all remaining rows from the statement handle as a list of
objects. Returns the empty list if no data is available.

=item finish

Calls finish() on the DBI statement handle and internally records that
there is no more data to be retrieved. This method is automatically
called when the cursor object goes out of scope, so be aware you cannot
use the passed-in statement handle again.

=back

=head1 SEE ALSO

L<DBI>, L<SQL::DB>

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
