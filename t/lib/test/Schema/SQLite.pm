package test::Schema::SQLite;
use strict;
my $VAR1 = [[undef,'main','_deploy','app',undef,'VARCHAR','40',undef,undef,undef,0,undef,undef,undef,undef,undef,1,'NO'],[undef,'main','_deploy','seq',undef,'INTEGER',undef,undef,undef,undef,0,undef,'0',undef,undef,undef,2,'NO'],[undef,'main','_deploy','ctime',undef,'TIMESTAMP',undef,undef,undef,undef,0,undef,'CURRENT_TIMESTAMP',undef,undef,undef,3,'NO'],[undef,'main','_deploy','type',undef,'VARCHAR','20',undef,undef,undef,1,undef,undef,undef,undef,undef,4,'YES'],[undef,'main','_deploy','data',undef,'VARCHAR',undef,undef,undef,undef,1,undef,undef,undef,undef,undef,5,'YES'],[undef,'main','actors','id',undef,'integer',undef,undef,undef,undef,1,undef,undef,undef,undef,undef,1,'YES'],[undef,'main','actors','name',undef,'varchar',undef,undef,undef,undef,1,undef,undef,undef,undef,undef,2,'YES'],[undef,'main','films','id',undef,'integer',undef,undef,undef,undef,1,undef,undef,undef,undef,undef,1,'YES'],[undef,'main','films','title',undef,'varchar',undef,undef,undef,undef,1,undef,undef,undef,undef,undef,2,'YES']];;

sub definition { $VAR1; }
sub clear { undef $VAR1; }

1;
__END__

=head1 NAME

test::Schema::SQLite - An SQL::DB::Schema definition

=head1 SYNOPSIS

    use SQL::DB; # or anything that extends SQL::DB

    my $db = SQL::DB->connect(
        dsn      => 'dbi:SQLite:...',
        username => 'username',
        password => 'password',
        schema   => 'test::Schema',
    );

=head1 DESCRIPTION

See L<sqldb-schema>(1) for details.

Generated:

    date:    Fri Jun 22 16:58:32 2012
    program: App::sqldb_schema
    source:  dbi:SQLite:dbname=/tmp/Test-Database-mark/SQLite/tdd_sqlite_mark_0

=head1 AUTHOR

Mark Lawrence E<lt>nomad\@null.netE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2011-2012 Mark Lawrence E<lt>nomad\@null.netE<gt>

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

=cut
# vim: set tabstop=4 expandtab: