use strict;
use warnings;
use Test::More;
use SQL::DB;

my @columns = (qw/id name dob/);

table 'people' => @columns;

my $speople = srow('people');
can_ok $speople, @columns;

my $upeople = urow('people');
can_ok $upeople, @columns;

foreach my $col ( @columns ) {
    is $speople->$col, 'people0.'.$col, $speople->$col;
    is $upeople->$col, $col, $upeople->$col;
    is $upeople->$col(1), $col .' = ?', $upeople->$col(1);
}

is_deeply [$speople->_columns],
    ['people0.id', 'people0.name', 'people0.dob'], 'srow _columns';

is_deeply [$upeople->_columns],
    ['id', 'name', 'dob'], 'urow _columns';


done_testing();
