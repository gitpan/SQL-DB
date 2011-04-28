use strict;
use warnings;
use Test::More tests => 1;
use SQL::DB::Cursor;

can_ok(
    'SQL::DB::Cursor', qw/
      new
      next
      all
      finish
      /
);

