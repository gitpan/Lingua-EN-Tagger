# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';
use Test::More tests => 7;
use Lingua::EN::Tagger;

ok('Lingua::EN::Tagger', 'module compiled'); # If we made it this far, we're ok.

#########################

# Insert your test code below, the Test module is use()ed here so read
# its man page ( perldoc Test ) for help writing this test script.

$p = new Lingua::EN::Tagger;

@phrases = (    "Andrew W. Mellon Foundation", 
                "Spring Clearance at J.C. Penny",
                "Prof. Paul Nelson",
                "Apr. 12, 1953",
                "On Dec. 24, 1994",
                "The mtn. road"
        );

$i=0;
foreach( @phrases ){
        $s = join ' ', $p->_split_sentences( &prepare( $_ ) );
        is( $s, $_, "sentence ender $i");
        $i++;
}

sub prepare {
        
        ( $sentence ) = @_;
        @s = split /\s+/, $sentence;
        return \@s;
}
