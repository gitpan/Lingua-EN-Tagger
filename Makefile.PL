use warnings;
use strict;
use ExtUtils::MakeMaker;
use FileHandle;

my %_LEXICON;
my %_HMM;

# This makefile written by Aaron Coburn

if( install() ){
WriteMakefile(
    'NAME'              => 'Lingua::EN::Tagger',
    'VERSION_FROM'      => 'Tagger.pm', # finds $VERSION
    'PREREQ_PM'         => { 'Lingua::Stem::En' => '0.01', 
                                'Lingua::EN::Sentence' => '0.01',
                                'HTML::TokeParser' => '0.01',
                                'Memoize' => '0.01',
                                'File::Spec' => '0.01',
                                'Storable' => '0.01',
                                'Carp' => '0.01',
                        }, # e.g., Module::Name => 1.1
    'AUTHOR' => 'Maciej Ceglowski and Aaron Coburn <maciej@ceglowski.com>, <acoburn@middlebury.edu>',
  
);

} else {
        die "Encountered problems installing the lexicon!\nMakefile not written!\n";
}
        

sub install {
        use Storable;
        use File::Spec;
        print "Creating part-of-speech lexicon\n";
        my $lex_dir = 'Tagger';
        my $word_path = File::Spec->catfile( $lex_dir, 'pos_words.hash' );
        my $tag_path = File::Spec->catfile( $lex_dir, 'pos_tags.hash' );
        _load_tags( File::Spec->catfile( $lex_dir, 'tags.yml' ) );
        _load_words( File::Spec->catfile( $lex_dir, 'words.yml' ) );
        _load_words( File::Spec->catfile( $lex_dir, 'unknown.yml' ) );
        store \%_LEXICON, $word_path;
        store \%_HMM, $tag_path;
        if( -f $word_path and -f $tag_path ){
                return 1;
        } else {
                return 0;
        }
}

sub _load_words {
        my ( $file ) = @_;
        my $fh = new FileHandle $file;
        while ( <$fh> ){
                my ( $key, $data ) = m/^"?([^{"]+)"?: { (.*) }/;
                next unless $key and $data;
                my %tags = split /[:,]\s+/, $data;
                foreach( keys %tags ){
                        $_LEXICON{$key}{$_} = \$tags{$_};
                }
        }
        $fh->close;
}

sub _load_tags {
        my ( $file ) = @_;
        my $fh = new FileHandle $file;
        while ( <$fh> ){
                my ( $key, $data ) = m/^"?([^{"]+)"?: { (.*) }/;
                next unless $key and $data;
                my %tags = split /[:,]\s+/, $data;
                foreach( keys %tags ){
                        $_HMM{$key}{$_} = $tags{$_};
                }
        }
        $fh->close;
}

        
