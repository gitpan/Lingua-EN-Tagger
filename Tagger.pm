package Lingua::EN::Tagger;

our $VERSION = '0.03';

use warnings;
use strict;

use Carp;
use File::Spec;
use FileHandle;
use HTML::TokeParser;
use Lingua::Stem::En;
use Lingua::EN::Sentence qw( get_sentences add_acronyms );
use Memoize;
use Storable;


# Class variables ( shared among instances )
our %_LEXICON;          # this holds the word lexicon
our %_HMM;              # this holds the hidden markov model for English grammar
our $MNP;               # this holds the compiled maximal noun phrase regex
our ( $lexpath, $word_path, $tag_path );
our ( $NUM, $GER, $ADJ, $PART, $NN, $PREP, $DET, $PAREN, $QUOT, $SEN, $WORD);



BEGIN {         #       REGEX SETUP 

        sub get_exp {
                my ( $tag ) = @_;
                return unless defined $tag;
                return qr|<$tag>[^<]+</$tag>\s*|;
        }
        
        $NUM  = get_exp( 'cd' );
        $GER  = get_exp( 'vbg' );
        $ADJ  = get_exp( 'jj[rs]*' );
        $PART = get_exp( 'vbn' );
        $NN   = get_exp( 'nn[sp]*' );
        $PREP = get_exp( 'in' );
        $DET  = get_exp( 'det' );
        $PAREN= get_exp( '[lr]rb' );
        $QUOT = get_exp( 'ppr' );
        $SEN  = get_exp( 'pp' );
        $WORD = get_exp( '\w+' );


        ( $lexpath ) = __FILE__ =~ /(.*)\.pm/;
        $word_path = File::Spec->catfile( $lexpath, 'pos_words.hash' );
        $tag_path = File::Spec->catfile( $lexpath, 'pos_tags.hash' );
}       
        

memoize('stem',
                 TIE => [ 'Memoize::ExpireLRU',
                         CACHESIZE => 100000,
                        ]);
                          


=head1 NAME

Lingua::EN::Tagger - Part-of-speech tagger for English natural language processing.


=head1 SYNOPSIS

        # Create a parser object
        my $p = new Lingua::EN::Tagger;
                
        # Add part of speech tags to a text
        my $tagged_text = $p->add_tags( $text );
        
        ...
        
        # Get a list of all nouns and noun phrases with occurence counts
        my %word_list = $p->get_words( $text );
        
        ...
        
        # Get a readable version of the tagged text
        my $readable_text = $p->get_readable( $text );
        

=head1 DESCRIPTION

The module is a probability based, corpus-trained tagger that assigns POS tags to 
English text based on a lookup dictionary and probability values.  The tagger
determines appropriate tags based on conditional probabilities - it looks at the
preceding tag to figure out what the appropriate tag is for the current word. 
Unknown words will be classified according to word morphology or can be set to
be treated as nouns or other parts of speech.

The tagger also recursively extracts as many nouns and noun phrases as it can, using a 
set of regular expressions. 

=head2 CLASS METHODS

=over 


=head1 METHODS
        
=over 

=item new %PARAMS

Class constructor.  Takes a hash with the following parameters (shown with default
values):

=over 

=item unknown_word_tag => ''

Tag to assign to unknown words

=item stem => 1

Stem single words using Lingua::Stem::EN

=item weight_noun_phrases => 1

When returning occurence counts for a noun phrase, multiply the value 
by the number of words in the NP.   

=item longest_noun_phrase => 50

Will ignore noun phrases longer than this threshold. This affects
only the get_words() and get_nouns() methods.

=item relax => 0

Relax the Hidden Markov Model: this may improve accuracy for 
uncommon words, particularly words used polysemously

=back

=cut

sub new {
        my ( $class, %params ) = @_;
        my $self = {    unknown_word_tag => '', 
                        stem => 1,
                        weight_noun_phrases => 1,
                        longest_noun_phrase => 50,
                        tag_lex => 'tags.yml',
                        word_lex => 'words.yml',
                        unknown_lex => 'unknown.yml',
                        word_path => $word_path,
                        tag_path => $tag_path,
                        relax => 0,
                        debug => 0,
                        %params };
        
        bless $self, $class;
        
        unless ( -f $self->{'word_path'} and -f $self->{'tag_path'} ){
                carp "Couldn't locate POS lexicon, compiling new one" if $self->{'debug'};
                $self->install();
        } else {
                %_LEXICON = %{ retrieve( $self->{'word_path'} ) }; # A hash of words and corresponding parts of speech
                %_HMM = %{ retrieve( $self->{'tag_path'} ) };   # A hash of adjacent part of speech tags and the probability of each
        }

        $MNP = $self->_get_max_noun_regex();
        $self->_reset();
        
        return $self;
}
        
=over 

=item add_tags TEXT
 
Examine the string provided and return it fully tagged ( XML style )

=cut

sub add_tags {
        
        my ( $self, $text ) = @_;
        
        return unless $self->_valid_text( $text );

        my @text = $self->_clean_text( $text );
        my $t = $self->{'current_tag'}; # shortcut
        my ( @tags ) = 
                map {   
                        $t = $self->_assign_tag( $t, $self->_clean_word( $_ )) 
                                || $self->{'unknown_word_tag'} || 'nn';
                       "<$t>$_</$t>"
                } @text;
        $self->{'current_tag'} = $t;
        $self->_reset;
        return join ' ', @tags;
}       



=item get_words TEXT

Given a text string, return as many nouns and
noun phrases as possible.  Applies L<add_tags> and involves three stages:

=over 

=item * Tag the text

=item * Extract all the maximal noun phrases

=item * Recursively extract all noun phrases from the MNPs

=back

=cut

                
sub get_words {
        my ( $self, $text ) = @_;
        
        return unless $self->_valid_text( $text );
        
        my $tagged = $self->add_tags( $text );
        
        if( $self->{'longest_noun_phrase'} <= 1 ){
                return $self->get_nouns( $tagged );
        } else {
                return $self->get_noun_phrases( $tagged );
        } 
}
        

=item get_readable TEXT

Return an easy-on-the-eyes tagged version of a text string.  Applies
L<add_tags> and reformats to be easier to read.

=cut            

sub get_readable {
        my ( $self, $text ) = @_;

        return unless $self->_valid_text( $text );
        
        my $tagged =  $self->add_tags( $text );
        $tagged =~ s/<[a-z]+>([^<]+)<\/([a-z]+)>/$1\/\U$2/go;
        return $tagged;
}       



###########################################
# _valid_text TEXT
# 
# Check whether the text is a valid string
###########################################
sub _valid_text {
        my ( $self, $text ) = @_;
        if( !defined $text ){
                # $text is undefined, nothing to parse
                carp "method call on uninitialized variable" if $self->{'debug'};
                return undef;
        } elsif ( ref $text ){
                # $text is a scalar reference, don't parse
                carp "method call on a scalar reference" if $self->{'debug'};
                return undef;
        } elsif ( $text =~ /^\s*$/ ){
                # $text is defined as an empty string, nothing to parse
                return undef;
        } else {
                # $text is valid
                return 1;
        }
}               


#####################################################################
# _strip_tags TEXT
#
# Return a text string with the XML-style part-of-speech tags removed.
#####################################################################
sub _strip_tags {
        my ( $self, $text ) = @_;
        return unless $self->_valid_text( $text );
                        
        $text =~ s/<[^>]+>//gs;
        $text =~ s/\s+/ /gs;
        $text =~ s/^\s*//;
        $text =~ s/\s*$//;
        return lc($text);
}
                
                
#####################################################################
# _clean_text TEXT
#
# Strip the provided text of HTML-style tags and separate off
# any punctuation in preparation for tagging
#####################################################################
sub _clean_text {
        my ( $self, $text ) = @_;
        return unless $self->_valid_text( $text );
        
        # Strip out any markup and convert entities to their proper form
        my $html_parser = HTML::TokeParser->new( \$text );
        my $cleaned_text = $html_parser->get_text;
        while( $html_parser->get_token ){
                $cleaned_text .= ( $html_parser->get_text )." ";
        }
        
        # Add some acronyms not included in Lingua::EN::Sentence
        add_acronyms('brig','mfg','messrs', @{$self->{'acronyms'} } );
        my $sentences = get_sentences( $cleaned_text );

        # Tokenize the sentences (splitting on punctuation as you go)
        my @tokenized = map { $self->_split_punct( $_ ) }
                        map { s/([\.\?\!]+)(\W*)$/ $1 $2 /; split /\s+/ }
                                @$sentences;
        
        return @tokenized;
}


###########################################################################
# _split_punct TERM
# 
# Separate punctuation from words, where appropriate. This leaves trailing 
# periods in place to be dealt with later. Called by the _clean_text method.
###########################################################################
sub _split_punct {
        
        local $_ = $_[1];       

        # If there's no punctuation, return immediately
        return $_ if /^[a-zA-Z]+$/;
        
        # Sanity checks
        s/\W{10,}/ /og;         # get rid of long trails of non-word characters
        
        # Put quotes into a standard format
        s/`(?!`)(?=.*\w)/` /og;         # Shift left quotes off text
        s/"(?=.*\w)/ `` /og;            # Convert left quotes to `` 
        s/(?<![\w\s'])'(?=.*\w)/ ` /go; # Convert left quotes to ` 
        s/"/ '' /og;                    # Convert (remaining) quotes to ''
        s/(?<=\w)'(?!')(?=\W|$)/ ' /go; # Separate right single quotes

        # Handle all other punctuation
        s/--+/ - /go;                   # Convert and separate dashes
        s/,(?!\d)/ , /go;               # Shift commas off everything but numbers
        s/:$/ :/go;                     # Shift semicolons off
        s/(\.\.\.+)/ $1 /;              # Shift elipses off 
        s/(?![`'\.,\s:-])(\W)/ $1 /go;  # Separate all other punctuation      

        # English-specific contractions
        s/(?<=[a-zA-Z])'([dms])\b/ '$1/go;      # Separate off 'd 'm 's
        s/n't\b/ n't/go;                        # Separate off n't      
        s/'(ve|ll|re)\b/ '$1/go;                # Separate off 've, 'll, 're
        
        return split;
}



#####################################################################
# _assign_tag TAG, WORD ( memoized )
#
# Given a preceding tag TAG, assign a tag to WORD.   
# Called by the choose_tag method.
# This subroutine is a modified version of the Viterbi algorithm
# for part of speech tagging
#####################################################################
my %seen;       # memoize
sub _assign_tag {

        my ( $self, $prev_tag, $word) = @_;

        return $seen{$prev_tag}{$word} if exists( $seen{$prev_tag}{$word});
        
        if ( $self->{'unknown_word_tag'} and $word eq "-unknown-" ){
                # If the 'unknown_word_tag' value is defined,
                # classify unknown words accordingly
                $seen{$prev_tag}{$word} = $self->{'unknown_word_tag'};
                return $self->{'unknown_word_tag'};
        } elsif ( $word eq "-sym-" ){
                # If this is a symbol, tag it as a symbol
                $seen{$prev_tag}{$word} = "sym";
                return "sym";
        }
        
        my $best_so_far = 0;
        
        my $w = $_LEXICON{$word};
        my $t = \%_HMM;
        
        ##############################################################
        # TAG THE TEXT
        # What follows is a modified version of the Viterbi algorithm
        # which is used in most POS taggers
        ##############################################################
        my $best_tag;
        
        foreach my $tag ( keys %{ $t->{$prev_tag} } ){
                
                # With the $self->{'relax'} var set, this method
                # will also include any `open classes' of POS tags
                my $pw;
                if( defined ${ $w->{$tag} } ){
                        $pw = ${ $w->{$tag} };
                } elsif ( $self->{'relax'} and  $tag =~ /^(?:jj|nn|rb|vb)/  ){
                        $pw = 0;
                } else {
                        next;
                }
                
                # Baysian logic: 
                # P =  P( $tag | $prev_tag ) * P( $tag | $word )
                my $probability =
                        $t->{$prev_tag}{$tag} * ( $pw + 1 );
                                
                # Set the tag with maximal probability
                if( $probability > $best_so_far ) { 
                        $best_so_far = $probability;
                        $best_tag = $tag;
                }
        }
        
        $seen{$prev_tag}{$word} = $best_tag;
        return $best_tag;
}
        
        
############################################################################
# _reset
#
# ehis subroutine will reset the preceeding tag to a sentence ender ( PP ). 
# This prepares the first word of a new sentence to be tagged correctly.
############################################################################
sub _reset {
        my ( $self ) = @_;
        $self->{'current_tag'} = 'pp';
}

#####################################################################
# _clean_word WORD
#
# This subroutine determines whether a word should be considered in its 
# lower or upper case form. This is useful in considering proper nouns
# and words that begin sentences. Called by L<choose_tag>.
#####################################################################
sub _clean_word {
        
        my (  $self, $word ) = @_;

        if ( defined $_LEXICON{$word} ) {
                # seen this word as it appears (lower or upper case) 
                return $word;
                
        } elsif ( defined $_LEXICON{lcfirst $word} ) {
                # seen this word only as lower case 
                return lcfirst $word;
                
        } else {
                # never seen this word. guess.
                return $self->_classify_unknown_word( $word );
        }
}


#####################################################################
# _classify_unknown_word WORD
#
# This changes any word not appearing in the lexicon to identifiable 
# classes of words handled by a simple unknown word classification 
# metric. Called by the _clean_word method.
#####################################################################
sub _classify_unknown_word {
        my ( $self, $word) = @_;
        local $_ = $word;

        if( m/[\(\{\[]/ ){ # Left brackets
                $word = "*LRB*";
        
        } elsif( m/[\)\]\}]/o ){ # Right brackets
                $word = "*RRB*";
        
        } elsif ( m/-?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)/ ){ # Floating point number
                $word = "*NUM*";
        
        } elsif ( m/^[0-9]+[\d\/:-]+[0-9]/ ){ # Other number constructs
                $word = "*NUM*";
                
        } elsif ( m/^-?[0-9]+\w+$/o ){  # Ordinal number
                $word = "*ORD*";
        
        } elsif ( m/^[A-Z][A-Z\.-]*$/o ) { # Abbreviation (all caps)
                $word = "-abr-";
        
        } elsif ( m/\w-\w/o ){ # Hyphenated word
                my ( $h_suffix) = m/-([^-]+)$/;
                
                if ( defined ${ $_LEXICON{$h_suffix}{'jj'} } ){
                        # last part of this is defined as an adjective
                        $word = "-hyp-adj-";
                } else {
                        # last part of this is not defined as an adjective
                        $word = "-hyp-";
                }
        
        } elsif ( m/^\W+$/o ){ # Symbol
                $word = "-sym-";
                
        } elsif ( $_ eq ucfirst ) { # Capiitalized word
                $word = "-cap-";
                
        } elsif (  m/ing$/o ) { # Ends in 'ing'
                $word = "-ing-";
        
        } elsif( m/s$/o ) { # Ends in 's'
                $word = "-s-";
        
        } elsif ( m/tion$/o ){ # Ends in 'tion'
                $word = "-tion-";
        
        } elsif ( m/ly$/o ){ # Ends in 'ly'
                $word = "-ly-";
        
        } elsif ( m/ed$/o ){ # Ends in 'ed'
                $word = "-ed-";
                
        } else { # Completely unknown
                $word = "-unknown-";
        }
        
        return $word;
}
                



#####################################################################
# stem WORD ( memoized )
#
# Returns the word stem as given by L<Lingua::Stem::EN>. This can be
# turned off with the class parameter 'stem' => 0.
#####################################################################
sub stem {
        my ( $self, $word ) = @_;
        return $word unless $self->{'stem'};

        my $stemref = Lingua::Stem::En::stem( -words => [ $word ] );
        return $stemref->[0];
}




#####################################################################
# _get_max_noun_regex
#
# This returns a compiled regex for extracting maximal noun phrases  
# from a POS-tagged text.
#####################################################################
sub _get_max_noun_regex {
        my $regex = qr/ 
                (?:$NUM)?(?:$GER|$ADJ|$PART)*   # optional number, gerund - adjective -participle
                        (?:$NN)+                # Followed by one or more nouns

                        (?:
                                (?:$PREP)*(?:$DET)?(?:$NUM)? # Optional preposition, determinant, cardinal
                                (?:$GER|$ADJ|$PART)*    # Optional gerund-adjective -participle
                                (?:$NN)+                # one or more nouns
                        )*
                /xo;            
        return $regex;
}



######################################################################
=item get_nouns TAGGED_TEXT

Given a POS-tagged text, this method returns all nouns and their 
occurance frequencies.

=cut
######################################################################
sub get_nouns {
        my ( $self, $text ) = @_;
        
        return unless $self->_valid_text( $text );
        
        my @trimmed =   map { $self->_strip_tags( $_ ) }
                                ( $text =~ /($NN)/gs );

        my %return;
        foreach my $n ( @trimmed ) {
                
                $n = $self->stem( $n );
                next unless length($n) < 100; # sanity check on word length
                $return{$n}++ unless $n =~ /^\s*$/;
        
        }
        
        return %return;
}


######################################################################
=item get_max_noun_phrases TAGGED_TEXT

Given a POS-tagged text, this method returns only the maximal noun phrases.
May be called directly, but is also used by L<get_noun_phrases>

=cut
######################################################################
sub get_max_noun_phrases {
        my ( $self, $text ) = @_;
                        
        return unless $self->_valid_text( $text );
        
        my @mn_phrases =   map { $self->_strip_tags( $_ ) }
                                ( $text =~ /($MNP)/gs );
        
        my %return;
        foreach my $p ( @mn_phrases ) {
        
                $p = $self->stem( $p )
                        unless $p =~ /\s/; # stem single words
                
                $return{$p}++ unless $p =~ /^\s*$/;
        
        }
        
        return %return;


}

######################################################################
=item get_noun_phrases TAGGED_TEXT

Similar to get_words, but requires a POS-tagged text as an argument.

=cut
######################################################################
sub get_noun_phrases {
        
        my ( $self, $text ) = @_;

        return unless $self->_valid_text( $text );

        my $found;
        my $phrase_ext = qr/(?:$PREP|$DET|$NUM)+/xo;
        
        # Find MNPs in the text, one sentence at a time
        # Record and split if the phrase is extended by a (?:$PREP|$DET|$NUM)
        my @mn_phrases =  map { $found->{$_}++ if m/$phrase_ext/; split /$phrase_ext/ }
                        ( $text =~ /($MNP)/gs );
        
        foreach( @mn_phrases ){

                # Split the phrase into an array of words, and
                # create a loop for each word, shortening the
                # phrase by removing the word in the first position 
                # Record the phrase and any single nouns that are found
                
                my @words = split;
                
                for( 0 .. $#words ){
                
                        $found->{ join( " ", @words ) }++ if scalar @words > 1;
                        my $w = shift @words;
                        $found->{$w}++ if $w =~ /$NN/;
                
                }
        }
                                        
        my %return;
        foreach( keys %{ $found } ){
                my $k = $self->_strip_tags( $_ );
                my $v = $found->{$_};
                
                # We weight by the word count to favor long noun phrases
                my @space_count = $k =~ /\s+/go; 
                my $word_count = scalar @space_count + 1;
                
                # Throttle MNPs if necessary
                next if $word_count > $self->{'longest_noun_phrase'};
                
                $k = $self->stem( $k ) unless $word_count > 1; # stem single words
                my $multiplier = 1;
                $multiplier = $word_count if $self->{'weight_noun_phrases'};
                $return{$k} += ( $multiplier * $v );
        }
        
        return %return;
}


######################################################################
=item install

Reads some included corpus data and saves it in a stored hash on the 
local filesystem. This is called automatically if the tagger can't 
find the stored lexicon. 

=cut
######################################################################
sub install {
        my ( $self ) = @_;
                
        carp "Compiling part-of-speech lexicon" if $self->{'debug'};
        $self->_load_tags( $self->{'tag_lex'} );
        $self->_load_words( $self->{'word_lex'} );
        $self->_load_words( $self->{'unknown_lex'} );
        store \%_LEXICON, $self->{'word_path'};
        store \%_HMM, $self->{'tag_path'};
}



######################################################## 
#       LOAD THE 2-GRAMS INTO A HASH FROM YAML DATA
# 
# This is a naive (but fast) YAML data parser. It will 
# load a YAML document with a collection of key: value 
# entries ( {pos tag}: {probability} ) mapped onto 
# single keys ( {tag} ). Each map is expected to be on a
# single line; i.e., det: { jj: 0.2, nn: 0.5, vb: 0.0002 } 
#########################################################
sub _load_tags {

        my ( $self, $lexicon ) = @_;

        my $path = File::Spec->catfile( $lexpath, $lexicon );
        my $fh = new FileHandle $path or die $!;
        while( <$fh> ){
                next unless my ( $key, $data ) = m/^"?([^{"]+)"?: { (.*) }/;
                my %tags = split /[:,]\s+/, $data;
                foreach( keys %tags ){
                        $_HMM{$key}{$_} = $tags{$_};
                }
        }
        $fh->close;
}
        




######################################################### 
#       LOAD THE WORD LEXICON INTO A HASH FROM YAML DATA
# 
# This is a naive (but fast) YAML data parser. It will 
# load a YAML document with a collection of key: value 
# entries ( {pos tag}: {count} ) mapped onto single 
# keys ( {a word} ). Each map is expected to be on a
# single line; i.e., key: { jj: 103, nn: 34, vb: 1 } 
#########################################################
sub _load_words {
        
        my ( $self, $lexicon ) = @_;
        my $path = File::Spec->catfile( $lexpath, $lexicon );
        
        my $fh = new FileHandle $path or die $!;
        while( <$fh> ){
                next unless my ( $key, $data ) = m/^"?([^{"]+)"?: { (.*) }/;
                my %tags = split /[:,]\s+/, $data;
                foreach( keys %tags ){
                        $_LEXICON{$key}{$_} = \$tags{$_};
                }
        }
        $fh->close;
}       




############################
#       RETURN TRUE
############################
1;



__END__

=head1 HISTORY

=over 

=item 0.03

11/03 Fixed some errors in the text scrubbing methods
Shortened and moved lexicon, made things run faster
Added a testing suite
(Aaron Coburn)

=item 0.02

5/03 Applied fixes for module installer from Nathaniel Irons

=item 0.01

Created 10/02 by Aaron Coburn as LSI::Parser::POS
Moved to  Lingua::EN::Tagger 2/03 Maciej Ceglowski


=back 

=head1 AUTHORS

        Maciej Ceglowski <developer@ceglowski.com>
        Aaron Coburn <acoburn@middlebury.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of version 2 of the GNU General Public License as
published by the Free Software Foundation.


=cut
