package Lingua::Tagger::EN;

our $VERSION = '0.01';

use warnings;
use strict;

use Lingua::Stem::En;
use File::Spec;
use PerlIO::gzip;
use LWP::UserAgent;
use FileHandle;
use Storable;
use Carp;





# Class variables ( shared among instances )
our %_keyword_hash;
our %_words;
our %_tags;
our $lexpath = "~/.POS_lexicon";
our $lexurl = "http://javelina.cet.middlebury.edu/lsi/parser/";

our $word_file = 'pos_words.hash';
our $tag_file = 'pos_tags.hash';
our $mnp; 

our $word_path = File::Spec->catfile( $lexpath, $word_file );
our $tag_path = File::Spec->catfile( $lexpath, $tag_file );

our( $NUM, $GER, $ADJ, $PART, $NN, $PREP, $DET, $CARD, $PAREN, $QUOT, $SEN, $WORD);



BEGIN {		#	REGEX SETUP 

	sub get_exp {
		my ( $tag ) = @_;
		return qr|<$tag>[^<]+</$tag>\s*|;
	}
	$NUM  = get_exp( 'cd' );
	$GER  = get_exp( 'vbg' );
	$ADJ  = get_exp( 'jj[rs]*' );
	$PART = get_exp( 'vbn' );
    $NN   = get_exp( 'nn[sp]*' );
	$PREP = get_exp( 'in' );
	$DET  = get_exp( 'det' );
	$CARD = get_exp( 'cd' );
	$PAREN= get_exp( 'rrb' );
	$QUOT = get_exp( 'ppr' );
	$SEN	 = get_exp( 'pp' );
	$WORD = get_exp( '\w+' );

}


mkdir $lexpath unless -d $lexpath;


unless ( -f $word_path and -f $tag_path ){
 	__PACKAGE__->initialize();
}

%_words =%{ retrieve( $word_path ) };	# A hash of words and corresponding parts of speech
%_tags = %{ retrieve( $tag_path ) };	# A hash of adjacent part of speech tags and the probability of each


=head1 NAME

Lingua::EN::Tagger - Part of speech tagger for English natural language processing.


=head1 SYNOPSIS
		
	# Add part of speech tags to a text
	my $tagged_text = $p->add_tags( $text );
	
	...
	
	# Get a list of all noun phrases with occurence counts
	my %word_list = $p->get_words( $text );
	
	...
	
	# Get a readable version of the tagged text
	my $readable_text = $p->get_readable( $text );
	

=head1 DESCRIPTION

The module is a probability based, corpus-trained tagger that assigns POS tags to 
English text based on a lookup dictionary and probability values.  The tagger
determines appropriate tags based on conditional probabilities - it looks at the
preceding tag to figure out what the appropriate tag is for the current word. 
Unknown words can be treated as nouns or other parts of speech.

The tagger also recursively extracts as many nouns and noun phrases as it can, using a 
set of regular expressions. 

=head2 CLASS METHODS

=over 

=item initialize

Downloads some corpus data and saves it in a stored hash on the local filesystem.
This is called automatically the first time you run the tagger.

=cut


##########################
# 
# 	CLASS METHODS
#
########################## 
sub initialize {
	my ( $class ) = @_;
	$class->load_tags( 'tags.xml.gz' );
	$class->load_words( 'words.xml.gz' );
	$class->load_words( 'unknown.xml.gz' );
	store \%_words, $word_path;
	store \%_tags, $tag_path;
}



######################################################## 
# 	LOAD THE 2-GRAMS INTO A HASH FROM XML
########################################################
sub load_tags {

	my ( $class, $lexicon ) = @_;
	
	my $path = File::Spec->catfile( $lexpath, $lexicon );
	unless ( -f $path ){
		$class->_download_lexicon( $lexicon );
	}
	
	open FILE, "<:gzip", $path or die "couldn't open $path: $!";
	
	my $tag1;
	while ( <FILE> ){
		if( /<t1>([^<]+)<\/t1>/ ){
			$tag1 = $1;
		}
		if( /<t2 p="([^"]+)">([^<]+)<\/t2>/ ){
			$_tags{$tag1}{$2} = $1;
		}
	}
	close FILE;
}




######################################################## 
# 	LOAD THE WORD LEXICON INTO A HASH FROM XML
########################################################
sub load_words {
	
	my ( $class, $lexicon ) = @_;
	
	my $path = File::Spec->catfile( $lexpath, $lexicon );
	
	unless ( -f $path ){
		$class->_download_lexicon( $lexicon );
	}
	
	open FILE, "<:gzip", $path or die "Couldn't open $path: $!";
	my $word;
	while ( <FILE> ){
		if( /<w c="([^"]+)">([^<]+)<\/w>/ ){
			$word = $2;
			$_words{$word}{'count'} = $1;
		}
		if( /<t c="([^"]+)">([^<]+)<\/t>/ ){
			my $count = $1;
			my $count_ref = \$count;
			$_words{$word}{$2} = $count_ref;
		}
	}
	close FILE;	
}



######################################################## 
# 	DOWNLOAD THE SPECIFIED LEXICON
########################################################
sub _download_lexicon {
	my ( $class, $file ) = @_;
	
	my $path = File::Spec->catfile( $lexpath, $file );
	
	my $agent = LWP::UserAgent->new();
	$agent->agent("Web-O-Matic 2.1");

	my $response = $agent->get( $lexurl.$file );
	
	if ( $response->is_error ) {
		croak "Failed to download lexicon. $!";
	}
	
	open my $fh, "> $path" or die "Could not open $path to write: $!";
	print $fh $response->content();
	close $fh;
}


=head1 METHODS
	
=over 

=item new %PARAMS

Class constructor.  Takes a hash with the following parameters (shown with default
values):

=over 

=item unknown_word_tag => ''

Tag to assign to unknown words

=item debug => 0

Print some debugging info.

=item stem => 1

Stem single words using Lingua::Stem::EN

=item weight_noun_phrases => 1

When returning occurence counts for a noun phrase, multiply the value 
by the number of words in the NP.   

=item longest_noun_phrase => 100

Will ignore noun phrases longer than this threshold

=back

=cut
sub new {
	my ( $class, %params ) = @_;
	my $self = {  unknown_word_tag => '', 
				  debug => 0,
				  stem => 1,
				  weight_noun_phrases => 1,
				  longest_noun_phrase => 100,
				  %params };
	$mnp = _get_max_noun_regex();

	warn "entered POS constructor\n" if  $self->{'debug'};
	bless $self, $class;
	$self->reset();
	return $self;
}

	
=over 

=item get_words DOC or TEXT

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
	

	my $tagged = $self->add_tags( $text );
	my %keywords = $self->get_noun_phrases( $tagged );
	
	# Filter out any numeric junk
	foreach my $k ( keys %keywords ) {
		delete ( $keywords{$k} ) 
			if $k =~ /^[0-9\.]/o;
	}
	
	return %keywords;
}
	




=item get_readable TEXT

Return an easy-on-the-eyes tagged version of a text string.  Applies
L<add_tags> and reformats to be easier to read.

=cut		

sub get_readable {
	my ( $self, $raw_text ) = @_;
	my $text = $self->add_tags( $raw_text );
	$text =~ s/<[a-z]+>([^<]+)<\/([a-z]+)>/$1\/\U$2/go;
	return $text;
}	




=item add_tags TEXT

Examine the string provided and return it fully tagged ( XML style )

=cut

sub add_tags {
	
	my ( $self, $text ) = @_;

	my @text = $self->clean_text( $text );
	my ( @tags ) = map { $self->choose_tag( $_) } @text;
	$self->reset;
	return join ' ', @tags;
}	





=item strip_tags TEXT

Return a text string with the XML-style part-of-speech tags removed.

=cut

sub strip_tags {
	my ( $self, $text ) = @_;
	return unless defined $text;
			
	$text =~ s/<[^>]+>//gs;
	$text =~ s/\s+/ /gs;
	return lc($text);
}
		
		
=item clean_text TEXT

Strip the provided text of punctuation and HTML-style tags
in preparation for tagging

=cut

sub clean_text {

	my ( $self, $text ) = @_;
	
	# Convert entities to tags (Maciej)
	$text =~ s/&gt;/>/go;
	$text =~ s/&lt;/</go;
	$text =~ s/&quot;/"/go;
	$text =~ s/&amp;/&/go;
	
	$text =~ s/<[^>]+>/ /go;	# NAIVE tag stripper
	
	my ( @words ) = split(/\s+/, $text);
	
	my @tokenized;
	
	foreach ( @words ) {
		push @tokenized, $self->_split_punct($_);
	}
	
	my @last = ('') x 3;
	my @done;
	for (0 .. $#tokenized) {
		# current ends with period and has some word chars in it
		if ($tokenized[$_] =~ /^(.*\w.*)\.$/) {
			# leave attached if current token is in list of abbrev
			if ( $_words{$tokenized[$_]}{'count'}) {
				push @done, $tokenized[$_];
			}
			# split period off if next doesn't exist or is capitalized
			elsif (not defined $tokenized[$_+1] or $tokenized[$_+1] eq ucfirst($tokenized[$_+1])) {
				push (@done, $1, '.')
				# (this is a sentence boundary)
			}
			else {
				push @done, $tokenized[$_];
			}
		}
		else {
			push @done, $tokenized[$_];
		}
		
	}
	return @done;
}


=item _split_punct TEXT

Separate punctuation from words, where appropriate. This leaves trailing 
periods in place to be dealt with later. Called by L<clean_text>.

=cut

sub _split_punct {
	
	my ( $self, $term ) = @_;
	local $_ = $term;
	
	return ($_) unless /[^a-z ]/o;   # Short-circuit
	
	s/(?<=[^\s])--/ --/go;				# Separate dashes from leading text
	s/--(?=[^\s])/-- /go;				# Separate dashes from trailing text
	s/--/-/go;							# Shorten dashes
	s/[\(\[\{]/\( /go;					# Separate leading brackets
	s/[\)\]\}]/ \)/go;					# Separate trailing brackets
	s/\)(?=\W)/\) /go;					# Separate puctuation off trailing brackets
	s/(\W)\)/$1 \)/go;					# Separate puctuation off trailing brackets
	
	s/^"(?=\w)/`` /og;					# Convert left quotes to ``
	s/(?<!\s)"/ '' /og;					# Convert right quotes to ''
	s/('?')(?=\W)/$1 /og;				# Shift any punctuation off end of quotes
	s/(?<!\s)'(?!\w)/ '/og;				# Separate off single quotes
	s/^'(?=[a-zA-Z])/` /og;				# Convert left single quotes to ` and separate
	s/(`)(?=\w)/` /og;					# Shift left quotes ( ` ) off any word
	
	s/^([-]+)/$1 /og;							# Separate leading punctuation
	s/([:;,\?\!-%]+)(?=\s|$)/ $1/g;				# Separate trailing punctuation
	s/(?<!\s)\.\.\.+(?=\s|$)/ \.\.\./og;		# Separate off an ellipsis
	
	
	# Separate contractions
	s/([a-zA-Z])('[dms])(?=\s|$)/$1 $2/go;		# Separate off 'd 'm 's
	s/n't(?=\s|$)/ n't/go;						# Separate off n't	
	s/('[rvl][el])(?=\s|$)/ $1 /g;				# Separate off 've, 'll, 're
	
	return split;
}



=item choose_tag WORD

Select an appropriate tag for the word provided.  This subroutine is context-
sensitive - it remembers the immediately preceding word, and uses it 
to calculate the probabilities for various POS assignments for this word.

=cut

sub choose_tag {
	my ( $self,  $word) = @_;
	my $clean = $self->_clean_word( $word );
	$self->{'current_tag'} = $self->_assign_tag( $self->{'current_tag'}, $clean ) || 'nn';
	return "<".$self->{'current_tag'}.">$word</".$self->{'current_tag'}.">";
}

=item _assign_tag TAG, WORD ( memoized )

Given a preceding tag TAG, assign a tag to WORD.   Called by L<choose_tag>.

=cut 

my %seen; # memoize
sub _assign_tag {

	my ( $self, $prev_tag, $word) = @_;
	
	return $seen{$prev_tag}{$word} if exists( $seen{$prev_tag}{$word});
	
	# If the 'unknown_word_tag' value is defined, classify unknown words accordingly
	if ( $self->{'unknown_word_tag'} and $word =~ /^-.+-$/o ){
		$seen{$prev_tag}{$word} = $self->{'unknown_word_tag'};
		return $self->{'unknown_word_tag'};
	}
	
	my $max_prob = 0;
	
	my $w = $_words{$word};
	my $t = \%_tags;
	
	# TAG THE TEXT
	
	my $best_tag;
	
	foreach my $tag ( keys %{ $t->{$prev_tag} } ){
		
		next unless defined ${ $w->{$tag} };
		my $pw = ${ $w->{$tag} };
		
		# P =  P( $tag | $prev_tag ) * P( $tag | $word )
		my $probability =
			$t->{$prev_tag}{$tag} * ( $pw + 1 );
				
		# Set the tag with maximal probability
		if( $probability > $max_prob ) { 
			$max_prob = $probability;
			$best_tag = $tag;
		}
	}
	$seen{$prev_tag}{$word} = $best_tag;
	return $best_tag;
}
	

=item reset

This subroutine will reset the preceeding tag to a sentence ender ( PP ). 
This prepares the first word of a new sentence to be tagged correctly.

=cut

sub reset {
	my ( $self ) = @_;
	$self->{'current_tag'} = 'pp';
}
			  
=item _clean_word WORD

This subroutine determines whether a word should be considered in its 
lower or upper case form. This is useful in considering proper nouns
and words that begin sentences. Called by L<choose_tag>.

=cut

sub _clean_word {
	
	
	my ( $self, $word ) = @_;
	my $w = \%_words;
	my $lower_case = lc $word;
	
	
	if ($lower_case eq $word) {
		# already lowercase word
		
		if ( $w->{$word}{'count'} ) {
			# seen this lowercase word
			return $word;
		} else {
			# never seen this lowercase word. guess.
			my $unknown_word = $self->_classify_unknown_word( $word );
			return $unknown_word;
		}
	} else {
		# $word is a titlecase word
		$w->{$lower_case}{'count'} ||= 0;
		$w->{$word}{'count'} ||= 0;
		

		if( $w->{$lower_case}{'count'} > $w->{$word}{'count'} ){
			# seen lowercase more than the uc form, e.g. 'The', 'Against'
			return $lower_case;	
		} else {
			if ( $w->{$word}{'count'} ) {
				# seen this word, capitalized
				return $word; # don't use the lowercase version
			} else {
				# never seen any form of this word
				my $unknown_word = $self->_classify_unknown_word( $word );
				return $unknown_word;
			}
		}
	} 
}

=item _classify_unknown_word WORD

This changes any word not appearing in the lexicon to identifiable 
classes of words handled by a simple unknown word classification 
metric. Called by L<_clean_word>.

=cut

sub _classify_unknown_word {
	my ( $self, $word) = @_;
	local $_ = $word;
	

	if( m/[\(\{\[]/ ){
		$word = "*LRB*";
	
	} elsif( m/[\)\]\}]/o ){
		$word = "*RRB*";
	
	} elsif( m/^-?'?\.?[0-9][0-9,\.:]*'?s?$/o ){ # i.e. -1,339.32
		# look at how to match a float in the cookbook
		$word = "*NUM*";
	
	} elsif ( m/^-?[0-9]+\w+$/o ){	# i.e. 246th
		$word = "*ORD*";
	
	} elsif ( m/^[A-Z][A-Z\.-]*$/o ) { # i.e. AFL-CIO
		$word = "-abr-";
	
	} elsif ( $_ eq ucfirst ) {	# i.e. Richard, Lewis-Mann
		$word = "-cap-";
		
	} elsif (  m/ing$/o ) {
		$word = "-ing-";
	
	}  elsif( m/s$/o ) {
		$word = "-s-";
	
	} elsif ( m/tion$/o ){
		$word = "-tion-";
	
	} elsif ( m/ly$/o ){
		$word = "-ly-";
	
	} elsif ( m/\w-\w/o ){
		$word = "-hyp-";
	
	} elsif ( m/ed$/o ){
		$word = "-en-";
		
	} else {
		$word = "-unknown-";
	}
	
	return $word;
}
		



=item stem WORD ( memoized )

Returns the word stem as given by L<Lingua::Stem::EN>. This can be
turned off with the class parameter 'stem' => 0.

=cut

my %known_stems;

sub stem {
	my ( $self, $word ) = @_;
	return $word unless $self->{'stem'};
	return $known_stems{ $word } if exists $known_stems{$word};
	my $stemref = Lingua::Stem::En::stem( -words => [ $word ] );
	
	$known_stems{ $word } = $stemref->[0] if exists $stemref->[0];
}




=item _get_max_noun_regex

This returns a compiled regex for extracting maximal noun phrases from 
a POS-tagged text.

=cut
sub _get_max_noun_regex {
	my $regex =	qr/ 
					(?:$NUM)?(?:$GER|$ADJ|$PART)*	# optional number, gerund - adjective -participle
					(?:$NN)+				   		# Followed by one or more nouns
				
					(?:
						(?:$PREP)*(?:$DET)?(?:$CARD)? # Optional preposition, determinant, cardinal
						(?:$GER|$ADJ|$PART)*		# Optional gerund-adjective -participle
						(?:$NN)+					# one or more nouns
					)*
				/xo;		
	return $regex;
}


=item _get_sentence_regex

This returns a compiled regex for extracting individual sentences from 
a POS-tagged text. This is used by L<get_style_data>.

=cut
sub _get_sentence_regex {
	my $regex =  qr|<.*?				# Starting with a tag, find everything
					$SEN		#		# up to a sentence ender <pp>
					($PAREN)?	#		# including an optional right parenthesis
					($QUOT)?	#		# or quote
				|xo;
	return $regex;
}


=item _get_noun_regex

This returns a compiled regex for extracting single nouns from 
a POS-tagged text. This is used by L<get_nouns>.

=cut
sub _get_noun_regex {

	my $regex = qr/$NN/xo;
	return $regex;

}





=item get_style_data TAGGED_STRING

This subroutine extracts style data statistics for each sentence of the 
tagged string input. The return value is an array of hash references, where 
each array element corresponds to a sentence. Each hash reference contains 
the following data: 

=over
 
=item * $ref->{sentence}: The tagged sentence, itself
=item * $ref->{words}: The number of words in the sentence
=item * $ref->{phrases}: The average length of maximal noun phrases
=item * $ref->{nouns}: The frequency of nouns in the sentence
=item * $ref->{adj}: The frequency of adjectives in the sentence
=item * $ref->{prep}: The frequency of prepositions in the sentence
=item * $ref->{verbs}: The frequency of verbs in the sentence
=item * $ref->{adv}: The frequency of adverbs in the sentence

=back

=cut
sub get_style_data {

	my ( $self, $text ) = @_;
	my $regex = $self->_get_sentence_regex();
	my @text_data;
	$text =~ s/\s+/ /g;
	
	while( $text =~ m/($regex)/gs ){

		my $sentence = $1;
		my $data;
		$data->{sentence} = $sentence;
		$data->{words} = $data->{nouns} = 
			$data->{prep} = $data->{verbs} = 
				$data->{adj} = $data->{adv} = 
					$data->{phrases} = 0;
		
	
		# GET THE AVG NOUN PHRASE LENGTH
		my %phrases = $self->get_max_noun_phrases( $sentence );
		my ( $mnp, $phrase_words, $avg_noun_phrase ) = ( 0, 0, 0 );
	
		foreach( keys %phrases ){
			$mnp += $phrases{$_};	# ADD THE NUMBER OF TIMES THIS PHRASE APPEARS IN THE TEXT
			while( /\b/g ){
				$phrase_words += .5;	# COUNT UP THE NUMBER OF WORDS ( 2 BOUNDARIES FOR EACH WORD )
			}
		}
		$data->{phrases} = $phrase_words / $mnp if $mnp;
		
		# COUNT THE WORDS AND VARIOUS PARTS OF SPEECH
		my @tokens = split /\s/, $sentence;
		foreach( @tokens ){
			$data->{words}++ unless ( m|</pp.?>| or m|</[lr]rb>| );
			if( m|</nn[sp]*>| ){
				$data->{nouns}++;
			} elsif( m|</in>| ) {
				$data->{prep}++;
			} elsif( m|</vb.?>| ){
				$data->{verbs}++;
			} elsif( m|</jj[rs]*>| ){
				$data->{adj}++; 
			} elsif( m|</rb>| ){
				$data->{adv}++;
			}
		}
		
		
		# NORMALIZE AND ADD THIS REF TO THE TEXT-DATA ARRAY
		$data->{nouns} /= $data->{words};
		$data->{prep} /= $data->{words};
		$data->{verbs} /= $data->{words};
		$data->{adj} /= $data->{words};
		$data->{adv} /= $data->{words};
		push @text_data, $data; 
	}
	return @text_data;
}



=item get_nouns TAGGED_TEXT

Given a POS-tagged text, this method returns all nouns and their 
occurance frequencies.

=cut
sub get_nouns {
	my ( $self, $text ) = @_;
	
	my $noun_re = $self->_get_noun_regex();
	my @nouns = ( $text =~ /($noun_re)/gs );
	my @trimmed = map { $self->strip_tags( $_ ) } @nouns;
		
	my %return;
	foreach my $n ( @trimmed ) {
		
		$n =~ s/\s*$//;
		$n = $self->stem( $n );
		
		$return{$n}++ unless $n =~ /^\s*$/;
	
	}
	
	return %return;
}


=item get_max_noun_phrases TAGGED_TEXT

Given a POS-tagged text, this method returns only the maximal noun phrases.
May be called directly, but is also used by L<get_noun_phrases>

=cut
sub get_max_noun_phrases {
	my ( $self, $text ) = @_;
			
	my $mnp = $self->_get_max_noun_regex();
	my @noun_phrases = ( $text =~ /($mnp)/gs );
	my @trimmed = map { $self->strip_tags( $_ ) } @noun_phrases;
	
	my %return;
	foreach my $p ( @trimmed ) {
	
		$p =~ s/\s*$//;
		$p = $self->stem( $p )
			unless $p =~/\s/; # stem single words
		
		$return{$p}++ unless $p =~ /^\s*$/;
	
	}
	
	return %return;


}

=item get_noun_phrases TAGGED_TEXT

Similar to get_words, but requires a POS-tagged text as an argument.

=cut
sub get_noun_phrases {
	
	my ( $self, $text ) = @_;
	my $found;
	
	while(  $text =~ /($mnp)/go ){
		
		my $phrase = $1;
		$phrase =~ s/\s+/ /go;
		$found->{$phrase}++;
		
		#my $shortened = '';
		#if( $phrase =~ />\s*</o ){
			my $shortened = $phrase =~ /^(<.+>)\s*$WORD$/ || '';
			
		#}
		$found = $self->_get_sub_phrases( $shortened, $found );
		
		
		while( $phrase =~ s/^$WORD(<.+>)\s*$/$1/ ){
			if( $phrase =~ /($mnp)/ ){
				$phrase = $1;
				$found = $self->_get_sub_phrases( $phrase, $found );
			}
		}
	}
	
	my %return;
	foreach( keys %{ $found } ){
		my $k = $self->strip_tags( $_ );
		my $v = $found->{$_};
		
		$k =~ s/^\s+//o;
		$k =~ s/\s+$//o;
		
		# We weight by the word count to favor long noun phrases
		my @space_count = $k =~ /\s+/go; 
		my $word_count = scalar @space_count + 1;
		
		# Throttle MNPs if necessary
		next if $word_count > $self->{'longest_noun_phrase'};
		
		$k = $self->stem( $k ) unless $word_count > 1; # stem single words
		
		$return{$k} += ( $word_count * $v );
	}
	
	return %return;
}



=item _get_sub_phrases TAGGED_TEXT

Used by L<get_nouns>, this extracts the nested noun phrases from a 
maximal noun phrase.

=cut
sub _get_sub_phrases {

	my ( $self, $text, $found ) = @_;
	
	while( $text =~ m/($mnp)/gos ){
		my $phrase = $1;
		$found->{$phrase}++;
		
		my $shortened = '';
		if( $phrase =~ />\s*</o ){
			$phrase =~ /^\s*(<.+>)\s*$WORD$/;
			$shortened = $1;
		}
		$found = $self->_get_sub_phrases( $shortened, $found );
	}
	
	return $found;
}


1;
__END__



=head1 HISTORY

=over 

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
