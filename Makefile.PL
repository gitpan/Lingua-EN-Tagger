use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'		=> 'Lingua::EN::Tagger',
    'VERSION_FROM'	=> 'Tagger.pm', # finds $VERSION
    'PREREQ_PM'		=> { Lingua::Stem::En => '0.01', 
    					File::Spec => '0.01',
    					PerlIO::gzip => '0.01',
    					LWP::UserAgent => '0.01',
    					FileHandle => '0.01',
    					Storable => '0.01',
    					Carp => '0.01',
    					
    					}, # e.g., Module::Name => 1.1
);
