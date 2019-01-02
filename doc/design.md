# Design Document

# Purpose

Augmenting a list of terms with synonyms from a knowledge source to
generate a data set for use with MetaMap or MetaMapLite.

# The Process

+ Install Knowledge Sources
+ Supply Term List
+ Select Knowledge Sources
+ Apply Knowledge Sources to Term List
+ Filter synonyms
+ Generate Data Set


# implementation: Web Application 

## URL Routes

### Home page (/)

Termlist submission form: function generates form for user to submit termlist.

### Process Termlist (/processtermlist/)

Each seed term is decomposed into its component subterms.  A list of
synonyms for each subterm is generated using the UMLS (or other
knowledge source).  Candidate terms are generated using synonyms
gleaned from the ordered set of subterm synonym sublists.  The UMLS is
then searched using the candidate terms and any concepts containing
the candidate term are to the seed term's synset along with the
concept's synonyms grouped by concept id.


If a seed term cannot be found in the UMLS then LVG (Lexical Variant
Generator) can be used using the same method to generate a set of
candidate synonyms grouped by a synthetic concept id.


Begin initial expansion of termlist listing synonyms gleaned from UMLS.

1. Clean up termlist supplied by form if necessary
2. generate term to concept id map of terms that are present in the UMLS.
3. generate set of conceptids found using termlist
4. 


#### Finding Expanded Term Combinations in UMLS

     for each subterm:
          get list of synonyms from UMLS
     recombine terms using ordered version of synonym lists

#### Finding Expanded term combinations not in UMLS

     for each subterm:
          get list of synonyms from UMLS
          get list of synonyms from LVG Acronym Expansions (or Fruitful Variants)
		  Keep intersection of both lists
     recombine terms using ordered version of synonym list intersections


returns form containing terms with expansions collapsed (user
expandable), collapsed terms are selectable.

### Process Filtered Termlist (/processfiltertermlis/)


### Load Knowledge Sources

#### Load UMLS Knowledge Source



# Documentation Design

## Audience

## Organization

Should this be a tutorial?


