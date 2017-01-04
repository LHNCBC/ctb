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

Begin initial expansion of termlist listing synonyms gleaned from UMLS.


#### Finding Expanded Term Combinations in UMLS

     for each subterm:
          get list of synonyms from UMLS
     recombine terms using ordered version of synonym lists

#### Finding Expanded term combinations not in UMLS

     for each subterm:
          get list of synonyms from UMLS
          get list of synonyms from LVG Acronym Expansions 
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


