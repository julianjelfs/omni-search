# OmniSearch text parser for Travel Search queries

### Introduction
Use this library to parse free text into a set of `SearchToken` 
items that can be used to initialise typical travel searches.

Search tokens are represented by the following union type:

```elm
type SearchToken
    = Adults Int
    | Rooms Int
    | ChildAges (List Int)
    | Date Date
    | From String
    | To String
    | Nights Int
    | Product ProductType
    | Other String
```

where `ProductType` is defined as 

```elm
type ProductType
    = Hotel
    | Flight
    | Transfer
    | Holiday
```

The `Other` type is used to capture any input that does not 
match any other parser so you can apply further processing to
those strings.

The syntax supported for each individual parser is as follows:

**Adults** - n adult(s)  
**Rooms** - n room(s)  
**Child Ages** - (n,n,n)  
**Date** - dd/mm or dd-mm or dd/mm/yyyy or dd-mm-yyyy or tomorrow  (date cannot be in the past)
**From** - anystring or "quoted string for multiple words"  
**To** - anystring or "quoted string for multiple words"  
**Nights** - n week(s) or n night(s) or n day(s)  
**Product** - hotel(s) or flight(s) or holiday(s) or package(s) or transfer(s) 

To parse text and receive a list of Search Tokens just call the 
`parse` function with the current date (required for relative and 
partial date parsing) and the input text. See the examples directory
for sample usage (cd into the examples directory and run `elm reactor`)
