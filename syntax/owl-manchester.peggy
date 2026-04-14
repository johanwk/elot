// --- Root Expressions ---
ClassExpression
  = OrExpression

// --- Class Expressions (Descriptions) ---
OrExpression
  = AndExpression (WS "or" WS AndExpression)*

AndExpression
  = PrimaryExpression (WS "and" WS PrimaryExpression)*

// "not" is recursive so that `not not ex:A` parses as not(not(A))
PrimaryExpression
  = "not" WS PrimaryExpression
  / AtomicExpression

AtomicExpression
  = Restriction
  / ClassIRI
  / "Thing"
  / "Nothing"
  / "{" OWS IndividualList OWS "}"
  / "(" OWS ClassExpression OWS ")"

// --- SubPropertyChain ---
// The "o" separator must not be part of a longer name, so we check !NameChar after it.
SubPropertyChain
  = ObjectPropertyExpression (WS "o" !NameChar WS ObjectPropertyExpression)+

// --- Restrictions ---
// Object property restrictions use class expressions as fillers;
// data property restrictions use data ranges as fillers.
// Since objectPropertyIRI and dataPropertyIRI are syntactically identical (both IRIs),
// we merge them into a single set of alternatives.  "value" tries Literal first
// (data property), then IndividualIRI (object property) via PEG ordered choice.
Restriction
  = ObjectPropertyExpression WS "some" WS PrimaryExpression
  / ObjectPropertyExpression WS "only" WS PrimaryExpression
  / ObjectPropertyExpression WS "value" WS Literal
  / ObjectPropertyExpression WS "value" WS IndividualIRI
  / ObjectPropertyExpression WS "min" WS Integer (WS PrimaryExpression)?
  / ObjectPropertyExpression WS "max" WS Integer (WS PrimaryExpression)?
  / ObjectPropertyExpression WS "exactly" WS Integer (WS PrimaryExpression)?
  / ObjectPropertyExpression WS "Self"

// --- Relational Expressions ---
// The OMN spec says: inverseObjectProperty ::= 'inverse' objectPropertyIRI
// but tools like Protégé and the OWL API also accept the parenthesized form:
//   inverse ( objectPropertyIRI )
// We support both.
ObjectPropertyExpression
  = "inverse" WS "(" OWS ObjectPropertyIRI OWS ")"
  / "inverse" WS ObjectPropertyIRI
  / ObjectPropertyIRI

// --- Data Ranges ---
// Full data range support including faceted restrictions (e.g. xsd:integer[>= 0, <= 100])
DataRange
  = DataConjunction (WS "or" WS DataConjunction)*

DataConjunction
  = DataPrimary (WS "and" WS DataPrimary)*

DataPrimary
  = "not" WS DataPrimary
  / DataAtomic

DataAtomic
  = DatatypeRestriction
  / DatatypeIRI
  / "{" OWS LiteralList OWS "}"
  / "(" OWS DataRange OWS ")"

// Faceted datatype restriction: Datatype [ facet value, ... ]
DatatypeRestriction
  = DatatypeIRI OWS "[" OWS FacetRestriction (OWS "," OWS FacetRestriction)* OWS "]"

FacetRestriction
  = Facet WS Literal

Facet
  = "minLength" / "maxLength" / "length" / "pattern" / "langRange"
  / "<=" / ">=" / "<" / ">"

// --- Fact (Individual frame assertion) ---
// fact ::= [ 'not' ] (objectPropertyFact | dataPropertyFact)
// objectPropertyFact ::= objectPropertyIRI individual
// dataPropertyFact   ::= dataPropertyIRI literal
// Since objectPropertyIRI and dataPropertyIRI are syntactically identical,
// we merge: try Literal first (data property fact), then Individual (object property fact).
Fact
  = "not" WS ObjectPropertyIRI WS (Literal / Individual)
  / ObjectPropertyIRI WS (Literal / Individual)

// --- Individual ---
// individual ::= individualIRI | nodeID
// NodeID tried first since _:foo also matches PrefixedName
Individual
  = NodeID / IndividualIRI

NodeID
  = "_:" LocalName

// --- Lists ---
ClassExpressionList          = ClassExpression (OWS "," OWS ClassExpression)*
// description2List: at least 2 class expressions (for misc DisjointClasses, EquivalentClasses)
ClassExpression2List         = ClassExpression (OWS "," OWS ClassExpression)+
ObjectPropertyExpressionList = ObjectPropertyExpression (OWS "," OWS ObjectPropertyExpression)*
// objectProperty2List / dataProperty2List: at least 2 (for misc DisjointProperties, EquivalentProperties)
ObjectPropertyExpression2List = ObjectPropertyExpression (OWS "," OWS ObjectPropertyExpression)+
IndividualList = Individual (OWS "," OWS Individual)*
// individual2List: at least 2 individuals (for misc SameIndividual, DifferentIndividuals)
Individual2List = Individual (OWS "," OWS Individual)+
LiteralList    = Literal (OWS "," OWS Literal)*

// --- Terminals & Identifiers ---
ClassIRI          = IRI
ObjectPropertyIRI = IRI
DatatypeIRI       = IRI
IndividualIRI     = IRI

IRI
  = FullIRI / PrefixedName / BareName

FullIRI
  = "<" [^>]+ ">"

PrefixedName
  = Prefix ":" LocalName
  / ":" LocalName

// --- Individual IRI list (for SameAs / DifferentFrom) ---
// In Elot org files, each SameAs/DifferentFrom entry is typically a single
// individual, but OMN allows comma-separated lists in annotated-list form.
IndividualIRIList = Individual (WS "," WS Individual)*

// BareName: an identifier that is NOT an OWL keyword.
// Uses negative lookahead (!) to reject keywords as bare names.
BareName = !KeywordBoundary LocalName

// KeywordBoundary: a keyword that is a complete token (not a prefix of a longer name).
// We check: keyword followed by NOT [a-zA-Z0-9_-].
KeywordBoundary
  = "and" !NameChar
  / "or" !NameChar
  / "not" !NameChar
  / "some" !NameChar
  / "only" !NameChar
  / "value" !NameChar
  / "min" !NameChar
  / "max" !NameChar
  / "exactly" !NameChar
  / "Self" !NameChar
  / "Thing" !NameChar
  / "Nothing" !NameChar
  / "inverse" !NameChar
  / "o" !NameChar
  / "minLength" !NameChar
  / "maxLength" !NameChar
  / "length" !NameChar
  / "pattern" !NameChar
  / "langRange" !NameChar

NameChar = [a-zA-Z0-9_-]

Prefix    = [a-zA-Z0-9_-]+
LocalName = [a-zA-Z_] [a-zA-Z0-9_-]*

Literal
  = '"' [^"]* '"' ("^^" DatatypeIRI / "@" LanguageTag)?
  / FloatingPointLiteral
  / DecimalLiteral
  / IntegerLiteral

LanguageTag = [a-zA-Z]+ ("-" [a-zA-Z0-9]+)*

// Numeric literals per OWL 2 Manchester Syntax §2.1
// Note: sign uses ("+" / "-") grouping, NOT ["+" / "-"] bracket notation,
// because peggy-to-peg.el treats [...] as a character class.
IntegerLiteral  = ("+" / "-")? Digits !("." / [eEfF])
DecimalLiteral  = ("+" / "-")? Digits "." Digits !([eEfF])
FloatingPointLiteral
  = ("+" / "-")? Digits ("." Digits)? [eE] ("+" / "-")? Digits [fF]?
  / ("+" / "-")? Digits ("." Digits)? [fF]

Digits  = [0-9]+
Integer = [0-9]+

// Whitespace (mandatory or optional depending on context)
WS "whitespace"
  = [ \t\n\r]+

OWS "optional whitespace"
  = [ \t\n\r]*
