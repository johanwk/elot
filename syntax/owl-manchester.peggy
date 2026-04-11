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
  / "{" WS IndividualList WS "}"
  / "(" OWS ClassExpression OWS ")"

// --- SubPropertyChain ---
SubPropertyChain
  = ObjectPropertyExpression (WS "o" WS ObjectPropertyExpression)+

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
ObjectPropertyExpression
  = "inverse" WS ObjectPropertyIRI
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
  / "{" WS LiteralList WS "}"
  / "(" OWS DataRange OWS ")"

// Faceted datatype restriction: Datatype [ facet value, ... ]
DatatypeRestriction
  = DatatypeIRI WS "[" OWS FacetRestriction (OWS "," OWS FacetRestriction)* OWS "]"

FacetRestriction
  = Facet WS Literal

Facet
  = "length" / "minLength" / "maxLength" / "pattern" / "langRange"
  / "<=" / "<" / ">=" / ">"

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
IndividualList = Individual (WS "," WS Individual)*
LiteralList    = Literal (WS "," WS Literal)*

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
// NOTE: The !KeywordToken negative lookahead below is NOT handled by
// peggy-to-peg.el; it is manually translated into (not ...) forms in
// the generated elot-owl-grammar.el.  See comments there.
// BareName = !KeywordToken LocalName
// KeywordToken = ("and"/"or"/"not"/"some"/"only"/"value"/"min"/"max"
//                /"exactly"/"Self"/"Thing"/"Nothing"/"inverse"/"o"
//                /"length"/"minLength"/"maxLength"/"pattern"/"langRange") ![a-zA-Z0-9_-]

Prefix    = [a-zA-Z0-9_-]+
LocalName = [a-zA-Z0-9_-]+

Literal
  = '"' [^"]* '"' ("^^" DatatypeIRI / "@" LanguageTag)?

LanguageTag = [a-zA-Z]+ ("-" [a-zA-Z0-9]+)*
Integer     = [0-9]+

// Whitespace (mandatory or optional depending on context)
WS "whitespace"
  = [ \t\n\r]+

OWS "optional whitespace"
  = [ \t\n\r]*
