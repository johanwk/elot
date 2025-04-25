
# Table of Contents

1.  [About this file](#org5f49e1f)
2.  [bfo.owl](#org0b0e509)
    1.  [Prefixes](#org50c4adb)
    2.  [bfo.owl ontology (obo:bfo.owl http://purl.obolibrary.org/obo/bfo/2020/bfo-core.ttl)](#bfo.owl-ontology-declaration)
    3.  [Datatypes](#bfo.owl-datatypes)
        1.  [rdf:langString](#rdf:langString)
        2.  [xsd:string](#xsd:string)
    4.  [Classes](#bfo.owl-class-hierarchy)
        1.  [Disjointness clauses](#org2a778e9):nodeclare:
        2.  [&ldquo;entity&rdquo;@en (obo:BFO\_0000001)](#obo:BFO_0000001)
            1.  [&ldquo;continuant&rdquo;@en (obo:BFO\_0000002)](#obo:BFO_0000002)
                1.  [&ldquo;generically dependent continuant&rdquo;@en (obo:BFO\_0000031)](#obo:BFO_0000031)
                2.  [&ldquo;independent continuant&rdquo;@en (obo:BFO\_0000004)](#obo:BFO_0000004)
                    1.  [&ldquo;immaterial entity&rdquo;@en (obo:BFO\_0000141)](#obo:BFO_0000141)
                        1.  [&ldquo;continuant fiat boundary&rdquo;@en (obo:BFO\_0000140)](#obo:BFO_0000140)
                            1.  [&ldquo;fiat line&rdquo;@en (obo:BFO\_0000142)](#obo:BFO_0000142)
                            2.  [&ldquo;fiat point&rdquo;@en (obo:BFO\_0000147)](#obo:BFO_0000147)
                            3.  [&ldquo;fiat surface&rdquo;@en (obo:BFO\_0000146)](#obo:BFO_0000146)
                        2.  [&ldquo;site&rdquo;@en (obo:BFO\_0000029)](#obo:BFO_0000029)
                        3.  [&ldquo;spatial region&rdquo;@en (obo:BFO\_0000006)](#obo:BFO_0000006)
                            1.  [&ldquo;one-dimensional spatial region&rdquo;@en (obo:BFO\_0000026)](#obo:BFO_0000026)
                            2.  [&ldquo;three-dimensional spatial region&rdquo;@en (obo:BFO\_0000028)](#obo:BFO_0000028)
                            3.  [&ldquo;two-dimensional spatial region&rdquo;@en (obo:BFO\_0000009)](#obo:BFO_0000009)
                            4.  [&ldquo;zero-dimensional spatial region&rdquo;@en (obo:BFO\_0000018)](#obo:BFO_0000018)
                    2.  [&ldquo;material entity&rdquo;@en (obo:BFO\_0000040)](#obo:BFO_0000040)
                        1.  [&ldquo;fiat object part&rdquo;@en (obo:BFO\_0000024)](#obo:BFO_0000024)
                        2.  [&ldquo;object&rdquo;@en (obo:BFO\_0000030)](#obo:BFO_0000030)
                        3.  [&ldquo;object aggregate&rdquo;@en (obo:BFO\_0000027)](#obo:BFO_0000027)
                3.  [&ldquo;specifically dependent continuant&rdquo;@en (obo:BFO\_0000020)](#obo:BFO_0000020)
                    1.  [&ldquo;quality&rdquo;@en (obo:BFO\_0000019)](#obo:BFO_0000019)
                        1.  [&ldquo;relational quality&rdquo;@en (obo:BFO\_0000145)](#obo:BFO_0000145)
                    2.  [&ldquo;realizable entity&rdquo;@en (obo:BFO\_0000017)](#obo:BFO_0000017)
                        1.  [&ldquo;disposition&rdquo;@en (obo:BFO\_0000016)](#obo:BFO_0000016)
                            1.  [&ldquo;function&rdquo;@en (obo:BFO\_0000034)](#obo:BFO_0000034)
                        2.  [&ldquo;role&rdquo;@en (obo:BFO\_0000023)](#obo:BFO_0000023)
            2.  [&ldquo;occurrent&rdquo;@en (obo:BFO\_0000003)](#obo:BFO_0000003)
                1.  [&ldquo;process&rdquo;@en (obo:BFO\_0000015)](#obo:BFO_0000015)
                    1.  [&ldquo;history&rdquo;@en (obo:BFO\_0000182)](#obo:BFO_0000182)
                2.  [&ldquo;process boundary&rdquo;@en (obo:BFO\_0000035)](#obo:BFO_0000035)
                3.  [&ldquo;spatiotemporal region&rdquo;@en (obo:BFO\_0000011)](#obo:BFO_0000011)
                4.  [&ldquo;temporal region&rdquo;@en (obo:BFO\_0000008)](#obo:BFO_0000008)
                    1.  [&ldquo;one-dimensional temporal region&rdquo;@en (obo:BFO\_0000038)](#obo:BFO_0000038)
                        1.  [&ldquo;temporal interval&rdquo;@en (obo:BFO\_0000202)](#obo:BFO_0000202)
                    2.  [&ldquo;zero-dimensional temporal region&rdquo;@en (obo:BFO\_0000148)](#obo:BFO_0000148)
                        1.  [&ldquo;temporal instant&rdquo;@en (obo:BFO\_0000203)](#obo:BFO_0000203)
    5.  [Object properties](#bfo.owl-object-property-hierarchy)
        1.  [&ldquo;concretizes&rdquo;@en (obo:BFO\_0000059)](#obo:BFO_0000059)
        2.  [&ldquo;continuant part of&rdquo;@en (obo:BFO\_0000176)](#obo:BFO_0000176)
            1.  [&ldquo;member part of&rdquo;@en (obo:BFO\_0000129)](#obo:BFO_0000129)
        3.  [&ldquo;environs&rdquo;@en (obo:BFO\_0000183)](#obo:BFO_0000183)
        4.  [&ldquo;exists at&rdquo;@en (obo:BFO\_0000108)](#obo:BFO_0000108)
        5.  [&ldquo;first instant of&rdquo;@en (obo:BFO\_0000221)](#obo:BFO_0000221)
        6.  [&ldquo;generically depends on&rdquo;@en (obo:BFO\_0000084)](#obo:BFO_0000084)
        7.  [&ldquo;has continuant part&rdquo;@en (obo:BFO\_0000178)](#obo:BFO_0000178)
            1.  [&ldquo;has member part&rdquo;@en (obo:BFO\_0000115)](#obo:BFO_0000115)
        8.  [&ldquo;has first instant&rdquo;@en (obo:BFO\_0000222)](#obo:BFO_0000222)
        9.  [&ldquo;has history&rdquo;@en (obo:BFO\_0000185)](#obo:BFO_0000185)
        10. [&ldquo;has last instant&rdquo;@en (obo:BFO\_0000224)](#obo:BFO_0000224)
        11. [&ldquo;has material basis&rdquo;@en (obo:BFO\_0000218)](#obo:BFO_0000218)
        12. [&ldquo;has occurrent part&rdquo;@en (obo:BFO\_0000117)](#obo:BFO_0000117)
            1.  [&ldquo;has temporal part&rdquo;@en (obo:BFO\_0000121)](#obo:BFO_0000121)
        13. [&ldquo;has participant&rdquo;@en (obo:BFO\_0000057)](#obo:BFO_0000057)
        14. [&ldquo;has realization&rdquo;@en (obo:BFO\_0000054)](#obo:BFO_0000054)
        15. [&ldquo;history of&rdquo;@en (obo:BFO\_0000184)](#obo:BFO_0000184)
        16. [&ldquo;is carrier of&rdquo;@en (obo:BFO\_0000101)](#obo:BFO_0000101)
        17. [&ldquo;is concretized by&rdquo;@en (obo:BFO\_0000058)](#obo:BFO_0000058)
        18. [&ldquo;last instant of&rdquo;@en (obo:BFO\_0000223)](#obo:BFO_0000223)
        19. [&ldquo;located in&rdquo;@en (obo:BFO\_0000171)](#obo:BFO_0000171)
        20. [&ldquo;location of&rdquo;@en (obo:BFO\_0000124)](#obo:BFO_0000124)
        21. [&ldquo;material basis of&rdquo;@en (obo:BFO\_0000127)](#obo:BFO_0000127)
        22. [&ldquo;occupies spatial region&rdquo;@en (obo:BFO\_0000210)](#obo:BFO_0000210)
        23. [&ldquo;occupies spatiotemporal region&rdquo;@en (obo:BFO\_0000200)](#obo:BFO_0000200)
        24. [&ldquo;occupies temporal region&rdquo;@en (obo:BFO\_0000199)](#obo:BFO_0000199)
        25. [&ldquo;occurrent part of&rdquo;@en (obo:BFO\_0000132)](#obo:BFO_0000132)
            1.  [&ldquo;temporal part of&rdquo;@en (obo:BFO\_0000139)](#obo:BFO_0000139)
        26. [&ldquo;occurs in&rdquo;@en (obo:BFO\_0000066)](#obo:BFO_0000066)
        27. [&ldquo;participates in&rdquo;@en (obo:BFO\_0000056)](#obo:BFO_0000056)
        28. [&ldquo;preceded by&rdquo;@en (obo:BFO\_0000062)](#obo:BFO_0000062)
        29. [&ldquo;precedes&rdquo;@en (obo:BFO\_0000063)](#obo:BFO_0000063)
        30. [&ldquo;realizes&rdquo;@en (obo:BFO\_0000055)](#obo:BFO_0000055)
        31. [&ldquo;spatially projects onto&rdquo;@en (obo:BFO\_0000216)](#obo:BFO_0000216)
        32. [&ldquo;specifically depended on by&rdquo;@en (obo:BFO\_0000194)](#obo:BFO_0000194)
            1.  [&ldquo;bearer of&rdquo;@en (obo:BFO\_0000196)](#obo:BFO_0000196)
        33. [&ldquo;specifically depends on&rdquo;@en (obo:BFO\_0000195)](#obo:BFO_0000195)
            1.  [&ldquo;inheres in&rdquo;@en (obo:BFO\_0000197)](#obo:BFO_0000197)
        34. [&ldquo;temporally projects onto&rdquo;@en (obo:BFO\_0000153)](#obo:BFO_0000153)
    6.  [Data properties](#bfo.owl-data-property-hierarchy)
    7.  [Annotation properties](#bfo.owl-annotation-property-hierarchy)
        1.  [skos:altLabel](#skos:altLabel)
        2.  [rdfs:comment](#rdfs:comment)
        3.  [dc11:contributor](#dc11:contributor)
        4.  [skos:definition](#skos:definition)
        5.  [dc:description](#dc:description)
        6.  [skos:example](#skos:example)
        7.  [dc11:identifier](#dc11:identifier)
        8.  [rdfs:label](#rdfs:label)
        9.  [dc11:license](#dc11:license)
        10. [dc:license](#dc:license)
        11. [skos:prefLabel](#skos:prefLabel)
        12. [skos:scopeNote](#skos:scopeNote)
        13. [dc:title](#dc:title)
    8.  [Individuals](#bfo.owl-individuals)




<a id="org5f49e1f"></a>

# About this file

This ELOT example was created by from the BFO-2020 ontology release of
2024-01-29, file *bfo-core.ttl*, downloaded from
<https://github.com/BFO-ontology/BFO-2020/releases/tag/release-2024-01-29>.
The file was opened with the ELOT function `elot-open-owl`.

One change was made to the source file before converting to an
org-mode document: The prefix `obo:` was applied to identifier (for
instance, from `<http://purl.obolibrary.org/obo/BFO_0000030>` for the
class &rsquo;object&rsquo; to just `obo::BFO_0000030`).

In the `:PROPERTIES:` org-mode &ldquo;drawer&rdquo; below, below the top headline,
some important changes have been made after conversion.

-   the `:tangle` target was changed to `bfo-core.omn`. By default, ELOT
    will use the *localname* of an ontology to determine the filename of
    an output ontology file. Because the localname of BFO-2020 is
    `bfo.owl`, this will produce `bfo.owl.omn` (Manchester Syntax) and
    `bfo.owl.ttl` (Turtle), which is not intuitive!
-   The `:ELOT-default-prefix:` has been set to `obo`.


<a id="org0b0e509"></a>

# bfo.owl



<a id="org50c4adb"></a>

## Prefixes

The ontology document in OWL employs the namespace prefixes of table [1](#org999422a).

<table id="org999422a" border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">
<caption class="t-above"><span class="table-number">Table 1:</span> OWL ontology prefixes</caption>

<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">prefix</th>
<th scope="col" class="org-left">uri</th>
</tr>
</thead>
<tbody>
<tr>
<td class="org-left">:</td>
<td class="org-left"><a href="http://purl.obolibrary.org/obo/bfo.owl">http://purl.obolibrary.org/obo/bfo.owl</a>#</td>
</tr>

<tr>
<td class="org-left">dc11:</td>
<td class="org-left"><a href="http://purl.org/dc/elements/1.1/">http://purl.org/dc/elements/1.1/</a></td>
</tr>

<tr>
<td class="org-left">dc:</td>
<td class="org-left"><a href="http://purl.org/dc/terms/">http://purl.org/dc/terms/</a></td>
</tr>

<tr>
<td class="org-left">obo:</td>
<td class="org-left"><a href="http://purl.obolibrary.org/obo/">http://purl.obolibrary.org/obo/</a></td>
</tr>

<tr>
<td class="org-left">owl:</td>
<td class="org-left"><a href="http://www.w3.org/2002/07/owl">http://www.w3.org/2002/07/owl</a>#</td>
</tr>

<tr>
<td class="org-left">rdf:</td>
<td class="org-left"><a href="http://www.w3.org/1999/02/22-rdf-syntax-ns">http://www.w3.org/1999/02/22-rdf-syntax-ns</a>#</td>
</tr>

<tr>
<td class="org-left">rdfs:</td>
<td class="org-left"><a href="http://www.w3.org/2000/01/rdf-schema">http://www.w3.org/2000/01/rdf-schema</a>#</td>
</tr>

<tr>
<td class="org-left">skos:</td>
<td class="org-left"><a href="http://www.w3.org/2004/02/skos/core">http://www.w3.org/2004/02/skos/core</a>#</td>
</tr>

<tr>
<td class="org-left">xml:</td>
<td class="org-left"><a href="http://www.w3.org/XML/1998/namespace">http://www.w3.org/XML/1998/namespace</a></td>
</tr>

<tr>
<td class="org-left">xsd:</td>
<td class="org-left"><a href="http://www.w3.org/2001/XMLSchema">http://www.w3.org/2001/XMLSchema</a>#</td>
</tr>
</tbody>
</table>


<a id="bfo.owl-ontology-declaration"></a>

## bfo.owl ontology (obo:bfo.owl <http://purl.obolibrary.org/obo/bfo/2020/bfo-core.ttl>)

-   **[dc11:contributor](#dc11:contributor):** Alan Ruttenberg
-   **[dc11:contributor](#dc11:contributor):** Albert Goldfain
-   **[dc11:contributor](#dc11:contributor):** Barry Smith
-   **[dc11:contributor](#dc11:contributor):** Bill Duncan
-   **[dc11:contributor](#dc11:contributor):** Bjoern Peters
-   **[dc11:contributor](#dc11:contributor):** Chris Mungall
-   **[dc11:contributor](#dc11:contributor):** David Osumi-Sutherland
-   **[dc11:contributor](#dc11:contributor):** Fabian Neuhaus
-   **[dc11:contributor](#dc11:contributor):** James A. Overton
-   **[dc11:contributor](#dc11:contributor):** Janna Hastings
-   **[dc11:contributor](#dc11:contributor):** Jie Zheng
-   **[dc11:contributor](#dc11:contributor):** John Beverley
-   **[dc11:contributor](#dc11:contributor):** Jonathan Bona
-   **[dc11:contributor](#dc11:contributor):** Larry Hunter
-   **[dc11:contributor](#dc11:contributor):** Leonard Jacuzzo
-   **[dc11:contributor](#dc11:contributor):** Ludger Jansen
-   **[dc11:contributor](#dc11:contributor):** Mark Jensen
-   **[dc11:contributor](#dc11:contributor):** Mark Ressler
-   **[dc11:contributor](#dc11:contributor):** Mathias Brochhausen
-   **[dc11:contributor](#dc11:contributor):** Mauricio Almeida
-   **[dc11:contributor](#dc11:contributor):** Melanie Courtot
-   **[dc11:contributor](#dc11:contributor):** Neil Otte
-   **[dc11:contributor](#dc11:contributor):** Pierre Grenon
-   **[dc11:contributor](#dc11:contributor):** Randall Dipert
-   **[dc11:contributor](#dc11:contributor):** Robert Rovetto
-   **[dc11:contributor](#dc11:contributor):** Ron Rudnicki
-   **[dc11:contributor](#dc11:contributor):** Stefan Schulz
-   **[dc11:contributor](#dc11:contributor):** Thomas Bittner
-   **[dc11:contributor](#dc11:contributor):** Werner Ceusters
-   **[dc11:contributor](#dc11:contributor):** Yongqun &ldquo;Oliver&rdquo; He
-   **[dc:description](#dc:description):** &ldquo;Basic Formal Ontology implemented in the Web Ontology Language (OWL 2) with direct semantics.&rdquo;@en
-   **[dc:license](#dc:license):** <https://creativecommons.org/licenses/by/4.0/>
-   **[dc:title](#dc:title):** BFO 2020
-   **[rdfs:comment](#rdfs:comment):** The most recent version of this file will always be in the GitHub repository <https://github.com/bfo-ontology/bfo-2020>


<a id="bfo.owl-datatypes"></a>

## Datatypes


<a id="rdf:langString"></a>

### rdf:langString


<a id="xsd:string"></a>

### xsd:string


<a id="bfo.owl-class-hierarchy"></a>

## Classes


<a id="org2a778e9"></a>

### Disjointness clauses     :nodeclare:

    DisjointClasses:
        obo:BFO_0000004, obo:BFO_0000020, obo:BFO_0000031
    DisjointClasses:
        obo:BFO_0000142, obo:BFO_0000146, obo:BFO_0000147
    DisjointClasses:
        obo:BFO_0000008, obo:BFO_0000011, obo:BFO_0000015, obo:BFO_0000035
    DisjointClasses:
        obo:BFO_0000009, obo:BFO_0000018, obo:BFO_0000026, obo:BFO_0000028
    DisjointClasses:
        obo:BFO_0000006, obo:BFO_0000029, obo:BFO_0000140


<a id="obo:BFO_0000001"></a>

### &ldquo;entity&rdquo;@en (obo:BFO\_0000001)

-   **[dc11:identifier](#dc11:identifier):** 001-BFO
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) An entity is anything that exists or has existed or will exist&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;Julius Caesar; the Second World War; your body mass index; Verdi&rsquo;s Requiem&rdquo;@en


<a id="obo:BFO_0000002"></a>

#### &ldquo;continuant&rdquo;@en (obo:BFO\_0000002)

-   **[dc11:identifier](#dc11:identifier):** 008-BFO
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A continuant is an entity that persists, endures, or continues to exist through time while maintaining its identity&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;A human being; a tennis ball; a cave; a region of space; someone&rsquo;s temperature&rdquo;@en
-   **SubClassOf:** [continuant part of](#obo:BFO_0000176) only [continuant](#obo:BFO_0000002)
-   **DisjointWith:** [occurrent](#obo:BFO_0000003)


<a id="obo:BFO_0000031"></a>

##### &ldquo;generically dependent continuant&rdquo;@en (obo:BFO\_0000031)

-   **[dc11:identifier](#dc11:identifier):** 074-BFO
-   **[skos:altLabel](#skos:altLabel):** &ldquo;g-dependent continuant&rdquo;@en
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A generically dependent continuant is an entity that exists in virtue of the fact that there is at least one of what may be multiple copies which is the content or the pattern that multiple copies would share&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;The pdf file on your laptop; the pdf file that is a copy thereof on my laptop; the sequence of this protein molecule; the sequence that is a copy thereof in that protein molecule; the content that is shared by a string of dots and dashes written on a page and the transmitted Morse code signal; the content of a sentence; an engineering blueprint&rdquo;@en
-   **DisjointWith:** [independent continuant](#obo:BFO_0000004), [specifically dependent continuant](#obo:BFO_0000020)


<a id="obo:BFO_0000004"></a>

##### &ldquo;independent continuant&rdquo;@en (obo:BFO\_0000004)

-   **[dc11:identifier](#dc11:identifier):** 017-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b is an independent continuant =Def b is a continuant & there is no c such that b specifically depends on c or b generically depends on c&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;An atom; a molecule; an organism; a heart; a chair; the bottom right portion of a human torso; a leg; the interior of your mouth; a spatial region; an orchestra&rdquo;@en
-   **SubClassOf:** [continuant part of](#obo:BFO_0000176) only [independent continuant](#obo:BFO_0000004)
-   **DisjointWith:** [specifically dependent continuant](#obo:BFO_0000020), [generically dependent continuant](#obo:BFO_0000031)


<a id="obo:BFO_0000141"></a>

###### &ldquo;immaterial entity&rdquo;@en (obo:BFO\_0000141)

-   **[dc11:identifier](#dc11:identifier):** 028-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b is an immaterial entity =Def b is an independent continuant which is such that there is no time t when it has a material entity as continuant part&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;As for fiat point, fiat line, fiat surface, site&rdquo;@en
-   **DisjointWith:** [material entity](#obo:BFO_0000040)

1.  &ldquo;continuant fiat boundary&rdquo;@en (obo:BFO\_0000140)

    -   **[dc11:identifier](#dc11:identifier):** 029-BFO
    -   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A continuant fiat boundary b is an immaterial entity that is of zero, one or two dimensions & such that there is no time t when b has a spatial region as continuant part & whose location is determined in relation to some material entity&rdquo;@en
    -   **[skos:example](#skos:example):** &ldquo;As for fiat point, fiat line, fiat surface&rdquo;@en
    -   **SubClassOf:** [location of](#obo:BFO_0000124) only [continuant fiat boundary](#obo:BFO_0000140)
    -   **SubClassOf:** [has continuant part](#obo:BFO_0000178) only [continuant fiat boundary](#obo:BFO_0000140)
    -   **DisjointWith:** [spatial region](#obo:BFO_0000006), [site](#obo:BFO_0000029)
    
    1.  &ldquo;fiat line&rdquo;@en (obo:BFO\_0000142)
    
        -   **[dc11:identifier](#dc11:identifier):** 032-BFO
        -   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A fiat line is a one-dimensional continuant fiat boundary that is continuous&rdquo;@en
        -   **[skos:example](#skos:example):** &ldquo;The Equator; all geopolitical boundaries; all lines of latitude and longitude; the median sulcus of your tongue; the line separating the outer surface of the mucosa of the lower lip from the outer surface of the skin of the chin&rdquo;@en
        -   **SubClassOf:** [has continuant part](#obo:BFO_0000178) only 
            ([fiat line](#obo:BFO_0000142) or [fiat point](#obo:BFO_0000147))
        -   **DisjointWith:** [fiat surface](#obo:BFO_0000146), [fiat point](#obo:BFO_0000147)
    
    2.  &ldquo;fiat point&rdquo;@en (obo:BFO\_0000147)
    
        -   **[dc11:identifier](#dc11:identifier):** 031-BFO
        -   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A fiat point is a zero-dimensional continuant fiat boundary that consists of a single point&rdquo;@en
        -   **[skos:example](#skos:example):** &ldquo;The geographic North Pole; the quadripoint where the boundaries of Colorado, Utah, New Mexico and Arizona meet; the point of origin of some spatial coordinate system&rdquo;@en
        -   **SubClassOf:** [has continuant part](#obo:BFO_0000178) only [fiat point](#obo:BFO_0000147)
        -   **DisjointWith:** [fiat line](#obo:BFO_0000142), [fiat surface](#obo:BFO_0000146)
    
    3.  &ldquo;fiat surface&rdquo;@en (obo:BFO\_0000146)
    
        -   **[dc11:identifier](#dc11:identifier):** 033-BFO
        -   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A fiat surface is a two-dimensional continuant fiat boundary that is self-connected&rdquo;@en
        -   **[skos:example](#skos:example):** &ldquo;The surface of the Earth; the plane separating the smoking from the non-smoking zone in a restaurant&rdquo;@en
        -   **SubClassOf:** [has continuant part](#obo:BFO_0000178) only [continuant fiat boundary](#obo:BFO_0000140)
        -   **DisjointWith:** [fiat line](#obo:BFO_0000142), [fiat point](#obo:BFO_0000147)

2.  &ldquo;site&rdquo;@en (obo:BFO\_0000029)

    -   **[dc11:identifier](#dc11:identifier):** 034-BFO
    -   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A site is a three-dimensional immaterial entity whose boundaries either (partially or wholly) coincide with the boundaries of one or more material entities or have locations determined in relation to some material entity&rdquo;@en
    -   **[skos:example](#skos:example):** &ldquo;A hole in a portion of cheese; a rabbit hole; the Grand Canyon; the Piazza San Marco; the kangaroo-joey-containing hole of a kangaroo pouch; your left nostril (a fiat part - the opening - of your left nasal cavity); the lumen of your gut; the hold of a ship; the interior of the trunk of your car; hole in an engineered floor joist&rdquo;@en
    -   **SubClassOf:** [continuant part of](#obo:BFO_0000176) only 
        ([site](#obo:BFO_0000029) or [material entity](#obo:BFO_0000040))
    -   **SubClassOf:** [has continuant part](#obo:BFO_0000178) only 
        ([site](#obo:BFO_0000029) or [continuant fiat boundary](#obo:BFO_0000140))
    -   **SubClassOf:** [occupies spatial region](#obo:BFO_0000210) only [three-dimensional spatial region](#obo:BFO_0000028)
    -   **DisjointWith:** [spatial region](#obo:BFO_0000006), [continuant fiat boundary](#obo:BFO_0000140)

3.  &ldquo;spatial region&rdquo;@en (obo:BFO\_0000006)

    -   **[dc11:identifier](#dc11:identifier):** 035-BFO
    -   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A spatial region is a continuant entity that is a continuant part of the spatial projection of a portion of spacetime at a given time&rdquo;@en
    -   **[skos:example](#skos:example):** &ldquo;As for zero-dimensional spatial region, one-dimensional spatial region, two-dimensional spatial region, three-dimensional spatial region&rdquo;@en
    -   **SubClassOf:** [continuant part of](#obo:BFO_0000176) only [spatial region](#obo:BFO_0000006)
    -   **DisjointWith:** [site](#obo:BFO_0000029), [continuant fiat boundary](#obo:BFO_0000140)
    
    1.  &ldquo;one-dimensional spatial region&rdquo;@en (obo:BFO\_0000026)
    
        -   **[dc11:identifier](#dc11:identifier):** 038-BFO
        -   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A one-dimensional spatial region is a whole consisting of a line together with zero or more lines which may have points as parts&rdquo;@en
        -   **[skos:example](#skos:example):** &ldquo;An edge of a cube-shaped portion of space; a line connecting two points; two parallel lines extended in space&rdquo;@en
        -   **SubClassOf:** [has continuant part](#obo:BFO_0000178) only 
            ([zero-dimensional spatial region](#obo:BFO_0000018) or [one-dimensional spatial region](#obo:BFO_0000026))
        -   **DisjointWith:** [two-dimensional spatial region](#obo:BFO_0000009), [zero-dimensional spatial region](#obo:BFO_0000018), [three-dimensional spatial region](#obo:BFO_0000028)
    
    2.  &ldquo;three-dimensional spatial region&rdquo;@en (obo:BFO\_0000028)
    
        -   **[dc11:identifier](#dc11:identifier):** 040-BFO
        -   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A three-dimensional spatial region is a whole consisting of a spatial volume together with zero or more spatial volumes which may have spatial regions of lower dimension as parts&rdquo;@en
        -   **[skos:example](#skos:example):** &ldquo;A cube-shaped region of space; a sphere-shaped region of space; the region of space occupied by all and only the planets in the solar system at some point in time&rdquo;@en
        -   **SubClassOf:** [has continuant part](#obo:BFO_0000178) only [spatial region](#obo:BFO_0000006)
        -   **DisjointWith:** [two-dimensional spatial region](#obo:BFO_0000009), [zero-dimensional spatial region](#obo:BFO_0000018), [one-dimensional spatial region](#obo:BFO_0000026)
    
    3.  &ldquo;two-dimensional spatial region&rdquo;@en (obo:BFO\_0000009)
    
        -   **[dc11:identifier](#dc11:identifier):** 039-BFO
        -   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A two-dimensional spatial region is a spatial region that is a whole consisting of a surface together with zero or more surfaces which may have spatial regions of lower dimension as parts&rdquo;@en
        -   **[skos:example](#skos:example):** &ldquo;The surface of a sphere-shaped part of space; an infinitely thin plane in space&rdquo;@en
        -   **SubClassOf:** [has continuant part](#obo:BFO_0000178) only 
            ([two-dimensional spatial region](#obo:BFO_0000009) or [zero-dimensional spatial region](#obo:BFO_0000018) or [one-dimensional spatial region](#obo:BFO_0000026))
        -   **DisjointWith:** [zero-dimensional spatial region](#obo:BFO_0000018), [one-dimensional spatial region](#obo:BFO_0000026), [three-dimensional spatial region](#obo:BFO_0000028)
    
    4.  &ldquo;zero-dimensional spatial region&rdquo;@en (obo:BFO\_0000018)
    
        -   **[dc11:identifier](#dc11:identifier):** 037-BFO
        -   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A zero-dimensional spatial region is one or a collection of more than one spatially disjoint points in space&rdquo;@en
        -   **[skos:example](#skos:example):** &ldquo;The spatial region occupied at some time instant by the North Pole&rdquo;@en
        -   **SubClassOf:** [has continuant part](#obo:BFO_0000178) only [zero-dimensional spatial region](#obo:BFO_0000018)
        -   **DisjointWith:** [two-dimensional spatial region](#obo:BFO_0000009), [one-dimensional spatial region](#obo:BFO_0000026), [three-dimensional spatial region](#obo:BFO_0000028)


<a id="obo:BFO_0000040"></a>

###### &ldquo;material entity&rdquo;@en (obo:BFO\_0000040)

-   **[dc11:identifier](#dc11:identifier):** 019-BFO
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A material entity is an independent continuant has some portion of matter as continuant part&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;A human being; the undetached arm of a human being; an aggregate of human beings&rdquo;@en
-   **SubClassOf:** [continuant part of](#obo:BFO_0000176) only [material entity](#obo:BFO_0000040)
-   **SubClassOf:** [has continuant part](#obo:BFO_0000178) only 
    ([site](#obo:BFO_0000029) or [material entity](#obo:BFO_0000040) or [continuant fiat boundary](#obo:BFO_0000140))
-   **DisjointWith:** [immaterial entity](#obo:BFO_0000141)

1.  &ldquo;fiat object part&rdquo;@en (obo:BFO\_0000024)

    -   **[dc11:identifier](#dc11:identifier):** 027-BFO
    -   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A fiat object part b is a material entity & such that if b exists then it is continuant part of some object c & demarcated from the remainder of c by one or more fiat surfaces&rdquo;@en
    -   **[skos:example](#skos:example):** &ldquo;The upper and lower lobes of the left lung; the dorsal and ventral surfaces of the body; the Western hemisphere of the Earth; the FMA:regional parts of an intact human body&rdquo;@en

2.  &ldquo;object&rdquo;@en (obo:BFO\_0000030)

    -   **[dc11:identifier](#dc11:identifier):** 024-BFO
    -   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) An object is a material entity which manifests causal unity & is of a type instances of which are maximal relative to the sort of causal unity manifested&rdquo;@en
    -   **[skos:example](#skos:example):** &ldquo;An organism; a fish tank; a planet; a laptop; a valve; a block of marble; an ice cube&rdquo;@en
    -   **[skos:scopeNote](#skos:scopeNote):** &ldquo;A description of three primary sorts of causal unity is provided in Basic Formal Ontology 2.0. Specification and User Guide&rdquo;@en

3.  &ldquo;object aggregate&rdquo;@en (obo:BFO\_0000027)

    -   **[dc11:identifier](#dc11:identifier):** 025-BFO
    -   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) An object aggregate is a material entity consisting exactly of a plurality (≥1) of objects as member parts which together form a unit&rdquo;@en
    -   **[skos:example](#skos:example):** &ldquo;The aggregate of the musicians in a symphony orchestra and their instruments; the aggregate of bearings in a constant velocity axle joint; the nitrogen atoms in the atmosphere; a collection of cells in a blood biobank&rdquo;@en
    -   **[skos:scopeNote](#skos:scopeNote):** The unit can, at certain times, consist of exactly one object, for example, when a wolf litter loses all but one of its pups, but it must at some time have a plurality of member parts.
    -   **[skos:scopeNote](#skos:scopeNote):** &rsquo;Exactly&rsquo; means that there are no parts of the object aggregate other than its member parts.


<a id="obo:BFO_0000020"></a>

##### &ldquo;specifically dependent continuant&rdquo;@en (obo:BFO\_0000020)

-   **[dc11:identifier](#dc11:identifier):** 050-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b is a specifically dependent continuant =Def b is a continuant & there is some independent continuant c which is not a spatial region & which is such that b specifically depends on c&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;(with multiple bearers) John&rsquo;s love for Mary; the ownership relation between John and this statue; the relation of authority between John and his subordinates&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;(with one bearer) The mass of this tomato; the pink colour of a medium rare piece of grilled filet mignon at its centre; the smell of this portion of mozzarella; the disposition of this fish to decay; the role of being a doctor; the function of this heart to pump blood; the shape of this hole&rdquo;@en
-   **DisjointWith:** [independent continuant](#obo:BFO_0000004), [generically dependent continuant](#obo:BFO_0000031)


<a id="obo:BFO_0000019"></a>

###### &ldquo;quality&rdquo;@en (obo:BFO\_0000019)

-   **[dc11:identifier](#dc11:identifier):** 055-BFO
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A quality is a specifically dependent continuant that, in contrast to roles and dispositions, does not require any further process in order to be realized&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;The colour of a tomato; the ambient temperature of this portion of air; the length of the circumference of your waist; the shape of your nose; the shape of your nostril; the mass of this piece of gold&rdquo;@en
-   **DisjointWith:** [realizable entity](#obo:BFO_0000017)

1.  &ldquo;relational quality&rdquo;@en (obo:BFO\_0000145)

    -   **[dc11:identifier](#dc11:identifier):** 057-BFO
    -   **[skos:definition](#skos:definition):** &ldquo;b is a relational quality =Def b is a quality & there exists c and d such that c and d are not identical & b specifically depends on c & b specifically depends on d&rdquo;@en
    -   **[skos:example](#skos:example):** &ldquo;A marriage bond; an instance of love; an obligation between one person and another&rdquo;@en


<a id="obo:BFO_0000017"></a>

###### &ldquo;realizable entity&rdquo;@en (obo:BFO\_0000017)

-   **[dc11:identifier](#dc11:identifier):** 058-BFO
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A realizable entity is a specifically dependent continuant that inheres in some independent continuant which is not a spatial region & which is of a type some instances of which are realized in processes of a correlated type&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;The role of being a doctor; the role of this boundary to delineate where Utah and Colorado meet; the function of your reproductive organs; the disposition of your blood to coagulate; the disposition of this piece of metal to conduct electricity&rdquo;@en
-   **DisjointWith:** [quality](#obo:BFO_0000019)

1.  &ldquo;disposition&rdquo;@en (obo:BFO\_0000016)

    -   **[dc11:identifier](#dc11:identifier):** 062-BFO
    -   **[skos:altLabel](#skos:altLabel):** &ldquo;internally-grounded realizable entity&rdquo;@en
    -   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A disposition b is a realizable entity such that if b ceases to exist then its bearer is physically changed & b&rsquo;s realization occurs when and because this bearer is in some special physical circumstances & this realization occurs in virtue of the bearer&rsquo;s physical make-up&rdquo;@en
    -   **[skos:example](#skos:example):** &ldquo;An atom of element X has the disposition to decay to an atom of element Y; the cell wall is disposed to transport cellular material through endocytosis and exocytosis; certain people have a predisposition to colon cancer; children are innately disposed to categorize objects in certain ways&rdquo;@en
    -   **DisjointWith:** [role](#obo:BFO_0000023)
    
    1.  &ldquo;function&rdquo;@en (obo:BFO\_0000034)
    
        -   **[dc11:identifier](#dc11:identifier):** 064-BFO
        -   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A function is a disposition that exists in virtue of its bearer&rsquo;s physical make-up & this physical make-up is something the bearer possesses because it came into being either through evolution (in the case of natural biological entities) or through intentional design (in the case of artefacts) in order to realize processes of a certain sort&rdquo;@en
        -   **[skos:example](#skos:example):** &ldquo;The function of a hammer to drive in nails; the function of a heart pacemaker to regulate the beating of a heart through electricity&rdquo;@en

2.  &ldquo;role&rdquo;@en (obo:BFO\_0000023)

    -   **[dc11:identifier](#dc11:identifier):** 061-BFO
    -   **[skos:altLabel](#skos:altLabel):** &ldquo;externally-grounded realizable entity&rdquo;@en
    -   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A role b is a realizable entity such that b exists because there is some single bearer that is in some special physical, social, or institutional set of circumstances in which this bearer does not have to be & b is not such that, if it ceases to exist, then the physical make-up of the bearer is thereby changed&rdquo;@en
    -   **[skos:example](#skos:example):** &ldquo;The priest role; the student role; the role of subject in a clinical trial; the role of a stone in marking a property boundary; the role of a boundary to demarcate two neighbouring administrative territories; the role of a building in serving as a military target&rdquo;@en
    -   **DisjointWith:** [disposition](#obo:BFO_0000016)


<a id="obo:BFO_0000003"></a>

#### &ldquo;occurrent&rdquo;@en (obo:BFO\_0000003)

-   **[dc11:identifier](#dc11:identifier):** 077-BFO
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) An occurrent is an entity that unfolds itself in time or it is the start or end of such an entity or it is a temporal or spatiotemporal region&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;As for process, history, process boundary, spatiotemporal region, zero-dimensional temporal region, one-dimensional temporal region, temporal interval, temporal instant.&rdquo;@en
-   **DisjointWith:** [continuant](#obo:BFO_0000002)


<a id="obo:BFO_0000015"></a>

##### &ldquo;process&rdquo;@en (obo:BFO\_0000015)

-   **[dc11:identifier](#dc11:identifier):** 083-BFO
-   **[skos:altLabel](#skos:altLabel):** &ldquo;event&rdquo;@en
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) p is a process means p is an occurrent that has some temporal proper part and for some time t, p has some material entity as participant&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;An act of selling; the life of an organism; a process of sleeping; a process of cell-division; a beating of the heart; a process of meiosis; the taxiing of an aircraft; the programming of a computer&rdquo;@en
-   **SubClassOf:** [has occurrent part](#obo:BFO_0000117) only 
    ([process](#obo:BFO_0000015) or [process boundary](#obo:BFO_0000035))
-   **SubClassOf:** [occurrent part of](#obo:BFO_0000132) only [process](#obo:BFO_0000015)
-   **SubClassOf:** [temporal part of](#obo:BFO_0000139) only [process](#obo:BFO_0000015)
-   **DisjointWith:** [temporal region](#obo:BFO_0000008), [spatiotemporal region](#obo:BFO_0000011), [process boundary](#obo:BFO_0000035)


<a id="obo:BFO_0000182"></a>

###### &ldquo;history&rdquo;@en (obo:BFO\_0000182)

-   **[dc11:identifier](#dc11:identifier):** 138-BFO
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A history is a process that is the sum of the totality of processes taking place in the spatiotemporal region occupied by the material part of a material entity&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;The life of an organism from the beginning to the end of its existence&rdquo;@en


<a id="obo:BFO_0000035"></a>

##### &ldquo;process boundary&rdquo;@en (obo:BFO\_0000035)

-   **[dc11:identifier](#dc11:identifier):** 084-BFO
-   **[skos:definition](#skos:definition):** &ldquo;p is a process boundary =Def p is a temporal part of a process & p has no proper temporal parts&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;The boundary between the 2nd and 3rd year of your life&rdquo;@en
-   **SubClassOf:** [has occurrent part](#obo:BFO_0000117) only [process boundary](#obo:BFO_0000035)
-   **SubClassOf:** [has temporal part](#obo:BFO_0000121) only [process boundary](#obo:BFO_0000035)
-   **SubClassOf:** [occurrent part of](#obo:BFO_0000132) only 
    ([process](#obo:BFO_0000015) or [process boundary](#obo:BFO_0000035))
-   **SubClassOf:** [temporal part of](#obo:BFO_0000139) only 
    ([process](#obo:BFO_0000015) or [process boundary](#obo:BFO_0000035))
-   **DisjointWith:** [temporal region](#obo:BFO_0000008), [spatiotemporal region](#obo:BFO_0000011), [process](#obo:BFO_0000015)


<a id="obo:BFO_0000011"></a>

##### &ldquo;spatiotemporal region&rdquo;@en (obo:BFO\_0000011)

-   **[dc11:identifier](#dc11:identifier):** 095-BFO
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A spatiotemporal region is an occurrent that is an occurrent part of spacetime&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;The spatiotemporal region occupied by the development of a cancer tumour; the spatiotemporal region occupied by an orbiting satellite&rdquo;@en
-   **[skos:scopeNote](#skos:scopeNote):** &ldquo;&lsquo;Spacetime&rsquo; here refers to the maximal instance of the universal spatiotemporal region.&rdquo;@en
-   **SubClassOf:** [occurrent part of](#obo:BFO_0000132) only [spatiotemporal region](#obo:BFO_0000011)
-   **SubClassOf:** [temporal part of](#obo:BFO_0000139) only [spatiotemporal region](#obo:BFO_0000011)
-   **DisjointWith:** [temporal region](#obo:BFO_0000008), [process](#obo:BFO_0000015), [process boundary](#obo:BFO_0000035)


<a id="obo:BFO_0000008"></a>

##### &ldquo;temporal region&rdquo;@en (obo:BFO\_0000008)

-   **[dc11:identifier](#dc11:identifier):** 100-BFO
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A temporal region is an occurrent over which processes can unfold&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;As for zero-dimensional temporal region and one-dimensional temporal region&rdquo;@en
-   **SubClassOf:** [occurrent part of](#obo:BFO_0000132) only [temporal region](#obo:BFO_0000008)
-   **SubClassOf:** [temporal part of](#obo:BFO_0000139) only [temporal region](#obo:BFO_0000008)
-   **DisjointWith:** [spatiotemporal region](#obo:BFO_0000011), [process](#obo:BFO_0000015), [process boundary](#obo:BFO_0000035)


<a id="obo:BFO_0000038"></a>

###### &ldquo;one-dimensional temporal region&rdquo;@en (obo:BFO\_0000038)

-   **[dc11:identifier](#dc11:identifier):** 103-BFO
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A one-dimensional temporal region is a temporal region that is a whole that has a temporal interval and zero or more temporal intervals and temporal instants as parts&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;The temporal region during which a process occurs&rdquo;@en
-   **SubClassOf:** [has temporal part](#obo:BFO_0000121) only 
    ([one-dimensional temporal region](#obo:BFO_0000038) or [zero-dimensional temporal region](#obo:BFO_0000148))
-   **SubClassOf:** [temporal part of](#obo:BFO_0000139) only [one-dimensional temporal region](#obo:BFO_0000038)
-   **DisjointWith:** [zero-dimensional temporal region](#obo:BFO_0000148)

1.  &ldquo;temporal interval&rdquo;@en (obo:BFO\_0000202)

    -   **[dc11:identifier](#dc11:identifier):** 155-BFO
    -   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A temporal interval is a one-dimensional temporal region that is continuous, thus without gaps or breaks&rdquo;@en
    -   **[skos:example](#skos:example):** &ldquo;The year 2018.&rdquo;@en
    -   **[skos:scopeNote](#skos:scopeNote):** &ldquo;A one-dimensional temporal region can include as parts not only temporal intervals but also temporal instants separated from other parts by gaps.&rdquo;@en


<a id="obo:BFO_0000148"></a>

###### &ldquo;zero-dimensional temporal region&rdquo;@en (obo:BFO\_0000148)

-   **[dc11:identifier](#dc11:identifier):** 102-BFO
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A zero-dimensional temporal region is a temporal region that is a whole consisting of one or more separated temporal instants as parts&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;A temporal region that is occupied by a process boundary; the moment at which a finger is detached in an industrial accident&rdquo;@en
-   **SubClassOf:** [has temporal part](#obo:BFO_0000121) only [zero-dimensional temporal region](#obo:BFO_0000148)
-   **DisjointWith:** [one-dimensional temporal region](#obo:BFO_0000038)

1.  &ldquo;temporal instant&rdquo;@en (obo:BFO\_0000203)

    -   **[dc11:identifier](#dc11:identifier):** 209-BFO
    -   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) A temporal instant is a zero-dimensional temporal region that has no proper temporal part&rdquo;@en
    -   **[skos:example](#skos:example):** &ldquo;The millennium&rdquo;@en


<a id="bfo.owl-object-property-hierarchy"></a>

## Object properties


<a id="obo:BFO_0000059"></a>

### &ldquo;concretizes&rdquo;@en (obo:BFO\_0000059)

-   **[dc11:identifier](#dc11:identifier):** 256-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b concretizes c =Def b is a process or a specifically dependent continuant & c is a generically dependent continuant & there is some time t such that c is the pattern or content which b shares at t with actual or potential copies&rdquo;@en
-   **[skos:scopeNote](#skos:scopeNote):** &ldquo;Users that require more sophisticated representations of time are encouraged to import a temporal extension of BFO-Core provided by the BFO development team. See documentation for guidance: <https://github.com/BFO-ontology/BFO-2020/tree/master/src/owl/profiles/temporal%20extensions>&rdquo;@en
-   **Domain:** [process](#obo:BFO_0000015) or [specifically dependent continuant](#obo:BFO_0000020)
-   **Range:** [generically dependent continuant](#obo:BFO_0000031)
-   **InverseOf:** [is concretized by](#obo:BFO_0000058)


<a id="obo:BFO_0000176"></a>

### &ldquo;continuant part of&rdquo;@en (obo:BFO\_0000176)

-   **[dc11:identifier](#dc11:identifier):** 221-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b continuant part of c =Def b and c are continuants & there is some time t such that b and c exist at t & b continuant part of c at t&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;Milk teeth continuant part of human; surgically removed tumour continuant part of organism&rdquo;@en
-   **[skos:scopeNote](#skos:scopeNote):** &ldquo;Users that require more sophisticated representations of time are encouraged to import a temporal extension of BFO-Core provided by the BFO development team. See documentation for guidance: <https://github.com/BFO-ontology/BFO-2020/tree/master/src/owl/profiles/temporal%20extensions>&rdquo;@en
-   **Domain:** [continuant](#obo:BFO_0000002)
-   **Range:** [continuant](#obo:BFO_0000002)
-   **InverseOf:** [has continuant part](#obo:BFO_0000178)


<a id="obo:BFO_0000129"></a>

#### &ldquo;member part of&rdquo;@en (obo:BFO\_0000129)

-   **[dc11:identifier](#dc11:identifier):** 228-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b member part of c `Def b is an object & c is a material entity & there is some time t such that b continuant part of c at t & there is a mutually exhaustive and pairwise disjoint partition of c into objects x1, ..., xn (for some n ≠ 1) with b = xi (for some 1 <` i <= n)&rdquo;@en
-   **[skos:scopeNote](#skos:scopeNote):** &ldquo;Users that require more sophisticated representations of time are encouraged to import a temporal extension of BFO-Core provided by the BFO development team. See documentation for guidance: <https://github.com/BFO-ontology/BFO-2020/tree/master/src/owl/profiles/temporal%20extensions>&rdquo;@en
-   **SubPropertyOf:** [continuant part of](#obo:BFO_0000176)
-   **Domain:** [material entity](#obo:BFO_0000040)
-   **Range:** [material entity](#obo:BFO_0000040)
-   **InverseOf:** [has member part](#obo:BFO_0000115)


<a id="obo:BFO_0000183"></a>

### &ldquo;environs&rdquo;@en (obo:BFO\_0000183)

-   **[dc11:identifier](#dc11:identifier):** 267-BFO
-   **[skos:altLabel](#skos:altLabel):** &ldquo;contains process&rdquo;@en
-   **[skos:definition](#skos:definition):** &ldquo;b environs c =Def c occurs in b&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;Mouth environs process of mastication; city environs traffic&rdquo;@en
-   **Domain:** [site](#obo:BFO_0000029) or [material entity](#obo:BFO_0000040)
-   **Range:** [process](#obo:BFO_0000015) or [process boundary](#obo:BFO_0000035)
-   **InverseOf:** [occurs in](#obo:BFO_0000066)


<a id="obo:BFO_0000108"></a>

### &ldquo;exists at&rdquo;@en (obo:BFO\_0000108)

-   **[dc11:identifier](#dc11:identifier):** 118-BFO
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) exists at is a relation between a particular and some temporal region at which the particular exists&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;First World War exists at 1914-1916; Mexico exists at January 1, 2000&rdquo;@en
-   **Domain:** [entity](#obo:BFO_0000001)
-   **Range:** [temporal region](#obo:BFO_0000008)


<a id="obo:BFO_0000221"></a>

### &ldquo;first instant of&rdquo;@en (obo:BFO\_0000221)

-   **[dc11:identifier](#dc11:identifier):** 268-BFO
-   **[skos:definition](#skos:definition):** &ldquo;t first instant of t&rsquo; =Def t is a temporal instant & t&rsquo; is a temporal region t&rsquo; & t precedes all temporal parts of t&rsquo; other than t&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;An hour starting at midnight yesterday has first instant midnight yesterday&rdquo;@en
-   **Domain:** [temporal instant](#obo:BFO_0000203)
-   **Range:** [temporal region](#obo:BFO_0000008)
-   **InverseOf:** [has first instant](#obo:BFO_0000222)


<a id="obo:BFO_0000084"></a>

### &ldquo;generically depends on&rdquo;@en (obo:BFO\_0000084)

-   **[dc11:identifier](#dc11:identifier):** 252-BFO
-   **[skos:altLabel](#skos:altLabel):** &ldquo;g-depends on&rdquo;@en
-   **[skos:definition](#skos:definition):** &ldquo;b generically depends on c =Def b is a generically dependent continuant & c is an independent continuant that is not a spatial region & at some time t there inheres in c a specifically dependent continuant which concretizes b at t&rdquo;@en
-   **[skos:scopeNote](#skos:scopeNote):** &ldquo;Users that require more sophisticated representations of time are encouraged to import a temporal extension of BFO-Core provided by the BFO development team. See documentation for guidance: <https://github.com/BFO-ontology/BFO-2020/tree/master/src/owl/profiles/temporal%20extensions>&rdquo;@en
-   **Domain:** [generically dependent continuant](#obo:BFO_0000031)
-   **Range:** [independent continuant](#obo:BFO_0000004)
    and (not ([spatial region](#obo:BFO_0000006)))
-   **InverseOf:** [is carrier of](#obo:BFO_0000101)


<a id="obo:BFO_0000178"></a>

### &ldquo;has continuant part&rdquo;@en (obo:BFO\_0000178)

-   **[dc11:identifier](#dc11:identifier):** 271-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b has continuant part c =Def c continuant part of b&rdquo;@en
-   **[skos:scopeNote](#skos:scopeNote):** &ldquo;Users that require more sophisticated representations of time are encouraged to import a temporal extension of BFO-Core provided by the BFO development team. See documentation for guidance: <https://github.com/BFO-ontology/BFO-2020/tree/master/src/owl/profiles/temporal%20extensions>&rdquo;@en
-   **Domain:** [continuant](#obo:BFO_0000002)
-   **Range:** [continuant](#obo:BFO_0000002)
-   **InverseOf:** [continuant part of](#obo:BFO_0000176)


<a id="obo:BFO_0000115"></a>

#### &ldquo;has member part&rdquo;@en (obo:BFO\_0000115)

-   **[dc11:identifier](#dc11:identifier):** 230-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b has member part c =Def c member part of b&rdquo;@en
-   **[skos:scopeNote](#skos:scopeNote):** &ldquo;Users that require more sophisticated representations of time are encouraged to import a temporal extension of BFO-Core provided by the BFO development team. See documentation for guidance: <https://github.com/BFO-ontology/BFO-2020/tree/master/src/owl/profiles/temporal%20extensions>&rdquo;@en
-   **SubPropertyOf:** [has continuant part](#obo:BFO_0000178)
-   **Domain:** [material entity](#obo:BFO_0000040)
-   **Range:** [material entity](#obo:BFO_0000040)
-   **InverseOf:** [member part of](#obo:BFO_0000129)


<a id="obo:BFO_0000222"></a>

### &ldquo;has first instant&rdquo;@en (obo:BFO\_0000222)

-   **[dc11:identifier](#dc11:identifier):** 261-BFO
-   **[skos:definition](#skos:definition):** &ldquo;t has first instant t&rsquo; =Def t&rsquo; first instant of t&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;The first hour of a year has first instant midnight on December 31&rdquo;@en
-   **Domain:** [temporal region](#obo:BFO_0000008)
-   **Range:** [temporal instant](#obo:BFO_0000203)
-   **InverseOf:** [first instant of](#obo:BFO_0000221)
-   **Characteristics:** Functional


<a id="obo:BFO_0000185"></a>

### &ldquo;has history&rdquo;@en (obo:BFO\_0000185)

-   **[dc11:identifier](#dc11:identifier):** 145-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b has history c =Def c history of b&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;This organism has history this life&rdquo;@en
-   **Domain:** [material entity](#obo:BFO_0000040)
-   **Range:** [history](#obo:BFO_0000182)
-   **InverseOf:** [history of](#obo:BFO_0000184)


<a id="obo:BFO_0000224"></a>

### &ldquo;has last instant&rdquo;@en (obo:BFO\_0000224)

-   **[dc11:identifier](#dc11:identifier):** 215-BFO
-   **[skos:definition](#skos:definition):** &ldquo;t has last instant t&rsquo; =Def t&rsquo; last instant of t&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;The last hour of a year has last instant midnight December 31&rdquo;@en
-   **Domain:** [temporal region](#obo:BFO_0000008)
-   **Range:** [temporal instant](#obo:BFO_0000203)
-   **InverseOf:** [last instant of](#obo:BFO_0000223)
-   **Characteristics:** Functional


<a id="obo:BFO_0000218"></a>

### &ldquo;has material basis&rdquo;@en (obo:BFO\_0000218)

-   **[dc11:identifier](#dc11:identifier):** 242-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b has material basis c =Def b is a disposition & c is a material entity & there is some d bearer of b & there is some time t such that c is a continuant part of d at t & d has disposition b because c is a continuant part of d at t&rdquo;@en
-   **[skos:scopeNote](#skos:scopeNote):** &ldquo;Users that require more sophisticated representations of time are encouraged to import a temporal extension of BFO-Core provided by the BFO development team. See documentation for guidance: <https://github.com/BFO-ontology/BFO-2020/tree/master/src/owl/profiles/temporal%20extensions>&rdquo;@en
-   **Domain:** [disposition](#obo:BFO_0000016)
-   **Range:** [material entity](#obo:BFO_0000040)
-   **InverseOf:** [material basis of](#obo:BFO_0000127)


<a id="obo:BFO_0000117"></a>

### &ldquo;has occurrent part&rdquo;@en (obo:BFO\_0000117)

-   **[dc11:identifier](#dc11:identifier):** 202-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b has occurrent part c =Def c occurrent part of b&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;Mary&rsquo;s life has occurrent part Mary&rsquo;s 5th birthday&rdquo;@en
-   **Domain:** [occurrent](#obo:BFO_0000003)
-   **Range:** [occurrent](#obo:BFO_0000003)
-   **InverseOf:** [occurrent part of](#obo:BFO_0000132)
-   **Characteristics:** Transitive


<a id="obo:BFO_0000121"></a>

#### &ldquo;has temporal part&rdquo;@en (obo:BFO\_0000121)

-   **[dc11:identifier](#dc11:identifier):** 211-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b has temporal part c =Def c temporal part of b&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;Your life has temporal part the first year of your life&rdquo;@en
-   **SubPropertyOf:** [has occurrent part](#obo:BFO_0000117)
-   **Domain:** [occurrent](#obo:BFO_0000003)
-   **Range:** [occurrent](#obo:BFO_0000003)
-   **InverseOf:** [temporal part of](#obo:BFO_0000139)
-   **Characteristics:** Transitive


<a id="obo:BFO_0000057"></a>

### &ldquo;has participant&rdquo;@en (obo:BFO\_0000057)

-   **[dc11:identifier](#dc11:identifier):** 248-BFO
-   **[skos:definition](#skos:definition):** &ldquo;p has participant c =Def c participates in p&rdquo;@en
-   **[skos:scopeNote](#skos:scopeNote):** &ldquo;Users that require more sophisticated representations of time are encouraged to import a temporal extension of BFO-Core provided by the BFO development team. See documentation for guidance: <https://github.com/BFO-ontology/BFO-2020/tree/master/src/owl/profiles/temporal%20extensions>&rdquo;@en
-   **Domain:** [process](#obo:BFO_0000015)
-   **Range:** [specifically dependent continuant](#obo:BFO_0000020) or [generically dependent continuant](#obo:BFO_0000031) or ([independent continuant](#obo:BFO_0000004)
    and (not ([spatial region](#obo:BFO_0000006))))
-   **InverseOf:** [participates in](#obo:BFO_0000056)


<a id="obo:BFO_0000054"></a>

### &ldquo;has realization&rdquo;@en (obo:BFO\_0000054)

-   **[dc11:identifier](#dc11:identifier):** 206-BFO
-   **[skos:altLabel](#skos:altLabel):** &ldquo;realized in&rdquo;@en
-   **[skos:definition](#skos:definition):** &ldquo;b has realization c =Def c realizes b&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;As for realizes&rdquo;@en
-   **Domain:** [realizable entity](#obo:BFO_0000017)
-   **Range:** [process](#obo:BFO_0000015)
-   **InverseOf:** [realizes](#obo:BFO_0000055)


<a id="obo:BFO_0000184"></a>

### &ldquo;history of&rdquo;@en (obo:BFO\_0000184)

-   **[dc11:identifier](#dc11:identifier):** 144-BFO
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) history of is a relation between history b and material entity c such that b is the unique history of c&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;This life is the history of this organism&rdquo;@en
-   **Domain:** [history](#obo:BFO_0000182)
-   **Range:** [material entity](#obo:BFO_0000040)
-   **InverseOf:** [has history](#obo:BFO_0000185)
-   **Characteristics:** Functional, InverseFunctional


<a id="obo:BFO_0000101"></a>

### &ldquo;is carrier of&rdquo;@en (obo:BFO\_0000101)

-   **[dc11:identifier](#dc11:identifier):** 254-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b is carrier of c =Def there is some time t such that c generically depends on b at t&rdquo;@en
-   **[skos:scopeNote](#skos:scopeNote):** &ldquo;Users that require more sophisticated representations of time are encouraged to import a temporal extension of BFO-Core provided by the BFO development team. See documentation for guidance: <https://github.com/BFO-ontology/BFO-2020/tree/master/src/owl/profiles/temporal%20extensions>&rdquo;@en
-   **Domain:** [independent continuant](#obo:BFO_0000004)
    and (not ([spatial region](#obo:BFO_0000006)))
-   **Range:** [generically dependent continuant](#obo:BFO_0000031)
-   **InverseOf:** [generically depends on](#obo:BFO_0000084)


<a id="obo:BFO_0000058"></a>

### &ldquo;is concretized by&rdquo;@en (obo:BFO\_0000058)

-   **[dc11:identifier](#dc11:identifier):** 258-BFO
-   **[skos:definition](#skos:definition):** &ldquo;c is concretized by b =Def b concretizes c&rdquo;@en
-   **[skos:scopeNote](#skos:scopeNote):** &ldquo;Users that require more sophisticated representations of time are encouraged to import a temporal extension of BFO-Core provided by the BFO development team. See documentation for guidance: <https://github.com/BFO-ontology/BFO-2020/tree/master/src/owl/profiles/temporal%20extensions>&rdquo;@en
-   **Domain:** [generically dependent continuant](#obo:BFO_0000031)
-   **Range:** [process](#obo:BFO_0000015) or [specifically dependent continuant](#obo:BFO_0000020)
-   **InverseOf:** [concretizes](#obo:BFO_0000059)


<a id="obo:BFO_0000223"></a>

### &ldquo;last instant of&rdquo;@en (obo:BFO\_0000223)

-   **[dc11:identifier](#dc11:identifier):** 269-BFO
-   **[skos:definition](#skos:definition):** &ldquo;t last instant of t&rsquo; =Def t is a temporal instant & t&rsquo; is a temporal region & all temporal parts of t&rsquo; other than t precede t&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;Last midnight is the last instant of yesterday&rdquo;@en
-   **Domain:** [temporal instant](#obo:BFO_0000203)
-   **Range:** [temporal region](#obo:BFO_0000008)
-   **InverseOf:** [has last instant](#obo:BFO_0000224)


<a id="obo:BFO_0000171"></a>

### &ldquo;located in&rdquo;@en (obo:BFO\_0000171)

-   **[dc11:identifier](#dc11:identifier):** 234-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b located in c =Def b is an independent continuant & c is an independent & neither is a spatial region & there is some time t such that the spatial region which b occupies at t is continuant part of the spatial region which c occupies at t&rdquo;@en
-   **[skos:scopeNote](#skos:scopeNote):** &ldquo;Users that require more sophisticated representations of time are encouraged to import a temporal extension of BFO-Core provided by the BFO development team. See documentation for guidance: <https://github.com/BFO-ontology/BFO-2020/tree/master/src/owl/profiles/temporal%20extensions>&rdquo;@en
-   **Domain:** [independent continuant](#obo:BFO_0000004)
    and (not ([spatial region](#obo:BFO_0000006)))
-   **Range:** [independent continuant](#obo:BFO_0000004)
    and (not ([spatial region](#obo:BFO_0000006)))
-   **InverseOf:** [location of](#obo:BFO_0000124)


<a id="obo:BFO_0000124"></a>

### &ldquo;location of&rdquo;@en (obo:BFO\_0000124)

-   **[dc11:identifier](#dc11:identifier):** 236-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b location of c =Def c located in b&rdquo;@en
-   **[skos:scopeNote](#skos:scopeNote):** &ldquo;Users that require more sophisticated representations of time are encouraged to import a temporal extension of BFO-Core provided by the BFO development team. See documentation for guidance: <https://github.com/BFO-ontology/BFO-2020/tree/master/src/owl/profiles/temporal%20extensions>&rdquo;@en
-   **Domain:** [independent continuant](#obo:BFO_0000004)
    and (not ([spatial region](#obo:BFO_0000006)))
-   **Range:** [independent continuant](#obo:BFO_0000004)
    and (not ([spatial region](#obo:BFO_0000006)))
-   **InverseOf:** [located in](#obo:BFO_0000171)


<a id="obo:BFO_0000127"></a>

### &ldquo;material basis of&rdquo;@en (obo:BFO\_0000127)

-   **[dc11:identifier](#dc11:identifier):** 244-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b material basis of c =Def c has material basis b&rdquo;@en
-   **[skos:scopeNote](#skos:scopeNote):** &ldquo;Users that require more sophisticated representations of time are encouraged to import a temporal extension of BFO-Core provided by the BFO development team. See documentation for guidance: <https://github.com/BFO-ontology/BFO-2020/tree/master/src/owl/profiles/temporal%20extensions>&rdquo;@en
-   **Domain:** [material entity](#obo:BFO_0000040)
-   **Range:** [disposition](#obo:BFO_0000016)
-   **InverseOf:** [has material basis](#obo:BFO_0000218)


<a id="obo:BFO_0000210"></a>

### &ldquo;occupies spatial region&rdquo;@en (obo:BFO\_0000210)

-   **[dc11:identifier](#dc11:identifier):** 232-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b occupies spatial region r =Def b is an independent continuant that is not a spatial region & r is a spatial region & there is some time t such that every continuant part of b occupies some continuant part of r at t and no continuant part of b occupies any spatial region that is not a continuant part of r at t&rdquo;@en
-   **[skos:scopeNote](#skos:scopeNote):** &ldquo;Users that require more sophisticated representations of time are encouraged to import a temporal extension of BFO-Core provided by the BFO development team. See documentation for guidance: <https://github.com/BFO-ontology/BFO-2020/tree/master/src/owl/profiles/temporal%20extensions>&rdquo;@en
-   **Domain:** [independent continuant](#obo:BFO_0000004)
    and (not ([spatial region](#obo:BFO_0000006)))
-   **Range:** [spatial region](#obo:BFO_0000006)


<a id="obo:BFO_0000200"></a>

### &ldquo;occupies spatiotemporal region&rdquo;@en (obo:BFO\_0000200)

-   **[dc11:identifier](#dc11:identifier):** 082-BFO
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) occupies spatiotemporal region is a relation between a process or process boundary p and the spatiotemporal region s which is its spatiotemporal extent&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;A particle emitted by a nuclear reactor occupies the spatiotemporal region which is its trajectory&rdquo;@en
-   **Domain:** [process](#obo:BFO_0000015) or [process boundary](#obo:BFO_0000035)
-   **Range:** [spatiotemporal region](#obo:BFO_0000011)
-   **Characteristics:** Functional


<a id="obo:BFO_0000199"></a>

### &ldquo;occupies temporal region&rdquo;@en (obo:BFO\_0000199)

-   **[dc11:identifier](#dc11:identifier):** 132-BFO
-   **[skos:definition](#skos:definition):** &ldquo;p occupies temporal region t =Def p is a process or process boundary & the spatiotemporal region occupied by p temporally projects onto t&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;The Second World War occupies the temporal region September 1, 1939 - September 2, 1945&rdquo;@en
-   **Domain:** [process](#obo:BFO_0000015) or [process boundary](#obo:BFO_0000035)
-   **Range:** [temporal region](#obo:BFO_0000008)
-   **Characteristics:** Functional


<a id="obo:BFO_0000132"></a>

### &ldquo;occurrent part of&rdquo;@en (obo:BFO\_0000132)

-   **[dc11:identifier](#dc11:identifier):** 003-BFO
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) occurrent part of is a relation between occurrents b and c when b is part of c&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;Mary&rsquo;s 5th birthday is an occurrent part of Mary&rsquo;s life; the first set of the tennis match is an occurrent part of the tennis match&rdquo;@en
-   **Domain:** [occurrent](#obo:BFO_0000003)
-   **Range:** [occurrent](#obo:BFO_0000003)
-   **InverseOf:** [has occurrent part](#obo:BFO_0000117)
-   **Characteristics:** Transitive


<a id="obo:BFO_0000139"></a>

#### &ldquo;temporal part of&rdquo;@en (obo:BFO\_0000139)

-   **[dc11:identifier](#dc11:identifier):** 078-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b temporal part of c =Def b occurrent part of c & (b and c are temporal regions) or (b and c are spatiotemporal regions & b temporally projects onto an occurrent part of the temporal region that c temporally projects onto) or (b and c are processes or process boundaries & b occupies a temporal region that is an occurrent part of the temporal region that c occupies)&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;Your heart beating from 4pm to 5pm today is a temporal part of the process of your heart beating; the 4th year of your life is a temporal part of your life, as is the process boundary which separates the 3rd and 4th years of your life; the first quarter of a game of football is a temporal part of the whole game&rdquo;@en
-   **SubPropertyOf:** [occurrent part of](#obo:BFO_0000132)
-   **Domain:** [occurrent](#obo:BFO_0000003)
-   **Range:** [occurrent](#obo:BFO_0000003)
-   **InverseOf:** [has temporal part](#obo:BFO_0000121)
-   **Characteristics:** Transitive


<a id="obo:BFO_0000066"></a>

### &ldquo;occurs in&rdquo;@en (obo:BFO\_0000066)

-   **[dc11:identifier](#dc11:identifier):** 143-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b occurs in c =Def b is a process or a process boundary & c is a material entity or site & there exists a spatiotemporal region r & b occupies spatiotemporal region r & for all time t, if b exists at t then c exists at t & there exist spatial regions s and s&rsquo; where b spatially projects onto s at t & c occupies spatial region s&rsquo; at t & s is a continuant part of s&rsquo; at t&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;A process of digestion occurs in the interior of an organism; a process of loading artillery rounds into a tank cannon occurs in the interior of the tank&rdquo;@en
-   **Domain:** [process](#obo:BFO_0000015) or [process boundary](#obo:BFO_0000035)
-   **Range:** [site](#obo:BFO_0000029) or [material entity](#obo:BFO_0000040)
-   **InverseOf:** [environs](#obo:BFO_0000183)


<a id="obo:BFO_0000056"></a>

### &ldquo;participates in&rdquo;@en (obo:BFO\_0000056)

-   **[dc11:identifier](#dc11:identifier):** 250-BFO
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) participates in holds between some b that is either a specifically dependent continuant or generically dependent continuant or independent continuant that is not a spatial region & some process p such that b participates in p some way&rdquo;@en
-   **[skos:scopeNote](#skos:scopeNote):** &ldquo;Users that require more sophisticated representations of time are encouraged to import a temporal extension of BFO-Core provided by the BFO development team. See documentation for guidance: <https://github.com/BFO-ontology/BFO-2020/tree/master/src/owl/profiles/temporal%20extensions>&rdquo;@en
-   **Domain:** [specifically dependent continuant](#obo:BFO_0000020) or [generically dependent continuant](#obo:BFO_0000031) or ([independent continuant](#obo:BFO_0000004)
    and (not ([spatial region](#obo:BFO_0000006))))
-   **Range:** [process](#obo:BFO_0000015)
-   **InverseOf:** [has participant](#obo:BFO_0000057)


<a id="obo:BFO_0000062"></a>

### &ldquo;preceded by&rdquo;@en (obo:BFO\_0000062)

-   **[dc11:identifier](#dc11:identifier):** 213-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b preceded by c =Def b precedes c&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;The temporal region occupied by the second half of the match is preceded by the temporal region occupied by the first half of the match&rdquo;@en
-   **Domain:** [occurrent](#obo:BFO_0000003)
-   **Range:** [occurrent](#obo:BFO_0000003)
-   **InverseOf:** [precedes](#obo:BFO_0000063)
-   **Characteristics:** Transitive


<a id="obo:BFO_0000063"></a>

### &ldquo;precedes&rdquo;@en (obo:BFO\_0000063)

-   **[dc11:identifier](#dc11:identifier):** 270-BFO
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) precedes is a relation between occurrents o, o&rsquo; such that if t is the temporal extent of o & t&rsquo; is the temporal extent of o&rsquo; then either the last instant of o is before the first instant of o&rsquo; or the last instant of o is the first instant of o&rsquo; & neither o nor o&rsquo; are temporal instants&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;The temporal region occupied by Mary&rsquo;s birth precedes the temporal region occupied by Mary&rsquo;s death.&rdquo;@en
-   **[skos:scopeNote](#skos:scopeNote):** Each temporal region is its own temporal extent. The temporal extent of a spatiotemporal region is the temporal region it temporally projects onto. The temporal extent of a process or process boundary that occupies temporal region t is t.
-   **[skos:scopeNote](#skos:scopeNote):** Precedes defines a strict partial order on occurrents.
-   **Domain:** [occurrent](#obo:BFO_0000003)
-   **Range:** [occurrent](#obo:BFO_0000003)
-   **InverseOf:** [preceded by](#obo:BFO_0000062)
-   **Characteristics:** Transitive


<a id="obo:BFO_0000055"></a>

### &ldquo;realizes&rdquo;@en (obo:BFO\_0000055)

-   **[dc11:identifier](#dc11:identifier):** 059-BFO
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) realizes is a relation between a process b and realizable entity c such that c inheres in some d & for all t, if b has participant d then c exists & the type instantiated by b is correlated with the type instantiated by c&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;A balding process realizes a disposition to go bald; a studying process realizes a student role; a process of pumping blood realizes the pumping function of a heart&rdquo;@en
-   **Domain:** [process](#obo:BFO_0000015)
-   **Range:** [realizable entity](#obo:BFO_0000017)
-   **InverseOf:** [has realization](#obo:BFO_0000054)


<a id="obo:BFO_0000216"></a>

### &ldquo;spatially projects onto&rdquo;@en (obo:BFO\_0000216)

-   **[dc11:identifier](#dc11:identifier):** 246-BFO
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) spatially projects onto is a relation between some spatiotemporal region b and spatial region c such that at some time t, c is the spatial extent of b at t&rdquo;@en
-   **[skos:scopeNote](#skos:scopeNote):** &ldquo;Users that require more sophisticated representations of time are encouraged to import a temporal extension of BFO-Core provided by the BFO development team. See documentation for guidance: <https://github.com/BFO-ontology/BFO-2020/tree/master/src/owl/profiles/temporal%20extensions>&rdquo;@en
-   **Domain:** [spatiotemporal region](#obo:BFO_0000011)
-   **Range:** [spatial region](#obo:BFO_0000006)


<a id="obo:BFO_0000194"></a>

### &ldquo;specifically depended on by&rdquo;@en (obo:BFO\_0000194)

-   **[dc11:identifier](#dc11:identifier):** 260-BFO
-   **[skos:altLabel](#skos:altLabel):** &ldquo;s-depended on by&rdquo;@en
-   **[skos:definition](#skos:definition):** &ldquo;b specifically depended on by c =Def c specifically depends on b&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;Coloured object specifically depended on by colour&rdquo;@en
-   **Domain:** [specifically dependent continuant](#obo:BFO_0000020) or ([independent continuant](#obo:BFO_0000004)
    and (not ([spatial region](#obo:BFO_0000006))))
-   **Range:** [specifically dependent continuant](#obo:BFO_0000020)
-   **InverseOf:** [specifically depends on](#obo:BFO_0000195)


<a id="obo:BFO_0000196"></a>

#### &ldquo;bearer of&rdquo;@en (obo:BFO\_0000196)

-   **[dc11:identifier](#dc11:identifier):** 053-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b bearer of c =Def c inheres in b&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;A patch of ink is the bearer of a colour quality; an organism is the bearer of a temperature quality&rdquo;@en
-   **SubPropertyOf:** [specifically depended on by](#obo:BFO_0000194)
-   **Domain:** [independent continuant](#obo:BFO_0000004)
    and (not ([spatial region](#obo:BFO_0000006)))
-   **Range:** [specifically dependent continuant](#obo:BFO_0000020)
-   **InverseOf:** [inheres in](#obo:BFO_0000197)


<a id="obo:BFO_0000195"></a>

### &ldquo;specifically depends on&rdquo;@en (obo:BFO\_0000195)

-   **[dc11:identifier](#dc11:identifier):** 012-BFO
-   **[skos:altLabel](#skos:altLabel):** &ldquo;s-depends on&rdquo;@en
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) specifically depends on is a relation between a specifically dependent continuant b and specifically dependent continuant or independent continuant that is not a spatial region c such that b and c share no parts in common & b is of a nature such that at all times t it cannot exist unless c exists & b is not a boundary of c&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;A shape specifically depends on the shaped object; hue, saturation and brightness of a colour sample specifically depends on each other&rdquo;@en
-   **[skos:scopeNote](#skos:scopeNote):** &ldquo;The analogue of specifically depends on for occurrents is has participant.&rdquo;@en
-   **Domain:** [specifically dependent continuant](#obo:BFO_0000020)
-   **Range:** [specifically dependent continuant](#obo:BFO_0000020) or ([independent continuant](#obo:BFO_0000004)
    and (not ([spatial region](#obo:BFO_0000006))))
-   **InverseOf:** [specifically depended on by](#obo:BFO_0000194)


<a id="obo:BFO_0000197"></a>

#### &ldquo;inheres in&rdquo;@en (obo:BFO\_0000197)

-   **[dc11:identifier](#dc11:identifier):** 051-BFO
-   **[skos:definition](#skos:definition):** &ldquo;b inheres in c =Def b is a specifically dependent continuant & c is an independent continuant that is not a spatial region & b specifically depends on c&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;A shape inheres in a shaped object; a mass inheres in a material entity&rdquo;@en
-   **SubPropertyOf:** [specifically depends on](#obo:BFO_0000195)
-   **Domain:** [specifically dependent continuant](#obo:BFO_0000020)
-   **Range:** [independent continuant](#obo:BFO_0000004)
    and (not ([spatial region](#obo:BFO_0000006)))
-   **InverseOf:** [bearer of](#obo:BFO_0000196)


<a id="obo:BFO_0000153"></a>

### &ldquo;temporally projects onto&rdquo;@en (obo:BFO\_0000153)

-   **[dc11:identifier](#dc11:identifier):** 080-BFO
-   **[skos:definition](#skos:definition):** &ldquo;(Elucidation) temporally projects onto is a relation between a spatiotemporal region s and some temporal region which is the temporal extent of s&rdquo;@en
-   **[skos:example](#skos:example):** &ldquo;The world line of a particle temporally projects onto the temporal region extending from the beginning to the end of the existence of the particle&rdquo;@en
-   **Domain:** [spatiotemporal region](#obo:BFO_0000011)
-   **Range:** [temporal region](#obo:BFO_0000008)
-   **Characteristics:** Functional


<a id="bfo.owl-data-property-hierarchy"></a>

## Data properties


<a id="bfo.owl-annotation-property-hierarchy"></a>

## Annotation properties


<a id="skos:altLabel"></a>

### skos:altLabel


<a id="rdfs:comment"></a>

### rdfs:comment


<a id="dc11:contributor"></a>

### dc11:contributor


<a id="skos:definition"></a>

### skos:definition


<a id="dc:description"></a>

### dc:description


<a id="skos:example"></a>

### skos:example


<a id="dc11:identifier"></a>

### dc11:identifier


<a id="rdfs:label"></a>

### rdfs:label


<a id="dc11:license"></a>

### dc11:license


<a id="dc:license"></a>

### dc:license


<a id="skos:prefLabel"></a>

### skos:prefLabel


<a id="skos:scopeNote"></a>

### skos:scopeNote


<a id="dc:title"></a>

### dc:title


<a id="bfo.owl-individuals"></a>

## Individuals

