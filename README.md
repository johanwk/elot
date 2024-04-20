

# What

This repository contains a template for writing OWL ontologies as
[Org Mode](https://orgmode.org/) documents, with supporting functions and scripts.

Check out the file `pizza.org` for an example ELOT file.

ELOT works on Windows, MacOS, and Linux (tested in WSL).

-   [Prerequisites in brief](#orgd1465bd)
-   [Installation](#org4cbe097)
    -   [Get Emacs](#org7fd3ccc)
    -   [Install ELOT in Emacs](#org7620947)
    -   [Install ELOT auxiliaries](#orga302cfa)
-   [Quick start using ELOT](#org317e051)
    -   [Add an ontology](#org10ec10e)
    -   [Adding classes and relations](#org92a3ae2)
    -   [Adding annotations](#orgf156c78)
    -   [Querying the ontology](#org850b96f)
    -   [Making a diagram](#orgc691d52)


<a id="orgd1465bd"></a>

## Prerequisites in brief

-   Download ELOT using [Git](https://github.com/git-guides/install-git) to easily obtain updates
-   Use a recent version (29.x) of [Emacs](https://www.gnu.org/software/emacs/download.html)
-   For viewing your ontologies, install version 5.6 of [Protégé Desktop](https://protege.stanford.edu/)
-   Install [Java](https://www.java.com/en/download/help/download_options.html) to enable advanced features
    -   Turtle output, ontology metrics, and more: install [ROBOT](http://robot.obolibrary.org/)
    -   Ontology diagrams: install [PlantUML](https://plantuml.com/) and [rdfpuml](https://github.com/VladimirAlexiev/rdf2rml)

If you are new to Emacs, the book [Mastering Emacs](https://www.masteringemacs.org/) is highly
recommended.


<a id="org4cbe097"></a>

## Installation


<a id="org7fd3ccc"></a>

### Get Emacs

ELOT has only been tested on recent versions of Emacs. As of 2024-04,
version 29.3 is the latest. See the [GNU Emacs download page](https://www.gnu.org/software/emacs/download.html).

For Windows users: download Emacs from the [emacs-29 for Windows](https://ftp.gnu.org/gnu/emacs/windows/emacs-29/)
directory. The package named [emacs-29.3-installer.exe](https://ftp.gnu.org/gnu/emacs/windows/emacs-29/emacs-29.3-installer.exe) will work
fine. It&rsquo;s preferable to install into a folder that doesn&rsquo;t contain
spaces.

For Linux users: ELOT has been tested on WSL 2 (Windows Subsystem
for Linux). It&rsquo;s recommended to 

-   upgrade to Ubuntu version 22; see the [guide at askubuntu.com](https://askubuntu.com/questions/1428423/upgrade-ubuntu-in-wsl2-from-20-04-to-22-04).
-   compile Emacs to obtain the latest version: see the [guide at
    hubisan/emacs-wsl](https://github.com/hubisan/emacs-wsl).

For MacOS users: See the GNU Emacs page.


<a id="org7620947"></a>

### Install ELOT in Emacs

ELOT is in active development and will see frequent updates. For easy
access to these updates, you should *clone* the ELOT repository using
Git. 

The following steps will get you started editing OWL ontologies.

1.  Create a directory for local Emacs add-ons in your home folder,
    named `elisp` (on Windows, that will likely mean
    `c:\Users\myname\elisp\`).
2.  Clone ELOT into the `elisp` folder using your Git client.  If using
    a terminal for Git, the following will do it.
    
        cd elisp
        git clone https://github.com/johanwk/elot.git
    
    You should now have a subfolder of `elisp` called `elot`.
3.  Ensure ELOT is loaded when Emacs starts up.
    -   For new Emacs users: find the file `elot-init.el` inside the `elot`
        folder, and copy it to a new file named `.emacs` *in your home
        folder*, then restart Emacs. You should now be looking at a
        basic, working Emacs configuration.
    -   Experienced Emacs users should open `elot-init.el` and look at the
        list of packages that are required. Add
        `~/elisp/elot/elot-package/` to your `load-path`.


<a id="orga302cfa"></a>

### Install ELOT auxiliaries

ELOT relies on external software programs to query your ontologies
and produce diagrams. These need to be downloaded.

Preparatory steps, if needed:

1.  Create a directory named `bin` in your home folder: you will
    download programs to this folder. On Windows, that will mean
    `c:\Users\myname\bin\`.
2.  Ensure the `bin` folder is on your PATH, so the programs can be
    found by ELOT. On Windows, use the Control Panel to edit Local
    Environment Variables and add `c:\Users\myname\bin\` to the list.

Get the tools:

1.  The [ROBOT](http://robot.obolibrary.org/) tool is highly recommended for ELOT. Download [robot.jar](https://github.com/ontodev/robot/releases/download/v1.9.5/robot.jar)
    from the [ROBOT releases](https://github.com/ontodev/robot/releases) page to your `bin` folder.
2.  The [PlantUML](https://plantuml.com/) tool is needed for diagrams.
    Download the latest version from [PlantUML Downloads](https://plantuml.com/download) (tested with [plantuml-1.2024.3.jar](https://github.com/plantuml/plantuml/releases/download/v1.2024.3/plantuml-1.2024.3.jar))
    to your `bin` folder.
    For convenience, rename it as just `plantuml.jar` (on Linux, make a symlink).
3.  The [rdfpuml](https://github.com/VladimirAlexiev/rdf2rml) tool will produce great-looking diagrams for
    ontologies.
    -   On Windows, download [rdfpuml.exe](https://github.com/VladimirAlexiev/rdf2rml/raw/master/bin/rdfpuml.exe) to your `bin` folder.
    -   On Linux or MacOS, clone the repository to your `bin` folder, then
        add `~/bin/rdf2rml/bin/` to your PATH. Install Perl modules as
        listed in the [rdfpuml installation guide](https://github.com/VladimirAlexiev/rdf2rml?tab=readme-ov-file#installation).
        
            cd ~/bin
            git clone https://github.com/VladimirAlexiev/rdf2rml.git


<a id="org317e051"></a>

## Quick start using ELOT


<a id="org10ec10e"></a>

### Add an ontology

After the installation steps, it&rsquo;s wise to restart Emacs!

Open the familiar Pizza ontology from `elisp/elot/pizza.org` for an
example of what an ELOT file will look like.

To create a new ontology, do this in Emacs:

-   Select a directory that you want to work in. Open a new Org Mode
    file, for instance `myontology.org`.
-   Insert the ELOT template for a document header.
    You can do this in two ways:
    -   Press the `F4` key. A menu is displayed at the bottom of the Emacs window.
        The letters in blue represent *key sequences* that will call up templates or execute commands.
    -   Type the same sequence (including `<`) at the beginning of a line and press `TAB`.

![img](./documentation/images/elot-helpdesk1.png)

-   Use `odh` (&ldquo;ontology document header&rdquo;) to call up the document header template.
    Answer the prompts, and a header is produced, like the following.
    
        # -*- eval: (load-library "elot-defaults") -*-
        #+title: My new ontology
        #+subtitle: An OWL ontology
        #+author: John Doe
        #+date: WIP (version of 2024-04-04 12:34)
-   Insert the ELOT template for an ontology skeleton: Below the
    header, press `F4` and then `ods`. Answer the prompts &#x2013; be
    ready to provide namespaces/prefixes for your ontology and the
    resources it will contain. You should see something like this
    screenshot:

![img](./documentation/images/elot-skeleton1.png)

Now create an OWL file from your new document.

-   Press `F4` and then `t`, to &ldquo;tangle&rdquo; the ontology to a file. The
    ontology will be in OWL Manchester Syntax, e.g., with filename
    `myontology.omn`.
-   If you have installed ROBOT, a Turtle file named `myontology.ttl`
    will also be available.
-   Have Protégé ready, and open the ontology file to inspect it.

![img](./documentation/images/protege-skeleton1.png)

-   Export to an HTML document with `F4`, then the `h` key. The document
    will open in your web browser.

![img](./documentation/images/firefox-skeleton1.png)


<a id="org92a3ae2"></a>

### Adding classes and relations

-   Navigate to the `Classes` heading
-   Press `F4` followed by `ocp` to insert headings for defining primitive
    classes, with some appropriate annotation properties added. Hit `ocd`
    for non-primitive classes. The templates reveal the ELOT authors&rsquo;
    preference for the [Industrial Ontology Foundry Annotation Vocabulary (IOF-AV)](https://spec.industrialontologies.org/iof/ontology/core/meta/AnnotationVocabulary/).
-   Navigate to the `Object properties` heading
-   `F4` followed by `op` will insert a relation (object, data, or
    annotation property).
-   The screenshot shows how checkboxes are included for tracking
    progress..

![img](documentation/images/elot-animal1.png)


<a id="orgf156c78"></a>

### Adding annotations

ELOT makes it easy to add annotations to both annotations and axioms: 
just put them in a sublist. 
In this screenshot, two annotations are added to the &ldquo;transitive&rdquo; characteristic axiom:

![img](documentation/images/elot-annotate-axiom1.png)


<a id="org850b96f"></a>

### Querying the ontology

-   Navigate to the `Prefixes` heading and insert a new heading named
    &ldquo;Queries&rdquo;.
-   Hit `F4`, then `obs` to insert a SPARQL *select* code block. Write a query over
    the ontology.
-   Hit `C-c C-c` to run the query.

![img](documentation/images/elot-query1.png)


<a id="orgc691d52"></a>

### Making a diagram

-   Hit `F4`, then `obc` to insert another query, this time SPARQL
    *construct*.
-   Write a query and hit `C-c C-c` to run it.

![img](documentation/images/elot-query2.png)

-   Hit `F4`, then `obd` to insert an *rdfpuml* diagram block.
-   When prompted, provide the name of the Turtle source (the name of
    the construct query) and the figure caption.
-   Place the cursor on the row of the  `#+call:` and hit `C-c C-c`. A
    diagram is generated.

![img](documentation/images/elot-rdfpuml1.png)

-   Hit `F4`, then `h` to view the query results and diagram in your
    browser.

![img](documentation/images/firefox-diagram1.png)

