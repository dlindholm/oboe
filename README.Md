
oboe (Open Biomedical Ontologies Extraction tools) package {.title .toc-ignore}
==========================================================

#### *Daniel Lindholm* 

#### *9/14/2016*


This package reads ontologies in OBO format and parses terms into a list
in R. It could be used to find where in the tree a specific term is
located, and has functionality to map cross-references (such as ICD9,
ICD10, OMIM, SNOMED, whatever cross-reference is in the database) to an
ontology identifier.

Below is a small demo on how it could be used. For this example,
[Disease Ontology](http://www.disease-ontology.org/) was used, but it
should work with any ontology saved in the OBO format (haven’t tested
yet, though).



### Initial steps

First, load package into R:

``` {.r}
if(!"devtools" %in% installed.packages()) install.packages("devtools")
if(!"oboe" %in% installed.packages()) devtools::install_github("dlindholm/oboe")
library("oboe")
```

    ## Loading required package: parallel

Then read OBO file, in this case from an URL to get the latest disease
ontology database, but this could of course also be a local file:

``` {.r}
doid <- read_obo("http://purl.obolibrary.org/obo/doid.obo")
```

This parses the OBO file into a list:

``` {.r}
head(doid)
```

    ## [[1]]
    ## [[1]]$id
    ## [1] "DOID:0001816"
    ## 
    ## [[1]]$name
    ## [1] "angiosarcoma"
    ## 
    ## [[1]]$alt_id
    ## [1] "DOID:267"  "DOID:4508"
    ## 
    ## [[1]]$def
    ## [1] "A malignant Vascular tumor that results_in rapidly proliferating, extensively infiltrating anaplastic cells derived_from blood vessels and derived_from the lining of irregular blood-filled spaces. [url:http\\://emedicine.medscape.com/article/276512-overview, url:http\\://en.wikipedia.org/wiki/Hemangiosarcoma, url:http\\://www.ncbi.nlm.nih.gov/pubmed/23327728]"
    ## 
    ## [[1]]$subset
    ## [1] "NCIthesaurus"
    ## 
    ## [[1]]$synonym
    ## [1] "hemangiosarcoma EXACT []"
    ## 
    ## [[1]]$xref
    ## [1] "MSH:D006394"                      "NCI:C3088"                       
    ## [3] "NCI:C9275"                        "SNOMEDCT_US_2016_03_01:33176006" 
    ## [5] "SNOMEDCT_US_2016_03_01:39000009"  "SNOMEDCT_US_2016_03_01:403977003"
    ## [7] "UMLS_CUI:C0018923"                "UMLS_CUI:C0854893"               
    ## 
    ## [[1]]$is_a
    ## [1] "DOID:1115 ! sarcoma"
    ## 
    ## 
    ## [[2]]
    ## [[2]]$id
    ## [1] "DOID:0002116"
    ## 
    ## [[2]]$name
    ## [1] "pterygium"
    ## 
    ## [[2]]$synonym
    ## [1] "surfer's eye EXACT []"
    ## 
    ## [[2]]$xref
    ##  [1] "ICD10CM:H11.0"                    "ICD10CM:H11.00"                  
    ##  [3] "ICD10CM:H11.009"                  "ICD9CM:372.4"                    
    ##  [5] "ICD9CM:372.40"                    "MSH:D011625"                     
    ##  [7] "SNOMEDCT_US_2016_03_01:155165000" "SNOMEDCT_US_2016_03_01:193879003"
    ##  [9] "SNOMEDCT_US_2016_03_01:193884009" "SNOMEDCT_US_2016_03_01:77489003" 
    ## [11] "UMLS_CUI:C0033999"               
    ## 
    ## [[2]]$is_a
    ## [1] "DOID:10124 ! corneal disease"
    ## 
    ## [[2]]$created_by
    ## [1] "laronhughes"
    ## 
    ## [[2]]$creation_date
    ## [1] "2010-06-30T02:44:30Z"
    ## 
    ## 
    ## [[3]]
    ## [[3]]$id
    ## [1] "DOID:0014667"
    ## 
    ## [[3]]$name
    ## [1] "disease of metabolism"
    ## 
    ## [[3]]$def
    ## [1] "A disease that involving errors in metabolic processes of building or degradation of molecules. [url:http\\://www.ncbi.nlm.nih.gov/books/NBK22259/]"
    ## 
    ## [[3]]$subset
    ## [1] "NCIthesaurus"
    ## 
    ## [[3]]$synonym
    ## [1] "metabolic disease  EXACT [SNOMEDCT_2005_07_31:75934005]"
    ## 
    ## [[3]]$xref
    ##  [1] "ICD10CM:E88.9"                    "ICD9CM:277.9"                    
    ##  [3] "MSH:D008659"                      "NCI:C3235"                       
    ##  [5] "SNOMEDCT_US_2016_03_01:154733004" "SNOMEDCT_US_2016_03_01:190961002"
    ##  [7] "SNOMEDCT_US_2016_03_01:267456000" "SNOMEDCT_US_2016_03_01:30390004" 
    ##  [9] "SNOMEDCT_US_2016_03_01:75934005"  "UMLS_CUI:C0025517"               
    ## 
    ## [[3]]$is_a
    ## [1] "DOID:4 ! disease"
    ## 
    ## 
    ## [[4]]
    ## [[4]]$id
    ## [1] "DOID:0050001"
    ## 
    ## [[4]]$name
    ## [1] "Actinomadura madurae infectious disease"
    ## 
    ## [[4]]$subset
    ## [1] "gram-positive_bacterial_infectious_disease"
    ## 
    ## [[4]]$is_obsolete
    ## [1] "true"
    ## 
    ## 
    ## [[5]]
    ## [[5]]$id
    ## [1] "DOID:0050002"
    ## 
    ## [[5]]$name
    ## [1] "Actinomadura pelletieri infectious disease"
    ## 
    ## [[5]]$subset
    ## [1] "gram-positive_bacterial_infectious_disease"
    ## 
    ## [[5]]$is_obsolete
    ## [1] "true"
    ## 
    ## 
    ## [[6]]
    ## [[6]]$id
    ## [1] "DOID:0050003"
    ## 
    ## [[6]]$name
    ## [1] "Streptomyces somaliensis infectious disease"
    ## 
    ## [[6]]$subset
    ## [1] "gram-positive_bacterial_infectious_disease"
    ## 
    ## [[6]]$is_obsolete
    ## [1] "true"

You could then remove the obsolete terms:

``` {.r}
doid <- remove_obsolete(doid)
```

</div>

<div id="finding-terms" class="section level3">

### Finding terms

You could look up terms in different ways. The *find\_entry* is the
function for that. Look for a specific entry id like this:

``` {.r}
doid[find_entry(doid, "id", "DOID:8805")]
```

    ## [[1]]
    ## [[1]]$id
    ## [1] "DOID:8805"
    ## 
    ## [[1]]$name
    ## [1] "intermediate coronary syndrome"
    ## 
    ## [[1]]$synonym
    ##  [1] "(Angina: [crescendo] or [unstable] or [at rest]) or (preinfarction syndrome) or (impending infarction) EXACT [SNOMEDCT_2005_07_31:194814006]"
    ##  [2] "Angina at rest EXACT [SNOMEDCT_2005_07_31:233818002]"                                                                                        
    ##  [3] "Angina at rest EXACT [SNOMEDCT_2005_07_31:194817004]"                                                                                        
    ##  [4] "Angina at rest EXACT [SNOMEDCT_2005_07_31:155313008]"                                                                                        
    ##  [5] "Anginal chest pain at rest EXACT [SNOMEDCT_2005_07_31:59021001]"                                                                             
    ##  [6] "Impending infarction (disorder) EXACT [SNOMEDCT_2005_07_31:25106000]"                                                                        
    ##  [7] "Preinfarction angina EXACT [MTHICD9_2006:411.1]"                                                                                             
    ##  [8] "Preinfarction angina (disorder) EXACT [SNOMEDCT_2005_07_31:64333001]"                                                                        
    ##  [9] "Unstable angina EXACT [SNOMEDCT_2005_07_31:155308009]"                                                                                       
    ## [10] "Unstable angina EXACT [MTH:NOCODE]"                                                                                                          
    ## [11] "Unstable angina EXACT [SNOMEDCT_2005_07_31:194816008]"                                                                                       
    ## [12] "Unstable angina EXACT [SNOMEDCT_2005_07_31:233820004]"                                                                                       
    ## [13] "Worsening angina EXACT [SNOMEDCT_2005_07_31:4557003]"                                                                                        
    ## [14] "Worsening angina EXACT [SNOMEDCT_2005_07_31:155314002]"                                                                                      
    ## [15] "Worsening angina EXACT [SNOMEDCT_2005_07_31:194819001]"                                                                                      
    ## 
    ## [[1]]$xref
    ##  [1] "ICD10CM:I20.0"                    "ICD9CM:411.1"                    
    ##  [3] "MSH:D000789"                      "NCI:C66911"                      
    ##  [5] "SNOMEDCT_US_2016_03_01:155308009" "SNOMEDCT_US_2016_03_01:155313008"
    ##  [7] "SNOMEDCT_US_2016_03_01:155314002" "SNOMEDCT_US_2016_03_01:194814006"
    ##  [9] "SNOMEDCT_US_2016_03_01:194816008" "SNOMEDCT_US_2016_03_01:194817004"
    ## [11] "SNOMEDCT_US_2016_03_01:194819001" "SNOMEDCT_US_2016_03_01:233818002"
    ## [13] "SNOMEDCT_US_2016_03_01:233820004" "SNOMEDCT_US_2016_03_01:25106000" 
    ## [15] "SNOMEDCT_US_2016_03_01:4557003"   "SNOMEDCT_US_2016_03_01:59021001" 
    ## [17] "SNOMEDCT_US_2016_03_01:64333001"  "UMLS_CUI:C0002965"               
    ## 
    ## [[1]]$is_a
    ## [1] "DOID:3393 ! coronary artery disease"

or for a specific name:

``` {.r}
doid[find_entry(doid, "name", "hypertension")]
```

    ## [[1]]
    ## [[1]]$id
    ## [1] "DOID:10763"
    ## 
    ## [[1]]$name
    ## [1] "hypertension"
    ## 
    ## [[1]]$def
    ## [1] "An artery disease characterized by chronic elevated blood pressure in the arteries. [PMID:24352797, url:https\\://en.wikipedia.org/wiki/Hypertension]"
    ## 
    ## [[1]]$synonym
    ## [1] "High blood pressure (& [essential hypertension]) EXACT [SNOMEDCT_2005_07_31:194757006]"
    ## [2] "HTN EXACT [CSP2005:0571-5243]"                                                         
    ## [3] "hyperpiesia EXACT [CSP2005:4003-0017]"                                                 
    ## [4] "hypertensive disease RELATED [MTH:NOCODE]"                                             
    ## [5] "vascular hypertensive disorder EXACT [NCI2004_11_17:C3117]"                            
    ## 
    ## [[1]]$xref
    ##  [1] "EFO:0000537"                      "ICD10CM:I10"                     
    ##  [3] "ICD10CM:I10-I15"                  "ICD9CM:401-405.99"               
    ##  [5] "ICD9CM:997.91"                    "MSH:D006973"                     
    ##  [7] "NCI:C3117"                        "SNOMEDCT_US_2016_03_01:155295004"
    ##  [9] "SNOMEDCT_US_2016_03_01:155302005" "SNOMEDCT_US_2016_03_01:194756002"
    ## [11] "SNOMEDCT_US_2016_03_01:194757006" "SNOMEDCT_US_2016_03_01:194760004"
    ## [13] "SNOMEDCT_US_2016_03_01:194794002" "SNOMEDCT_US_2016_03_01:195537001"
    ## [15] "SNOMEDCT_US_2016_03_01:266287006" "SNOMEDCT_US_2016_03_01:38341003" 
    ## [17] "UMLS_CUI:C0020538"               
    ## 
    ## [[1]]$is_a
    ## [1] "DOID:0050828 ! artery disease"


### Mapping other codes to an ontology ID

If you’d like to map a specific code from another classification, it is
possible to do so if this classification is in the *xref* part of the
database. You could find corresponding ontology ID’s for ICD10 (which is
the function’s default):

``` {.r}
map_to_id(doid, "I50.9")
```

    ## [1] "DOID:6000"

or from other classifications:

``` {.r}
# ICD9:
map_to_id(doid, "997.91", from = "ICD9CM")
```

    ## [1] "DOID:10763"

``` {.r}
# OMIM:
map_to_id(doid, "300069", from = "OMIM")
```

    ## [1] "DOID:12930"

``` {.r}
# UMLS:
map_to_id(doid, "C0155626", from = "UMLS_CUI")
```

    ## [1] "DOID:9408"

``` {.r}
# etc ...
```


### Finding a code’s position in the tree

Too look up where in the hierarchy an entry is, you could do like this:

``` {.r}
get_parents(doid, "DOID:9408", include_entry = TRUE)
```

    ##             id                          text depth
    ## 1       DOID:4                       disease     1
    ## 2       DOID:7  disease of anatomical entity     2
    ## 3    DOID:1287 cardiovascular system disease     3
    ## 4     DOID:178              vascular disease     4
    ## 5 DOID:0050828                artery disease     5
    ## 6    DOID:3393       coronary artery disease     6
    ## 7    DOID:5844         myocardial infarction     7
    ## 8    DOID:9408   acute myocardial infarction     8

Or, if you only want the position, use *get\_depth*:

``` {.r}
get_depth(doid, "DOID:9408")
```

    ## [1] 8



### Next steps

Using these functions, you could convert an ICD10 code into an ontology
ID, and further abstract the diagnosis to the desired level of detail,
and thus group diagnoses.

This is an early version of the package, which will be developed further
to add additional functionality, and to test with other ontologies in
the OBO format. Any input and suggestions, or collaboration via GitHub
is greatly appreciated!
