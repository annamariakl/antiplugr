# antiplugr


'antiplugr' is a package for detecting plagiarism via text mining tools. At this time it contains two functions. The first function 'search_for' is for finding special sentences in a document. The second function 'compare' compares a document to another document and searches for similar sentences using the cosine similarity.

## Installation
You can install antiplugr from GitHub with:
```
# install.packages("devtools")
devtools::install_github("annamariakl/antiplugr")
```

## Example using 'search_for':
This function needs the path of the PDF and a sentence as input. We use a modified summary of the fairy tale 'Hansel and Gretel' from Book Reports (https://www.bookreports.info/hansel-and-gretel-summary/) and a similar sentence from the whole fairy tale from Short Story America (http://www.shortstoryamerica.com/pdf_classics/grimm_hanse_and_gretel.pdf). 
```
# specify the path
doc <- system.file('pdf', 'summary_hansel_and_gretel.pdf', package = 'antiplugr')

# specify a sentence
sen <- "When four weeks had passed and Hansel was still thin, impatience overcame her, and she would wait no longer."


sen_sim <- search_for(doc, sen)

sen_sim
# A tibble: 1 x 3
  cos_sim  page sen_num
    <dbl> <int>   <int>
1   0.632     1      19
```
We now know that the 19th sentence on page 1 is similar to the given sentence (cosine similarity = 0.632).

## Example using 'compare':
We now compare the summary and the whole fairy tale of 'Hansel and Gretel'
```
# specify the paths
file1 <- system.file('pdf', 'summary_hansel_and_gretel.pdf', package = 'antiplugr')
> file2 <- system.file('pdf', 'grimm_hanse_and_gretel.pdf', package = 'antiplugr')  

doc_sim <- compare(file1, file2)

doc_sim
# A tibble: 5 x 6
  cos_sim sen_x                                          page_x sen_num_x page_source sen_num_source
    <dbl> <chr>                                           <int>     <int>       <int>          <int>
1   1.00  then a gentl voic call out from inside: nibbl~      1        10           5             14
2   1.00  the children answered: the wind, the wind, th~      1        11           5             15
3   1.00  they continu to eat, without be distracted.         1        12           5             16
4   0.632 when four week had pass and hansel was still ~      1        19           6             17
5   0.756 godless witch burn up miserably.                    2         1           7             15
```
The output contains the similar sentences in both documents, e.g. the 10th sentence on page 1 of the first document is exact the same (cos_sim = 1) as the 14th sentence on page 5 of the second document.

In both functions you can change the parameter 'cos_sim'. As default the output contains sentences which have a cosine similarity greater or equal 0.6/0.5.