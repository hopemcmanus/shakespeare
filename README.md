## Description

This repository contains data on 37 plays of William Shakespeare, prepared for tidy text analysis.

## Data

The texts were accessed via [Project Gutenberg](https://www.gutenberg.org/) and processed into CSV format. The processing code is available in `R/cleangutenbergshakespeare.R`. For each work obtained from Project Gutenberg, the Plain Text UTF-8 edition is identified as being in the public domain. The texts reproduced here are based on those editions and therefore retain the same public-domain status.

## Analysis

-   **`tidy-sentiment.R`**\
    **Description:** Performs sentiment analysis on tidy CSV data.\
    **Source:** Adapted from [Chapter 2 Sentiment Analysis with Tidy Data](https://www.tidytextmining.com/sentiment.html) of *Tidy Text Mining with R* by Silge & Robinson (2017)

-   **`tidy_frequency.R`**\
    **Description:** Performs tf-idf word/document frequency analysis.\
    **Source:** Adapted from [Chapter 3 Word and Document Frequency: tf-idf](https://www.tidytextmining.com/tfidf.html) of *Tidy Text Mining with R* by Silge & Robinson (2017)

-   **`tidy_relationships.R`**\
    **Description:** Extracts n-grams and calculates word correlations to explore frequently co-occurring words and relationships between terms.\
    **Source:** Adapted from [Chapter 4 Relationships Between Words: n-grams and Correlations](https://www.tidytextmining.com/ngrams.html) of *Tidy Text Mining with R* by Silge & Robinson (2017)

## Attribution and License

Some scripts and concepts used in this project are adapted from *Tidy Text Mining with R* by Julia Silge & David Robinson (2017).\
This material is licensed under the [Creative Commons Attribution-NonCommercial-ShareAlike 3.0 United States License (CC BY-NC-SA 3.0 US)](https://creativecommons.org/licenses/by-nc-sa/3.0/us/).\
By using or adapting these scripts, proper credit is given to the original authors, and any modifications are shared under the same license.

## Citations

-   **Tidy Text Mining with R**\
    **Description:** A comprehensive introduction to text mining in R using tidy data principles.\
    **Source:** Julia Silge & David Robinson (2017). [GitHub repository](https://github.com/juliasilge/tidy-text-mining). Code and book manuscript under CC BY-NC-SA license.

-   **text2map (R package)**\
    **Description:** R tools for text matrices, embeddings, document-term matrices, and computational text analysis, including metadata (e.g., Shakespeare metadata) and functions for embedding-, frequency-, and network-based analysis.\
    **Source:** [text2map on GitLab](https://gitlab.com/culturalcartography/text2map) by Dustin Stoltz & Marshall A. Taylor (MIT License, 2022–).\
    **DOI / Citation:** Stoltz, D. S., & Taylor, M. A. (2022). *text2map: R Tools for Text Matrices, Embeddings, and Networks*. Available at <https://gitlab.com/culturalcartography/text2map>

## See Also

[github.com/nrennie/shakespeare](https://github.com/nrennie/shakespeare/) contains data on the plays of William Shakespeare from [shakespeare.mit.edu](https://shakespeare.mit.edu/).
