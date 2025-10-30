Group Project
================
Lucien Patchett
2025-10-30

\#Question Can music be used an an analgesic in patients with
Fibromyalgia?

\#Hypothesis Participants who present with a diagnosis of Fibromyalgia
will demonstrate a decrease to nociception in response to listening to
music.

\#Visualizations We decided to use a Boxplot in order to best visualize
the range of reported values from patients.

\#Pic1 and Pic2 code chunk

``` r
library(ggplot2)
library(tidyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
data <- read.csv("dataset.csv")

# Pivot pic1 and pic2 into long format
data_long <- data %>%
  pivot_longer(cols = c(pic1, pic2), names_to = "treatment", values_to = "value")

# Make boxplot
ggplot(data_long, aes(x = treatment, y = value, fill = treatment)) +
  geom_boxplot(alpha = 0.7, width = 0.6) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  ylab("Pain Level") +
  xlab("") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    legend.position = "none"
  )
```

    ## Warning: Removed 46 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](Group-Project-%5BPatchett%5D_files/figure-gfm/code%20pic%20chunks-1.png)<!-- -->

\#Pim1 and Pim2 code chunk

``` r
library(ggplot2)
library(tidyr)
library(dplyr)

data <- read.csv("dataset.csv")

# Pivot pim1 and pim2 into long format
data_long_pim <- data %>%
  pivot_longer(cols = c(pim1, pim2), names_to = "treatment", values_to = "value")

# Make boxplot
ggplot(data_long_pim, aes(x = treatment, y = value, fill = treatment)) +
  geom_boxplot(alpha = 0.7, width = 0.6) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  ylab("Pain Level") +
  xlab("") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    legend.position = "none"
  )
```

    ## Warning: Removed 46 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](Group-Project-%5BPatchett%5D_files/figure-gfm/code%20chunk%20pim-1.png)<!-- -->

\#Accompanying Analysis Code Chunk We could use a PCA for further
analysis of our data set as a way to group by age so that we can
visualize if there is any correlation between age and reduction in pain.
From there we would be able to run further testing to see how the
nervous systems aging process might effect nociception.
