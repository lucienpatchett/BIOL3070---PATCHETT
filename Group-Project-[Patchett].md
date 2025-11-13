Group Project
================
Lucien Patchett
2025-10-30

\#Discussion Fibromyalgia is a medical condition that presents through
symptoms “including fatigue, sleep disturbances, cognitive dysfunction,
and depressive episodes… chronic fatigue syndrome, irritable bowel
syndrome (IBS), irritable bladder syndrome or interstitial cystitis, and
temporomandibular disorder (TMD)” (Clauw, 2009). Patients with
fibromyalgia often face daily challenges as they struggle to live a
normal life due to the limited visual indicators that can be perceived
by those around them. With this condition occurring in 2-8% of the
global population (Clauw, 2014) it has become crucial that we find ways
to mitigate the effects on patients. There is significant research on
the effects of music as a non-pharmaceutical intervention to reduce pain
levels in patients, however in this paper we will analyze the effects of
non-musical (pink) noise as it compares to music to determine if there
is significance in music as a treatment option for fibromyalgia.

\#Question Can music and/or pink noise be used as an analgesic in
patients with fibromyalgia?

\#Hypothesis Music and pink noise will lead to a decrease in nociception
in patients with a diagnosis of fibromyalgia.

\#Prediction Participants who present with a diagnosis fibromyalgia will
find a greater decrease in nociception when listening to music when
compared to “pink” noise.

\#pic and pim boxplot

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

# Pivot pim1 and pim2 into long format
data_long_pim <- data %>%
  pivot_longer(cols = c(pic1,pic2,pim1,pim2), names_to = "treatment", values_to = "value")

# Make boxplot
ggplot(data_long_pim, aes(x = treatment, y = value, fill = treatment)) +
  geom_boxplot(alpha = 0.7, width = 0.6) +
  theme_classic() +
  scale_fill_manual(values = c("pic1" = "Red", "pic2" = "Green", "pim1" = "Orange", "pim2" = "Black")) +
  ylab("Pain Level") +
  xlab("") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    legend.position = "none"
  )+
    scale_x_discrete(labels = c("pic1" = "Before Noise", "pic2" = "After Music", "pim1" = "Before Music", "pim2" = "After Music"))
```

    ## Warning: Removed 92 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](Group-Project-%5BPatchett%5D_files/figure-gfm/code%20chunk%20pim/pic-1.png)<!-- -->

\#Pic1 and Pic2 code chunk

``` r
library(ggplot2)
library(tidyr)
library(dplyr)

data <- read.csv("dataset.csv")

# Pivot pic1 and pic2 into long format
data_long <- data %>%
  pivot_longer(cols = c(pic1,pic2), names_to = "treatment", values_to = "value")

# Make boxplot
ggplot(data_long, aes(x = treatment, y = value, fill = treatment)) +
  geom_boxplot(alpha = 0.7, width = 0.6) +
  theme_classic() +
  scale_fill_manual(values = c("pic1" = "Red", "pic2" = "Green")) +
  ylab("Pain Level") +
  xlab("") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    legend.position = "none"
  )+
  scale_x_discrete(labels = c("pic1" = "Before", "pic2" = "After"))
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
  pivot_longer(cols = c(pim1,pim2), names_to = "treatment", values_to = "value")

# Make boxplot
ggplot(data_long_pim, aes(x = treatment, y = value, fill = treatment)) +
  geom_boxplot(alpha = 0.7, width = 0.6) +
  theme_classic() +
  scale_fill_manual(values = c("pim1" = "Orange", "pim2" = "Black")) +
  ylab("Pain Level") +
  xlab("") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    legend.position = "none"
  )+
    scale_x_discrete(labels = c("pim1" = "Before", "pim2" = "After"))
```

    ## Warning: Removed 46 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](Group-Project-%5BPatchett%5D_files/figure-gfm/code%20chunk%20pim-1.png)<!-- -->

``` r
library(ggplot2)
library(tidyr)
library(dplyr)

data <- read.csv("dataset.csv")

# Pivot pic2 and pim2 into long format
data_long_pim <- data %>%
  pivot_longer(cols = c(pic2,pim2), names_to = "treatment", values_to = "value")

# Make boxplot
ggplot(data_long_pim, aes(x = treatment, y = value, fill = treatment)) +
  geom_boxplot(alpha = 0.7, width = 0.6) +
  theme_classic() +
  scale_fill_manual(values = c("pic2" = "Green", "pim2" = "Black")) +
  ylab("Pain Level") +
  xlab("") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    legend.position = "none"
  )+
    scale_x_discrete(labels = c("pic2" = "After Noise", "pim2" = "After Music"))
```

    ## Warning: Removed 46 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](Group-Project-%5BPatchett%5D_files/figure-gfm/code%20chunk%20pim%20vs%20pic-1.png)<!-- -->

\#Works Cited OpenAI. (2025). ChatGPT (GPT-5) \[Large language model\].
OpenAI. <https://chat.openai.com/>

Clauw, D. J. (2009). Fibromyalgia: An Overview. The American Journal of
Medicine, 122(12), S3–S13.
<https://doi.org/10.1016/j.amjmed.2009.09.006>

Clauw, D. J. (2014). Fibromyalgia. JAMA, 311(15), 1547.
<https://doi.org/10.1001/jama.2014.3266>
